{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module App.CLI (execCLI) where

import           App.CLI.Options
import           App.CLI.Parsers
import           App.Config
import           App.ConfigBuilder
import           App.Git                    (BranchName (..), BranchStatus (..),
                                             IsBranchName, RemoteBranchName,
                                             WorkingCopyStatus (..),
                                             toBranchString, (</>))
import qualified App.Git                    as Git
import           App.InitialSetup
import           App.Stash
import           App.Types
import           App.Util

import           Control.Applicative        hiding ((<|>))
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Bool
import           Data.Char
import           Data.Either.Combinators
import           Data.List
import qualified Data.Map                   as Map
import           Data.String.Conversions
import           Jira.API                   hiding (getConfig, tryMaybe)
import           Network.HTTP.Types.URI
import           Options.Applicative
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.Read
import           Text.RegexPR

execCLI :: IO ()
execCLI =
  let parser = info (helper <*> optionParser) fullDesc
  in execParser parser >>= runCLI

runCLI :: CLIOptions -> IO ()
runCLI options = case options^.cliCommand of
  InitCommand ->
    doInitSetup
  ConfigTestCommand ->
    configTest
  ShowIssueTypesCommand ->
    showIssueTypes
  CleanupBranchesCommand ->
    cleanupLocalBranches
  ShowCommand issueString ->
    run $ withIssue issueString printIssue
  OpenCommand issueString ->
    run $ withIssueKey issueString $ openInBrowser <=< issueBrowserUrl
  SearchCommand searchOptions jql ->
    run $ searchIssues searchOptions jql
  NewCommand start issueTypeString summary -> run $ do
    issueType <- parseIssueType issueTypeString
    issueKeyString <- createIssue' issueType summary
    issueKey <- parseIssueKey issueKeyString
    openInBrowser =<< issueBrowserUrl issueKey
    liftIO $ print issueKey
    when start $ startIssue issueKey
  StartCommand issueString ->
    run $ withIssueKey issueString startIssue
  StopCommand issueString ->
    run $ withIssueKey issueString (liftJira . stopProgress)
  ResolveCommand issueString ->
    run $ withIssueKey issueString (liftJira . resolveIssue)
  CloseCommand issueString ->
    run $ withIssueKey issueString (liftJira . closeIssue)
  ReopenCommand issueString ->
    run $ withIssueKey issueString (liftJira . reopenIssue)
  CheckoutCommand issueString ->
    run $ withIssueKey (Just issueString) checkoutBranchForIssueKey
  CreatePullRequestCommand issueString ->
    run $ withIssueKey issueString openPullRequest'
  FinishCommand finishType issueString ->
    run $ withIssueKey issueString $
    case finishType of
      FinishWithPullRequest -> finishIssueWithPullRequest
      FinishWithMerge       -> finishIssueWithMerge
  CommitCommand gitOptions -> run $ do
    issueKey <- currentIssueKey
    liftIO
      $ withSystemTempFile "agile-cli.gittemplate"
      $ \tempPath tempHandle -> do
        hPutStr tempHandle $ show issueKey ++ " "
        hClose tempHandle
        rawSystem "git" $ ["commit", "-t", cs tempPath, "-e"] ++ gitOptions

printIssue :: Issue -> AppM ()
printIssue = liftIO . print

searchIssues :: SearchOptions -> String -> AppM ()
searchIssues (SearchOptions allProjects onlyMyIssues inBrowser) search = do
  jiraConfig <- view configJiraConfig <$> getConfig
  jql <- parseSearch search
  let optionConditions = wrapParens . intercalate " AND " $
                         ["project = "  ++ jiraConfig^.jiraProject  | not allProjects]
                      ++ ["assignee = " ++ jiraConfig^.jiraUsername | onlyMyIssues]
  let jql' = intercalate " AND " $
             [optionConditions | optionConditions /= ""]
          ++ [jql | jql /= ""]

  if inBrowser
  then openInBrowser $ jiraConfig^.jiraBaseUrl ++ "/issues/?jql=" ++ urlEncode' jql'
  else do
    issues <- liftJira $ searchIssues' jql'
    liftIO $ mapM_ (putStrLn . showIssue) issues
  where
    showIssue i = i^.iKey ++ ": " ++ i^.iSummary
    urlEncode' = cs . urlEncode True . cs
    wrapParens "" = ""
    wrapParens s  = "(" ++ s ++ ")"

startIssue :: IssueKey -> AppM ()
startIssue issueKey = do
  gitFetch
  branch <- getOrCreateBranch issueKey

  -- Try to fast-forward to current development branch.
  -- This is useful if the branch was already merged.
  config <- getConfig
  remoteBranch <- liftGit $ remoteDevelopBranch config
  attempt . liftGit $ Git.mergeBranch Git.OnlyFastForward
                                      remoteBranch
                                      branch

  liftGit $ Git.checkoutBranch branch
  startProgress' issueKey
  where
    getOrCreateBranch = orElse <$> branchForIssueKey
                               <*> createBranchForIssueKey

finishIssueWithPullRequest :: IssueKey -> AppM ()
finishIssueWithPullRequest issueKey = do
  gitFetch
  remote <- view configRemote <$> getConfig
  (,) <$> liftGit (Git.branchStatus remote issueKey)
      <*> liftGit Git.workingCopyStatus
  >>= \case
    (NoUpstream, _) -> throwError $ UserInputException
      "Your current branch has not been pushed! Cannot create pull request on remote server."
    (NewCommits, Clean) -> confirmFinish  $
      unlines' [ "You have new commits that haven't yet been pushed to the server."
              , "Do you wish to continue?"
              ]
    (NewCommits, Dirty) -> confirmFinish $
      unlines' [ "You have new commits and changes that haven't been comitted yet."
              , "Do you wish to continue?"
              ]
    (_ , Dirty) -> confirmFinish $
      unlines' [ "You have changes that haven't been comitted yet."
              , "Do you wish to continue?"
              ]
    _           -> finishIssueWithPullRequest' issueKey
  where
    confirmFinish message = liftIO (askYesNoWithDefault False message)
                        >>= bool (return ()) (finishIssueWithPullRequest' issueKey)

finishIssueWithPullRequest' :: IssueKey -> AppM ()
finishIssueWithPullRequest' issueKey = do
  liftJira $ resolveIssue issueKey
  openPullRequest' issueKey

finishIssueWithMerge :: IssueKey -> AppM ()
finishIssueWithMerge issueKey = do
  config <- getConfig
  let transition = config^.configJiraConfig.jiraFinishMergeTransition
  liftJira $ makeIssueTransition issueKey (TransitionName transition)

  source <- branchForIssueKey issueKey
  target <- liftGit $ config^.localDevelopBranch
  liftGit $ Git.mergeBranch Git.NonFastForward source target

configTest :: IO ()
configTest = runAppIO $ do
  configParts <- EitherT searchConfigParts

  liftIO $ do
    putStrLn "> Using config files:"
    mapM_ printConfigPath $ sort configParts
    putStrLn ""
    putStrLn "> Putting together config files..."

  EitherT readConfig'

  liftIO $ do
    putStrLn "> Running JIRA test request..."
    run . liftJira $ getRaw' "application-properties"
    putStrLn "Config seems OK."
  where
    runAppIO = either handleAppException (const $ return ()) <=< runEitherT
    printConfigPath = putStrLn . unConfigPath . configPartPath

showIssueTypes :: IO ()
showIssueTypes = run $ do
  liftIO $ putStrLn "Available issue types:"
  mapM_ printIssueType =<< availableIssueTypes
  where
    printIssueType t = liftIO . putStrLn $
      (t^.itName) ++ ": " ++ (t^.itDescription)

cleanupLocalBranches :: IO ()
cleanupLocalBranches = run $ liftGit Git.getLocalMergedBranches
                         >>= filterM isClosed
                         >>= mapM (liftGit . Git.removeBranch)
  where
    isClosed branch = onError (return False) $ do
      issueKey <- extractIssueKey branch
                  `orThrow` GitException "" -- meaningless, as it is catched above
      issue <- liftJira $ getIssue issueKey
      return $ issue^.iStatus == Closed

checkoutBranchForIssueKey :: IssueKey -> AppM ()
checkoutBranchForIssueKey issueKey = checkoutLocalBranch `orElse` fetchRemoteBranch `orElse` createBranch
  where
    checkoutLocalBranch = do
      branch <- branchForIssueKey issueKey
      liftGit $ Git.checkoutBranch branch

    createBranch =
      liftIO (askYesNoWithDefault True "No local/remote branch found for this issue. Create new branch?") >>= \case
        False -> return ()
        True  -> createBranchForIssueKey issueKey >>= liftGit . Git.checkoutBranch

    fetchRemoteBranch = do
      gitFetch
      remoteName     <- view configRemoteName <$> getConfig
      remoteBranches <- filter (matchesRemote remoteName)
                    <$> liftGit Git.getRemoteBranches
      remoteBranch   <- find (matchesIssueKey issueKey) remoteBranches
                        `orThrow` branchNotFoundException
      liftGit $ Git.checkoutRemoteBranch remoteBranch
        where
          matchesRemote remote = isPrefixOf (remote ++ "/") . show . Git.remoteName
          branchNotFoundException = UserInputException $
                                    "Branch for issue not found: " ++ show issueKey

createBranchForIssueKey :: IssueKey -> AppM BranchName
createBranchForIssueKey issueKey = withAsyncGitFetch $ \asyncFetch -> do
  issue <- liftJira $ getIssue issueKey
  let issueTypeName = issue^.iType.itName
  liftIO . putStrLn $ "Summary: " ++ issue^.iSummary
  branchDescription <- liftIO $ toBranchName
                   <$> askWithDefault (issue^.iSummary.to generateName)
                       "Short description for branch?"
  config <- getConfig
  let remote = config^.configRemote
  devBranch <- liftGit $ config^.configDevelopBranch.to Git.parseBranchName'
  let baseBranchName = remote </> devBranch

  let branchPrefix = config^.configBranchPrefixMap.at issueTypeName.non (config^.configDefaultBranchPrefix)
  let branchSuffix = view iKey issue ++ "-" ++ branchDescription
  branchName <- liftGit . Git.parseBranchName' $ branchPrefix ++ branchSuffix

  liftIO $ wait asyncFetch
  liftGit $ Git.newBranch branchName (Just baseBranchName)
  return branchName
  where
    toBranchName = map (\c -> if isSpace c then '-' else c)
    generateName = toBranchName . map toLower . take 30

createIssue' :: IssueTypeIdentifier -> String -> AppM String
createIssue' issueType summary = do
  config <- getConfig
  let project = ProjectKey $ config^.configJiraConfig.jiraProject
  liftJira . createIssue $ IssueCreationData project issueType summary

openPullRequest' :: IssueKey -> AppM ()
openPullRequest' issueKey = do
  config <- getConfig
  source <- branchForIssueKey issueKey
  target <- liftGit $ config^. localDevelopBranch
  openPullRequest source target

issueBrowserUrl :: IssueKey -> AppM String
issueBrowserUrl issue = do
  baseUrl <- view (configJiraConfig.jiraBaseUrl) <$> getConfig
  return $ baseUrl ++ "/browse/" ++ urlId issue

availableIssueTypes :: AppM [IssueType]
availableIssueTypes = do
  (CreateIssueMetadata projectPairs) <- liftJira getCreateIssueMetadata
  projectKey <- view (configJiraConfig.jiraProject) <$> getConfig
  projectPair <- liftMaybe
    (ConfigException $ "Project not found: " ++ projectKey) $
    find (\p -> p^._1.pKey == projectKey) projectPairs
  return $ snd projectPair

withAsyncGitFetch :: (Async () -> AppM b) -> AppM b
withAsyncGitFetch = (=<< liftIO (async (run gitFetch)))

gitFetch :: AppM ()
gitFetch = do
  remote <- view configRemoteName <$> getConfig
  liftGit $ Git.fetch remote

-- CLI parsing

parseIssueType :: String -> AppM IssueTypeIdentifier
parseIssueType typeName = do
  aliasMap <- view (configJiraConfig.jiraIssueTypeAliases) <$> getConfig
  let resolvedName = Map.findWithDefault typeName typeName aliasMap
  return $ IssueTypeName resolvedName

parseSearch :: String -> AppM String
parseSearch s = do
  searchMap <- view (configJiraConfig.jiraSearchAliases) <$> getConfig
  return . trim $ Map.findWithDefault s s searchMap

currentIssueKey :: AppM IssueKey
currentIssueKey = do
  branch <- liftGit Git.getCurrentBranch `orThrowM` branchException
  extractIssueKey branch `orThrow` parseException
  where
    branchException = UserInputException "You are not on a branch"
    parseException  = UserInputException "Can't parse issue from current branch"

extractIssueKey :: IsBranchName b => b -> Maybe IssueKey
extractIssueKey branch = do
  groups <- snd <$> matchRegexPR "/(\\w+)-(\\d+)" (toBranchString branch)
  key    <- lookup 1 groups
  n      <- lookup 2 groups >>= readMaybe
  return $ IssueKey key (IssueNumber n)

branchForIssueKey :: IssueKey -> AppM BranchName
branchForIssueKey issueKey = do
  branches <- liftGit Git.getLocalBranches
  find (matchesIssueKey issueKey) branches `orThrow` branchException
  where
    branchException = UserInputException $ "Branch for issue not found: " ++ show issueKey

startProgress' :: IssueKey -> AppM ()
startProgress' issueKey = do
  issue <- liftJira $ getIssue issueKey
  case issue^.iStatus of
    InProgress -> return ()
    _          -> liftJira $ startProgress issueKey

withIssueKey :: Maybe String -> (IssueKey -> AppM a) -> AppM a
withIssueKey Nothing         = (=<< currentIssueKey)
withIssueKey (Just issueKey) = (=<< parseIssueKey issueKey)

withIssue :: Maybe String -> (Issue -> AppM a) -> AppM a
withIssue s f = withIssueKey s (f <=< liftJira . getIssue)

matchesIssueKey :: IsBranchName b => IssueKey -> b -> Bool
matchesIssueKey issueKey branch = show issueKey `isInfixOf` toBranchString branch

-- App Monad

run :: AppM a -> IO ()
run m = runApp' m >>= either handleAppException (const $ return ())

runApp' :: AppM a -> IO (Either AppException a)
runApp' m = runEitherT $ do
  (configPath, config) <- EitherT readConfig'
  EitherT $ runApp configPath config m

liftJira :: JiraM a -> AppM a
liftJira m = do
  config        <- getConfig
  jiraApiConfig <- liftEitherIO $ getJiraApiConfig config
  result <- liftIO $ runJira jiraApiConfig m
  either (throwError . JiraApiException) return result

liftGit :: Git.GitM a -> AppM a
liftGit m = liftEitherIO $ mapLeft convertException <$> Git.runGit m
  where
    convertException (Git.GitException s) = GitException s

-- Branch Config Helpers

configRemote :: Getter Config Git.RemoteName
configRemote = configRemoteName.to Git.RemoteName

localDevelopBranch :: IsBranchName b => Getter Config (Git.GitM b)
localDevelopBranch = configDevelopBranch.to Git.parseBranchName'

remoteDevelopBranch :: Config -> Git.GitM RemoteBranchName
remoteDevelopBranch config = do
  devBranch <- view localDevelopBranch config
  return $ (config^.configRemote) </> devBranch

-- Exception Handling

handleAppException :: AppException -> IO ()
handleAppException exception = do
  case exception of
    IOException e ->
      putStrLn "IOException:" >> putStrLn e
    ConfigException s ->
      putStrLn "Problem with config file:" >> putStrLn s
    AuthException s ->
      putStrLn "Authentication Error:" >> putStrLn s
    GitException s ->
      putStrLn "Git error:" >> putStrLn s
    UserInputException s ->
      putStrLn s
    JiraApiException e -> case e of
      JsonFailure s            -> putStrLn "JIRA API: failed to parse JSON:" >> putStrLn s
      OtherException e         -> putStrLn "Fatal exception in JIRA API:"    >> print e
      GenericFailure           -> putStrLn "Fatal exception in JIRA API"
      BadRequestException info -> do
        putStrLn "Bad request to JIRA API:"
        print info
        -- Show available issue types if issuetype key is the the error map
        when (hasErrorField "issuetype" info) showIssueTypes

  exitWith $ ExitFailure 1
  where
    hasErrorField key (BadRequestInfo _ errorMap) = Map.member key errorMap

