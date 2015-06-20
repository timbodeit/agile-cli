{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App.CLI (execCLI) where

import           App.CLI.Options
import           App.CLI.Parsers
import           App.Config
import           App.Git                    (BranchStatus (..),
                                             WorkingCopyStatus (..))
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
import           Data.Git
import           Data.List
import qualified Data.Map                   as Map
import           Data.String.Conversions
import           Jira.API                   hiding (getConfig, tryMaybe)
import           Options.Applicative
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
    doInitSetup >> configTest
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
  SearchCommand jql ->
    run $ searchIssues jql
  NewCommand start issueTypeString summary -> run $ do
    issueType <- parseIssueType issueTypeString
    issueKeyString <- createIssue' issueType summary
    issueKey <- parseIssueKey issueKeyString
    openInBrowser =<< issueBrowserUrl issueKey
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

searchIssues :: String -> AppM ()
searchIssues jql = do
  issues <- liftJira $ searchIssues' jql
  liftIO $ mapM_ (putStrLn . showIssue) issues
  where
    showIssue i = i^.iKey ++ ": " ++ i^.iSummary

startIssue :: IssueKey -> AppM ()
startIssue issueKey = do
  branch <- getOrCreateBranch issueKey
  liftGit $ Git.checkoutBranch branch
  startProgress' issueKey
  where
    getOrCreateBranch = orElse <$> branchForIssueKey
                               <*> createBranchForIssueKey

finishIssueWithPullRequest :: IssueKey -> AppM ()
finishIssueWithPullRequest issueKey = (,)
  <$> liftGit Git.branchStatus
  <*> liftGit Git.workingCopyStatus
  >>= \case
    (NoUpstream, _    ) -> throwError $ UserInputException
      "Your current branch has not been pushed! Cannot create pull request on remote server."
    (NewCommits, Clean) -> confirmFinish  $
      unlines [ "You have new commits that haven't yet been pushed to the server."
              , "Do you with to continue"
              , "[y/n] "
              ]
    (NewCommits, Dirty) -> confirmFinish $
      unlines [ "You have new commits and changes that haven't been comitted yet."
              , "Do you with to continue"
              , "[y/n] "
              ]
    (        _ , Dirty) -> confirmFinish $
      unlines [ "You have changes that haven't been comitted yet."
              , "Do you with to continue"
              , "[y/n] "
              ]
    _           -> finishIssueWithPullRequest' issueKey
  where
    confirmFinish message = do
      liftIO (putStr message)
      confirm' >>= bool (return ()) (finishIssueWithPullRequest' issueKey)
    confirm' = liftIO getChar >>= \case
      'y' -> return True
      'n' -> return False
      _ -> confirm'

finishIssueWithPullRequest' :: IssueKey -> AppM ()
finishIssueWithPullRequest' issueKey = do
  liftJira $ resolveIssue issueKey
  openPullRequest' issueKey

finishIssueWithMerge :: IssueKey -> AppM ()
finishIssueWithMerge issueKey = do
    liftJira $ closeIssue issueKey
    source <- branchForIssueKey issueKey
    target <- RefName . view configDevelopBranch <$> getConfig
    liftGit $ Git.mergeBranch source target

configTest :: IO ()
configTest = do
  result <- runApp' . liftJira $ getRaw' "application-properties"
  either handleError (const $ putStrLn "Config seems OK.") result
  where
    handleError e = putStrLn $ "Error while checking config:\n" ++ show e

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
    isClosed (RefName branch) = onError (return False) $ do
      issueKey <- extractIssueKey branch
                  `orThrow` GitException "" -- meaningless, as it is catched above
      issue <- liftJira $ getIssue issueKey
      return $ issue^.iStatus == Closed

checkoutBranchForIssueKey :: IssueKey -> AppM ()
checkoutBranchForIssueKey issueKey = checkoutLocalBranch `orElse` fetchRemoteBranch
  where
    checkoutLocalBranch = do
      branch <- branchForIssueKey issueKey
      liftGit $ Git.checkoutBranch branch
    fetchRemoteBranch = do
      remoteName     <- view configRemoteName <$> getConfig
      remoteBranches <- filter (matchesRemote remoteName)
                    <$> liftGit Git.getRemoteBranches
      remoteBranch   <- find (matchesIssueKey issueKey) remoteBranches
                        `orThrow` branchNotFoundException
      liftGit $ Git.checkoutRemoteBranch remoteBranch
        where
          matchesRemote remoteName (RefName s) = (remoteName ++ "/") `isPrefixOf` s
          branchNotFoundException = UserInputException $
                                    "Branch for issue not found: " ++ show issueKey

createBranchForIssueKey :: IssueKey -> AppM RefName
createBranchForIssueKey issueKey = withAsyncGitFetch $ \asyncFetch -> do
  issue <- liftJira $ getIssue issueKey
  let issueTypeName = issue^.iType.itName
  liftIO . putStrLn $ "Summary: " ++ issue^.iSummary
  branchDescription <- liftIO $ toBranchName
                   <$> askWithDefault (issue^.iSummary.to generateName)
                       "Short description for branch?"
  config <- getConfig
  let baseBranchName = config^.configRemoteName ++ "/" ++ config^.configDevelopBranch
  let branchSuffix = view iKey issue ++ "-" ++ branchDescription
      branchName = branchType issueTypeName ++ "/" ++ branchSuffix

  liftIO $ wait asyncFetch
  liftGit $ Git.newBranch branchName baseBranchName
  return $ RefName branchName
  where
    branchType "Bug" = "bugfix"
    branchType _     = "feature"
    toBranchName = map (\c -> if isSpace c then '-' else c)
    generateName = toBranchName . map toLower . take 30

createIssue' :: IssueTypeIdentifier -> String -> AppM String
createIssue' issueType summary = do
  config <- getConfig
  let project = ProjectKey $ config^.configJiraConfig.jiraProject
  liftJira . createIssue $ IssueCreationData project issueType summary

openPullRequest' :: IssueKey -> AppM ()
openPullRequest' issueKey = do
  source <- branchForIssueKey issueKey
  target <- view configDevelopBranch <$> getConfig
  openPullRequest source (RefName target)

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
withAsyncGitFetch = (=<< liftIO (async (run gitFetch')))

gitFetch' :: AppM ()
gitFetch' = do
  remote <- view configRemoteName <$> getConfig
  liftGit $ Git.fetch remote

-- CLI parsing

parseIssueType :: String -> AppM IssueTypeIdentifier
parseIssueType typeName = do
  aliasMap <- view (configJiraConfig.jiraIssueTypeAliases) <$> getConfig
  let resolvedName = Map.findWithDefault typeName typeName aliasMap
  return $ IssueTypeName resolvedName

currentIssueKey :: AppM IssueKey
currentIssueKey = do
  (RefName branchName) <- liftGit Git.getCurrentBranch `orThrowM` branchException
  extractIssueKey branchName `orThrow` parseException
  where
    branchException = UserInputException "You are not on a branch"
    parseException  = UserInputException "Can't parse issue from current branch"

extractIssueKey :: String -> Maybe IssueKey
extractIssueKey s = do
  groups <- snd <$> matchRegexPR "/(\\w+)-(\\d+)" s
  key    <- lookup 1 groups
  n      <- lookup 2 groups >>= readMaybe
  return $ IssueKey key (IssueNumber n)

branchForIssueKey :: IssueKey -> AppM RefName
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

matchesIssueKey :: IssueKey -> RefName -> Bool
matchesIssueKey issueKey (RefName s) = show issueKey `isInfixOf` s

-- App Monad

run :: AppM a -> IO ()
run m = runApp' m >>= either handleAppException (const $ return ())

runApp' :: AppM a -> IO (Either AppException a)
runApp' m = runEitherT $ do
  (configPath, config) <- hoistEitherIO readConfig'
  hoistEitherIO $ runApp configPath config m

liftJira :: JiraM a -> AppM a
liftJira m = do
  config <- getConfig
  jiraConfig <- liftIO $ getJiraConfig config
  result <- liftIO $ runJira jiraConfig m
  either (throwError . JiraApiException) return result

liftGit :: Git.GitM a -> AppM a
liftGit m = liftEitherIO $ mapLeft convertException <$> Git.runGit m
  where
    convertException (Git.GitException s) = GitException s

handleAppException :: AppException -> IO ()
handleAppException (ConfigException s) =
  putStrLn "Problem with config file:" >> putStrLn s
handleAppException (AuthException s) =
  putStrLn "Authentication Error:" >> putStrLn s
handleAppException (GitException s) =
  putStrLn "Git error:" >> putStrLn s
handleAppException (UserInputException s) =
  putStrLn s
handleAppException (JiraApiException e) = case e of
  JsonFailure s            -> putStrLn "JIRA API: failed to parse JSON:" >> putStrLn s
  OtherException e         -> putStrLn "Fatal exception in JIRA API:"    >> print e
  GenericFailure           -> putStrLn "Fatal exception in JIRA API"
  BadRequestException info -> putStrLn "Bad request to JIRA API:"        >> print info
