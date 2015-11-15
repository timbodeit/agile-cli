{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}

module App.CLI (execCLI) where

import           App.Backends               hiding (issueId)
import           App.CLI.Options
import           App.Config
import           App.ConfigBuilder
import           App.Git                    (BranchName (..), BranchStatus (..),
                                             IsBranchName, RemoteBranchName,
                                             WorkingCopyStatus (..),
                                             toBranchString, (</>))
import qualified App.Git                    as Git
import           App.InitialSetup
import           App.Types
import           App.Util

import           Control.Applicative        hiding ((<|>))
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Bool
import           Data.Char
import           Data.Either.Combinators
import           Data.List
import qualified Data.Map                   as Map
import           Data.String.Conversions
import qualified Jira.API                   as Jira
import           Options.Applicative
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

execCLI :: IO ()
execCLI =
  let parser = info (helper <*> optionParser) fullDesc
  in execParser parser >>= runCLI


issueAction :: IssueBackend b => (b -> AppM a) -> ReaderT b AppM a
issueAction = ReaderT

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
    run $ withIssue issueString $ const . printIssue
  OpenCommand issueString ->
    run $ withIssueId issueString $ \issueId -> openInBrowser <=< issueUrl issueId
  SearchCommand searchOptions s -> run $ withIssueBackend $ \issueBackend -> do
    s' <- resolveSearch s

    if searchOnWebsite searchOptions
    then
      openInBrowser =<< searchUrl searchOptions s' issueBackend
    else do
      issues <- searchIssues searchOptions s' issueBackend
      mapM_ (liftIO . putStrLn . summarizeOneLine) issues
  NewCommand start issueTypeString summary -> run $ withIssueBackend $ \issueBackend -> do
    issueType <- parseIssueType' issueTypeString issueBackend
    let issueCreationData = makeIssueCreationData issueType summary issueBackend
    issueId <- createIssue issueCreationData issueBackend
    openInBrowser =<< issueUrl issueId issueBackend
    liftIO $ print issueId
    when start $ startWorkOnIssue issueId issueBackend
  StartCommand issueString ->
    run $ withIssueId issueString $ \issueId issueBackend ->
      startWorkOnIssue issueId issueBackend
  StopCommand issueString ->
    run $ withIssueId issueString stopProgress
  ResolveCommand issueString ->
    run $ withIssueId issueString resolve
  CloseCommand issueString ->
    run $ withIssueId issueString close
  ReopenCommand issueString ->
    run $ withIssueId issueString reopen
  CheckoutCommand issueString ->
    run $ withIssueId (Just issueString) checkoutBranchForIssueKey
  CreatePullRequestCommand issueString ->
    run $ withIssueId issueString $ const . openPullRequest
  FinishCommand finishType issueString ->
    run $ withIssueId issueString $
    case finishType of
      FinishWithPullRequest -> finishIssueWithPullRequest
      FinishWithMerge       -> finishIssueWithMerge
  CommitCommand gitOptions -> run $ withIssueBackend $ \backend -> do
    issueKey <- currentIssueKey backend
    liftIO
      $ withSystemTempFile "agile-cli.gittemplate"
      $ \tempPath tempHandle -> do
        hPutStr tempHandle $ show issueKey ++ " "
        hClose tempHandle
        rawSystem "git" $ ["commit", "-t", cs tempPath, "-e"] ++ gitOptions

printIssue :: IsIssue i => i -> AppM ()
printIssue = liftIO . putStrLn . summarize

startWorkOnIssue :: IssueBackend i => IssueId (Issue i) -> i -> AppM ()
startWorkOnIssue issueId issueBackend = do
  branch <- createBranchForIssueKey issueId issueBackend
  liftGit $ Git.checkoutBranch branch
  startProgress'
  where
    startProgress' = do
      issue <- getIssueById issueId issueBackend
      when (issueStatus issue /= InProgress) $
        startProgress issueId issueBackend

finishIssueWithPullRequest :: IssueBackend i => IssueId (Issue i) -> i -> AppM ()
finishIssueWithPullRequest issueId issueBackend = do
  gitFetch
  remote <- view configRemote <$> getConfig
  (,) <$> liftGit (Git.branchStatus remote $ show issueId)
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
    _           -> finishIssueWithPullRequest' issueId issueBackend
  where
    confirmFinish message = liftIO (askYesNoWithDefault False message)
                        >>= bool (return ()) (finishIssueWithPullRequest' issueId issueBackend)

finishIssueWithPullRequest' :: IssueBackend i => IssueId (Issue i) -> i -> AppM ()
finishIssueWithPullRequest' issueId issueBackend = do
  resolve issueId issueBackend
  openPullRequest issueId

finishIssueWithMerge :: IssueBackend i => IssueId (Issue i) -> i -> AppM ()
finishIssueWithMerge issueId issueBackend = do
  config <- getConfig
  let transition = config^.configJiraConfig.jiraFinishMergeTransition
  makeIssueTransition issueId transition issueBackend

  source <- branchForIssueKey issueId
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

  liftIO $ putStrLn "> Testing issue backend"
  EitherT $ runApp' $ withIssueBackend testBackend

  liftIO $ putStrLn "Config seems OK."
  where
    runAppIO = either handleAppException (const $ return ()) <=< runEitherT
    printConfigPath = putStrLn . unConfigPath . configPartPath

showIssueTypes :: IO ()
showIssueTypes = run $ withIssueBackend $ \issueBackend -> do
  liftIO $ putStrLn "Available issue types:"
  mapM_ printIssueType =<< getAvailableIssueTypes issueBackend
  where
    printIssueType t = liftIO . putStrLn $
      issueTypeName t ++ ": " ++ issueTypeDescription t

cleanupLocalBranches :: IO ()
cleanupLocalBranches = run $ liftGit Git.getLocalMergedBranches
                         >>= filterM isClosed
                         >>= mapM (liftGit . Git.removeBranch)
  where
    isClosed branch = onError (return False) $ withIssueBackend $ \backend -> do
      issueId <- extractIssueId branch backend
      issue   <- getIssueById issueId backend
      return $ issueStatus issue == Closed

checkoutBranchForIssueKey :: IssueBackend b => IssueId (Issue b) -> b -> AppM ()
checkoutBranchForIssueKey issueId issueBackend = checkoutLocalBranch `orElse` fetchRemoteBranch `orElse` createBranch
  where
    checkoutLocalBranch = do
      branch <- branchForIssueKey issueId
      liftGit $ Git.checkoutBranch branch

    createBranch =
      liftIO (askYesNoWithDefault True "No local/remote branch found for this issue. Create new branch?") >>= \case
        False -> return ()
        True  -> createBranchForIssueKey issueId issueBackend >>= liftGit . Git.checkoutBranch

    fetchRemoteBranch = do
      gitFetch
      remoteName     <- view configRemoteName <$> getConfig
      remoteBranches <- filter (matchesRemote remoteName)
                    <$> liftGit Git.getRemoteBranches
      remoteBranch   <- find (matchesIssueKey issueId) remoteBranches
                        `orThrow` branchNotFoundException
      liftGit $ Git.checkoutRemoteBranch remoteBranch
        where
          matchesRemote remote = isPrefixOf (remote ++ "/") . show . Git.remoteName
          branchNotFoundException = UserInputException $
                                    "Branch for issue not found: " ++ show issueId

createBranchForIssueKey :: IssueBackend b => IssueId (Issue b) -> b -> AppM BranchName
createBranchForIssueKey issueId issueBackend = withAsyncGitFetch $ \asyncFetch -> do
  issue <- getIssueById issueId issueBackend
  let issueTypeName = show $ issueType issue
  liftIO . putStrLn $ summarize issue
  branchDescription <- liftIO $ toBranchName
                  <$> askWithDefault (generateName $ suggestedBranchName issue)
                      "Short description for branch?"
  config <- getConfig
  let remote = config^.configRemote
  devBranch <- liftGit $ config^.configDevelopBranch.to Git.parseBranchName'
  let baseBranchName = remote </> devBranch

  let branchPrefix = config^.configBranchPrefixMap.at issueTypeName.non (config^.configDefaultBranchPrefix)
  let branchSuffix = show issueId ++ "-" ++ branchDescription
  branchName <- liftGit . Git.parseBranchName' $ branchPrefix ++ branchSuffix

  liftIO $ wait asyncFetch
  liftGit $ Git.newBranch branchName (Just baseBranchName)
  return branchName
  where
    toBranchName = map (\c -> if isSpace c then '-' else c)
    generateName = toBranchName . map toLower . take 30

openPullRequest :: IsIssueId i => i -> AppM ()
openPullRequest issueId = do
  config <- getConfig
  source <- branchForIssueKey issueId
  target <- liftGit $ config ^. localDevelopBranch

  url    <- withPullRequestBackend $ createPullRequest source target
  openInBrowser url

withAsyncGitFetch :: (Async () -> AppM b) -> AppM b
withAsyncGitFetch = (=<< liftIO (async (run gitFetch)))

gitFetch :: AppM ()
gitFetch = do
  remote <- view configRemoteName <$> getConfig
  liftGit $ Git.fetch remote

-- CLI parsing

parseIssueType' :: IssueBackend ib => String -> ib -> AppM (IssueTypeIdentifier (Issue ib))
parseIssueType' typeName issueBackend = do
  aliasMap <- view (configJiraConfig.jiraIssueTypeAliases) <$> getConfig
  let resolvedName = Map.findWithDefault typeName typeName aliasMap
  return $ toIssueTypeIdentifier resolvedName issueBackend

resolveSearch :: String -> AppM String
resolveSearch s = do
  searchMap <- view (configJiraConfig.jiraSearchAliases) <$> getConfig
  return . trim $ Map.findWithDefault s s searchMap

currentIssueKey :: IssueBackend i => i -> AppM (IssueId (Issue i))
currentIssueKey backend = do
  branch <- liftGit Git.getCurrentBranch `orThrowM` branchException
  extractIssueId branch backend
  where
    branchException = UserInputException "You are not on a branch"

branchForIssueKey :: IsIssueId i => i -> AppM BranchName
branchForIssueKey issueKey = do
  branches <- liftGit Git.getLocalBranches
  find (matchesIssueKey issueKey) branches `orThrow` branchException
  where
    branchException = UserInputException $ "Branch for issue not found: " ++ show issueKey

-- startProgress' :: IssueKey -> AppM ()
-- startProgress' issueKey = do
--   issue <- liftJira $ getIssue issueKey
--   case issue^.iStatus of
--     InProgress -> return ()
--     _          -> liftJira $ startProgress issueKey

withIssueId :: Maybe String -> (forall i. IssueBackend i => IssueId (Issue i) -> i -> AppM a) -> AppM a
withIssueId Nothing k = withIssueBackend $ \backend -> do
  branch  <- liftGit Git.getCurrentBranch `orThrowM` branchException
  issueId <- extractIssueId branch backend
  k issueId backend
  where
    branchException = UserInputException "You are not on a branch"
withIssueId (Just issueString) k = withIssueBackend $ \backend -> do
  issueId <- parseIssueId issueString backend
  k issueId backend

withIssueId' :: Maybe String -> (forall i. IssueBackend i => IssueId (Issue i) -> ReaderT i AppM a) -> AppM a
withIssueId' issueString r = withIssueId issueString $ \issueId backend -> runReaderT (r issueId) backend

withIssue :: Maybe String -> (forall i. IssueBackend i => Issue i -> i -> AppM a) -> AppM a
withIssue s k = withIssueId s $ \issueId backend -> do
  issue <- getIssueById issueId backend
  k issue backend

matchesIssueKey :: (IsIssueId i, IsBranchName b) => i -> b -> Bool
matchesIssueKey issueKey branch = show issueKey `isInfixOf` toBranchString branch

-- App Monad

run :: AppM a -> IO ()
run m = runApp' m >>= either handleAppException (const $ return ())

runApp' :: AppM a -> IO (Either AppException a)
runApp' m = runEitherT $ do
  (configPath, config) <- EitherT readConfig'
  EitherT $ runApp configPath config m

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
      Jira.JsonFailure s            -> putStrLn "JIRA API: failed to parse JSON:" >> putStrLn s
      Jira.OtherException e         -> putStrLn "Fatal exception in JIRA API:"    >> print e
      Jira.GenericFailure           -> putStrLn "Fatal exception in JIRA API"
      Jira.BadRequestException info -> do
        putStrLn "Bad request to JIRA API:"
        print info
        -- Show available issue types if issuetype key is the the error map
        -- when (hasErrorField "issuetype" info) showIssueTypes

  exitWith $ ExitFailure 1
  where
    hasErrorField key (Jira.BadRequestInfo _ errorMap) = Map.member key errorMap

