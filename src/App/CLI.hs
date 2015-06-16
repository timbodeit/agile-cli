{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App.CLI (execCLI) where

import           App.CLI.Options
import           App.CLI.Parsers
import           App.Config
import qualified App.Git                    as Git
import           App.Git (BranchStatus(..), WorkingCopyStatus(..))
import           App.InitialSetup
import           App.Stash
import           App.Types
import           App.Util

import           Control.Applicative        hiding ((<|>))
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Bool
import           Data.Char
import           Data.Either.Combinators
import           Data.Git
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String.Conversions
import           Data.Typeable
import           Jira.API                   hiding (getConfig)
import           Options.Applicative
import           System.IO
import           System.IO.Temp
import           System.Directory
import           System.Environment
import           System.Process
import           Text.Parsec
import           Text.Parsec.Char
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
  ShowCommand issueString ->
    run $ withIssue issueString showIssue
  OpenCommand issueString ->
    run $ withIssueKey issueString $ openInBrowser <=< issueBrowserUrl
  SearchCommand jql ->
    run $ searchIssues jql
  NewCommand issueTypeString start summary -> run $ do
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
    run $ withIssueKey issueString $ case finishType of
      FinishWithPullRequest -> finishIssueWithPullRequest
      FinishWithMerge -> finishIssueWithMerge
  CommitCommand options -> run $ do
    issueKey <- currentIssueKey
    liftIO
      $ withSystemTempFile "agile-cli.gittemplate"
      $ \tempPath tempHandle -> do
        hPutStr tempHandle $ show issueKey ++ " "
        hClose tempHandle
        rawSystem "git" $ ["commit", "-t", cs tempPath, "-e"] ++ options

showIssue :: Issue -> AppM ()
showIssue = liftIO . print

searchIssues :: String -> AppM ()
searchIssues jql = do
  issues <- liftJira $ searchIssues' jql
  liftIO $ mapM_ (putStrLn . showIssue) issues
  where
    showIssue i = i^.iKey ++ ": " ++ i^.iSummary

startIssue :: IssueKey -> AppM ()
startIssue issueKey = do
  branch <- createBranchForIssueKey issueKey
  liftGit $ Git.checkoutBranch branch
  liftJira $ startProgress issueKey

finishIssueWithPullRequest :: IssueKey -> AppM ()
finishIssueWithPullRequest issueKey = do
  brs <- liftGit Git.branchStatus
  wcs <- liftGit Git.workingCopyStatus
  case (brs, wcs) of
    (NoUpstream, _    ) -> throwError $ GitException "Your current branch has not been pushed! Cannot create pull request on remote server."
    (NewCommits, Clean) -> confirmFinish "You have new commits that haven't yet been pushed to the server.\nDo you with to continue\n[y/n] "
    (NewCommits, Dirty) -> confirmFinish "You have new commits and changes that haven't been comitted yet.\nDo you with to continue\n[y/n] "
    (        _ , Dirty) -> confirmFinish "You have changes that haven't been comitted yet.\nDo you with to continue\n[y/n] "
    otherwise           -> finishIssueWithPullRequest' issueKey
  where
    confirmFinish message = do
      liftIO (putStrLn message)
      confirm' >>= bool (return ()) (finishIssueWithPullRequest' issueKey)
    confirm' = liftIO getChar >>= \case
      'y' -> return True
      'n' -> return False
      otherwise -> confirm'

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

checkoutBranchForIssueKey :: IssueKey -> AppM RefName
checkoutBranchForIssueKey issueKey = do
  branch <- branchForIssueKey issueKey
  liftGit $ Git.checkoutBranch branch
  return branch

createBranchForIssueKey :: IssueKey -> AppM RefName
createBranchForIssueKey issueKey = do
  issue <- liftJira $ getIssue issueKey
  let issueTypeName = issue^.iType.itName
  branchDescription <- liftIO $ toBranchName <$> ask "Short description for branch?"
  config <- getConfig
  let baseBranchName = config^.configRemoteName ++ "/" ++ config^.configDevelopBranch
  let branchSuffix = view iKey issue ++ "-" ++ branchDescription
      branchName = branchType issueTypeName ++ "/" ++ branchSuffix
  liftGit $ Git.newBranch branchName baseBranchName
  return $ RefName branchName
  where
    branchType "Bug" = "bugfix"
    branchType _     = "feature"
    toBranchName = map (\c -> if isSpace c then '-' else c)

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

-- CLI parsing

parseIssueType :: Maybe String -> AppM IssueTypeIdentifier
parseIssueType = maybe defaultType resolveAlias
  where
    defaultType = IssueTypeName .
      view (configJiraConfig.jiraDefaultIssueType) <$> getConfig
    resolveAlias alias = do
      aliasMap <- view (configJiraConfig.jiraIssueTypeAliases) <$> getConfig
      return . IssueTypeName $
        Map.findWithDefault alias alias aliasMap

currentIssueKey :: AppM IssueKey
currentIssueKey = do
  (RefName branchName) <- liftGit Git.getCurrentBranch `orThrowM` branchException
  extractIssueKey branchName `orThrow` parseException
  where
    extractIssueKey :: String -> Maybe IssueKey
    extractIssueKey s = do
      groups <-  snd <$> matchRegexPR "/(\\w+)-(\\d+)" s
      key    <- lookup 1 groups
      n      <- lookup 2 groups >>= readMaybe
      return $ IssueKey key (IssueNumber n)

    branchException = UserInputException "You are not on a branch"
    parseException  = UserInputException "Can't parse issue from current branch"

branchForIssueKey :: IssueKey -> AppM RefName
branchForIssueKey issueKey = do
  branches <- liftGit Git.getBranches
  find containsKey branches `orThrow` branchException
  where
    containsKey (RefName s) = show issueKey `isInfixOf` s
    branchException = UserInputException $ "Branch for issue not found: " ++ show issueKey

withIssueKey :: Maybe String -> (IssueKey -> AppM a) -> AppM a
withIssueKey Nothing         = (=<< currentIssueKey)
withIssueKey (Just issueKey) = (=<< parseIssueKey issueKey)

withIssue :: Maybe String -> (Issue -> AppM a) -> AppM a
withIssue s f = withIssueKey s (f <=< liftJira . getIssue)

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
