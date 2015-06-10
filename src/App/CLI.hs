{-# LANGUAGE OverloadedStrings #-}

module App.CLI (dispatch) where

import           App.Config
import           App.Git
import           App.InitialSetup
import           App.Types
import           App.Util

import           Control.Applicative        hiding ((<|>))
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Either.Combinators
import           Data.Git
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Typeable
import           Jira.API                   hiding (getConfig)
import           System.Directory
import           System.Environment
import           System.Process
import           Text.Parsec
import           Text.Parsec.Char

dispatch :: [String] -> IO ()
dispatch [] = putStrLn "Usage: jira [global options] <command> [command options]"
dispatch ("init" : _)       = doInitSetup >> configTest
dispatch ("test" : _)       = configTest

dispatch ("show" : args)    = handleShowIssue args
dispatch ("open" : args)    = handleOpenIssue args
dispatch ("myopen" : args)  = handleMyOpen
dispatch ("search" : args)  = handleSearch args

dispatch ("new" : args)     = handleCreateIssue args
dispatch ("start" : args)   = handleStartIssue args
dispatch ("stop" : args)    = handleStopProgress args
dispatch ("resolve" : args) = handleResolveIssue args
dispatch ("close" : args)   = handleCloseIssue args
dispatch ("reopen" : args)  = handleReopenIssue args

dispatch ("newnow" : args)  = handleCreateAndStart args
dispatch ("co" : args)      = handleCheckout args

configTest :: IO ()
configTest = do
  result <- runApp' . liftJira $ getRaw' "application-properties"
  either handleError (const $ putStrLn "Config seems OK.") result
  where
    handleError e = putStrLn $ "Error while checking config:\n" ++ show e

handleCreateIssue :: [String] -> IO ()
handleCreateIssue (issueTypeName : summary : _) = run $
  doCreateIssue issueTypeName summary

handleShowIssue :: [String] -> IO ()
handleShowIssue args = run . withIssueIdentifier args $ \issueId -> do
  issue <- liftJira $ getIssue issueId
  liftIO $ print issue

handleOpenIssue :: [String] -> IO ()
handleOpenIssue args = run . withIssueIdentifier args $
  openInBrowser <=< issueBrowserUrl

handleSearch :: [String] -> IO ()
handleSearch (jql : _) = run $ do
  issues <- liftJira $ searchIssues' jql
  liftIO $ putStrLn $ unlines $ map showIssue $ sort issues
  where
    showIssue i = i^.iKey ++ ": " ++ i^.iSummary

handleMyOpen :: IO ()
handleMyOpen = run $ do
  config <- getConfig
  let jql = "status = open and assignee = "
          ++ config^.configUsername
          ++ " and project = "
          ++ config^.configProject
  liftIO $ handleSearch [jql]

handleStartIssue :: [String] -> IO ()
handleStartIssue (issueKey : _) = run $ do
  branch <- doCreateBranchForIssueKey issueKey
  runGit' $ checkoutBranch branch
  liftJira . startProgress =<< parseIssueIdentifier issueKey

handleStopProgress :: [String] -> IO ()
handleStopProgress args = run . withIssueIdentifier args $
  liftJira . stopProgress

handleResolveIssue :: [String] -> IO ()
handleResolveIssue args = run . withIssueIdentifier args $
  liftJira . resolveIssue

handleCloseIssue :: [String] -> IO ()
handleCloseIssue args = run . withIssueIdentifier args $
  liftJira . closeIssue

handleReopenIssue :: [String] -> IO ()
handleReopenIssue args = run . withIssueIdentifier args $
  liftJira . reopenIssue

handleCheckout :: [String] -> IO ()
handleCheckout (issueKey : _) = run $ doCheckoutBranchForIssueKey issueKey

handleCreateAndStart :: [String] -> IO ()
handleCreateAndStart (issueTypeName : summary : _) = run $ do
  issueKey <- doCreateIssue issueTypeName summary
  liftIO $ handleStartIssue [issueKey]

--

doCheckoutBranchForIssueKey :: String -> AppM RefName
doCheckoutBranchForIssueKey issueKey = do
  branch <- branchForIssueKey issueKey
  runGit' $ checkoutBranch branch
  return branch

doCreateBranchForIssueKey :: String -> AppM RefName
doCreateBranchForIssueKey issueKey = do
  issue <- liftJira . getIssue =<< parseIssueIdentifier issueKey
  let issueTypeName = view (iType.itName) issue
  liftIO $ ask "Short description for branch? > "
  branchDescription <- liftIO getLine'
  baseBranchName <- view configDevelopBranch <$> getConfig
  let branchSuffix = view iKey issue ++ "-" ++ branchDescription
      branchName = branchType issueTypeName ++ "/" ++ branchSuffix
  runGit' $ newBranch branchName baseBranchName
  return $ RefName branchName
  where
    branchType "Bug" = "bugfix"
    branchType _     = "feature"

doCreateIssue :: String -> String -> AppM String
doCreateIssue issueTypeName summary = do
  config <- getConfig
  let project   = ProjectKey $ view configProject config
      issueType = parseIssueType issueTypeName
  issueKey <- liftJira . createIssue $ IssueCreationData project issueType summary
  liftIO $ handleOpenIssue [issueKey]
  return issueKey

--

issueBrowserUrl :: IssueIdentifier -> AppM String
issueBrowserUrl issue = do
  baseUrl <- view configBaseUrl <$> getConfig
  case issue of
    IssueKey k -> return $ baseUrl ++ "/browse/" ++ k
    IssueId  i -> return $ baseUrl ++ "/browse/" ++ show i

-- CLI parsing

parseIssueType :: String -> IssueTypeIdentifier
parseIssueType "b" = IssueTypeName "Bug"
parseIssueType "f" = IssueTypeName "New Feature"
parseIssueType "i" = IssueTypeName "Improvement"
parseIssueType "t" = IssueTypeName "Task"
parseIssueType s   = IssueTypeName s

parseIssueIdentifier :: String -> AppM IssueIdentifier
parseIssueIdentifier s = do
  project <- view configProject <$> getConfig
  let defaultPrefix = project ++ "-"
      result        = mapLeft (const parseException) $
                      parse (issueParser defaultPrefix) "" s
  IssueKey <$> liftEither result
  where
    issueParser prefix = wholeIssueParser <|>
                         (prefix ++) <$> numberParser
    numberParser = many1 digit <* eof
    wholeIssueParser = do
      project <- manyTill letter (char '-')
      number  <- numberParser
      eof
      return $ project ++ "-" ++ number
    parseException = UserInputException $
      "Not a valid issue identifier: " ++ s

currentIssueIdentifier :: AppM IssueIdentifier
currentIssueIdentifier = do
  branch <- runGit' getCurrentBranch `orThrowM` branchException
  IssueKey <$> parseIssueKey branch `orThrow` parseException
  where
    parseIssueKey :: RefName -> Maybe String
    parseIssueKey (RefName branch) = branch =~~ "/(\\w+-\\d+)"

    branchException = UserInputException "You are not on a branch"
    parseException  = UserInputException "Can't parse issue from current branch"

withIssueIdentifier :: [String] -> (IssueIdentifier -> AppM a) -> AppM a
withIssueIdentifier [] =             (=<< currentIssueIdentifier)
withIssueIdentifier (issueKey : _) = (=<< parseIssueIdentifier issueKey)

withIssue :: [String] -> (Issue -> AppM a) -> AppM a
withIssue args k = withIssueIdentifier args $ k <=< liftJira . getIssue

branchForIssueKey :: String -> AppM RefName
branchForIssueKey issueKey = do
  branches <- runGit' getBranches
  find containsKey branches `orThrow` branchException
  where
    containsKey (RefName s) = issueKey `isInfixOf` s
    branchException = UserInputException $ "Branch for issue not found: " ++ issueKey

-- App Monad

run :: AppM a -> IO ()
run m = runApp' m >>= either print (const $ return ())

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

runGit' :: GitM a -> AppM a
runGit' m = liftEitherIO $ mapLeft convertException <$> runGit m
  where
    convertException (App.Git.GitException s) = App.Types.GitException s
