{-# LANGUAGE OverloadedStrings #-}

module Main where

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

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] = putStrLn "Usage: jira [global options] <command> [command options]"
dispatch ("init" : _)       = doInitSetup
dispatch ("test" : _)       = configTest
dispatch ("new" : args)     = handleCreateIssue args
dispatch ("show" : args)    = handleShowIssue args
dispatch ("search" : args)  = handleSearch args
dispatch ("myopen" : args)  = handleMyOpen

configTest :: IO ()
configTest = do
  result <- runApp' . liftJira $ getRaw' "application-properties"
  either handleError (const $ putStrLn "Config seems OK.") result
  where
    handleError e = putStrLn $ "Error while checking config:\n" ++ show e

handleCreateIssue :: [String] -> IO ()
handleCreateIssue (projectKey : issueType : summary : _) = run $ do
  issueKey <- liftJira $ createIssue $ IssueCreationData
    (ProjectKey projectKey)
    (IssueTypeName issueType)
    summary
  liftIO $ putStrLn $ "Issue created: " ++ issueKey

handleShowIssue :: [String] -> IO ()
handleShowIssue (issueKey : _) = run $ do
  issue <- liftJira . getIssue =<< toIssueIdentifier issueKey
  liftIO $ print issue

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

-- CLI parsing

toIssueIdentifier :: String -> AppM IssueIdentifier
toIssueIdentifier s = do
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
