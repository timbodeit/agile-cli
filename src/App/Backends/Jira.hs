{-# LANGUAGE TypeFamilies #-}

module App.Backends.Jira (IssueBackend(..), IsIssue(..), IsIssueId()) where

import           App.Backends.Jira.Parsers
import           App.Backends.Types
import           App.CLI.Options
import           App.Config
import           App.Git.Branch
import           App.Types
import           App.Util

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                 (find, intercalate)
import           Data.String.Conversions
import qualified Jira.API                  as Jira
import           Text.Parsec               (parse)
import           Text.Read
import           Text.RegexPR

instance IsIssueId Jira.IssueKey

instance IsIssueType Jira.IssueType where
  issueTypeName = Jira._itName
  issueTypeDescription = Jira._itDescription

instance IsIssue Jira.Issue where
  type IssueId Jira.Issue = Jira.IssueKey
  type IssueType Jira.Issue = Jira.IssueType
  type IssueTypeIdentifier Jira.Issue = Jira.IssueTypeIdentifier

  issueId issue = case parse issueKeyParser "" (issue^.Jira.iKey) of
    Left _  -> error "JIRA fatal error: unable to parse issue key"
    Right i -> i

  issueStatus issue = case issue^.Jira.iStatus of
    Jira.Closed     -> Closed
    Jira.InProgress -> InProgress
    _               -> Open

  issueType issue = issue^.Jira.iType
  summarize = show
  summarizeOneLine issue = (issue^.Jira.iKey) ++ ": " ++ (issue^.Jira.iSummary)
  suggestedBranchName = view Jira.iSummary

instance IssueBackend JiraConfig where
  type Issue JiraConfig = Jira.Issue
  type IssueCreationData JiraConfig = Jira.IssueCreationData

  createIssue creationData = runReaderT $ do
    issueString <- liftJira' $ Jira.createIssue creationData
    ReaderT $ parseIssueId issueString

  getIssueById  = liftJira . Jira.getIssue

  makeIssueTransition issueId transitionName = liftJira .
    Jira.makeIssueTransition issueId $ Jira.TransitionName transitionName

  issueUrl issueId jiraConfig = return $ (jiraConfig^.jiraBaseUrl) ++ "/browse/" ++ show issueId

  searchUrl options s jiraConfig = return $
    let jql     = toJql options s jiraConfig
        baseUrl = jiraConfig^.jiraBaseUrl
    in  baseUrl ++ "/issues/?jql=" ++ urlEncode jql

  parseIssueId = const . parseIssueKey

  extractIssueId branch _ = liftMaybe parseException $ do
    groups <- snd <$> matchRegexPR "/(\\w+)-(\\d+)" (toBranchString branch)
    key    <- lookup 1 groups
    n      <- lookup 2 groups >>= readMaybe
    return $ Jira.IssueKey key (Jira.IssueNumber n)
    where
      parseException = UserInputException $ "Unable to parse issue ID from: "
                                         ++ toBranchString branch

  toIssueTypeIdentifier = const . Jira.IssueTypeName

  makeIssueCreationData issueType summary jiraConfig =
    let project = (jiraConfig^.jiraProject . to Jira.ProjectKey)
    in Jira.IssueCreationData project issueType summary

  getAvailableIssueTypes jiraConfig = do
    (Jira.CreateIssueMetadata projectPairs) <- liftJira Jira.getCreateIssueMetadata jiraConfig
    let projectKey = jiraConfig^.jiraProject
    projectPair <- liftMaybe
      (ConfigException $ "Project not found: " ++ projectKey) $
      find (\p -> p^._1.Jira.pKey == projectKey) projectPairs
    return $ snd projectPair

  searchIssues options s = runReaderT $ do
    jql <- liftPure $ toJql options s
    liftJira' $ Jira.searchIssues' jql

  testBackend = liftJira . void $
    Jira.getRaw' "application-properties"

  startProgress = liftJira . Jira.startProgress
  stopProgress  = liftJira . Jira.stopProgress
  resolve       = liftJira . Jira.resolveIssue
  close         = liftJira . Jira.closeIssue
  reopen        = liftJira . Jira.reopenIssue


-- Search options

toJql :: SearchOptions -> String -> JiraConfig -> String
toJql (SearchOptions allProjects onlyMyIssues inBrowser) jql jiraConfig =
  let optionConditions = wrapParens . intercalate " AND " $
                         ["project = "  ++ jiraConfig^.jiraProject  | not allProjects]
                      ++ ["assignee = " ++ jiraConfig^.jiraUsername | onlyMyIssues]
  in  intercalate " AND " $
        [optionConditions | optionConditions /= ""]
        ++ [jql | jql /= ""]
  where
    wrapParens "" = ""
    wrapParens s  = "(" ++ s ++ ")"

-- Helpers

liftJira :: Jira.JiraM a -> JiraConfig -> AppM a
liftJira m jiraConfig = do
  jiraApiConfig <- liftEitherIO $ getJiraApiConfig jiraConfig
  result <- liftIO $ Jira.runJira jiraApiConfig m
  either (throwError . JiraApiException) return result

liftJira' :: Jira.JiraM a -> ReaderT JiraConfig AppM a
liftJira' = ReaderT . liftJira

liftPure :: (r -> a) -> ReaderT r AppM a
liftPure = ReaderT . fmap return
