{-# LANGUAGE TypeFamilies #-}

module App.Backends.Jira (IssueBackend(..), IsIssue(..), IsIssueId()) where

import           App.Backends.Jira.Parsers
import           App.Backends.Types
import           App.Config
import           App.Git.Branch
import           App.Types
import           App.Util

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List            (find)
import qualified Jira.API             as Jira
import           Text.Read
import           Text.RegexPR
import           Text.Parsec (parse)

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
    Jira.Closed -> Closed
    _           -> Open

  issueType issue = issue^.Jira.iType
  summarize = show
  summarizeOneLine = Jira._iSummary

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

  startProgress = liftJira . Jira.startProgress
  stopProgress  = liftJira . Jira.stopProgress
  resolve       = liftJira . Jira.resolveIssue
  close         = liftJira . Jira.closeIssue
  reopen        = liftJira . Jira.reopenIssue

-- Helpers

liftJira :: Jira.JiraM a -> JiraConfig -> AppM a
liftJira m jiraConfig = do
  jiraApiConfig <- liftEitherIO $ getJiraApiConfig jiraConfig
  result <- liftIO $ Jira.runJira jiraApiConfig m
  either (throwError . JiraApiException) return result

liftJira' :: Jira.JiraM a -> ReaderT JiraConfig AppM a
liftJira' = ReaderT . liftJira
