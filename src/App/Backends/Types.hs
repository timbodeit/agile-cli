{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module App.Backends.Types where

import           App.Git.Branch
import           App.Types
import           App.CLI.Options

-- Pull Requests
class PullRequestBackend a where
  createPullRequest :: BranchName -> BranchName -> a -> AppM String

-- Issue Management

class (Eq a, Show a) => IsIssueId a where

class (Eq a, Show a) => IsIssueType a where
  issueTypeName        :: a -> String
  issueTypeDescription :: a -> String

data IssueStatus = Open
                 | Closed
                 deriving (Show, Eq)

class (Eq a, IsIssueId (IssueId a), IsIssueType (IssueType a)) => IsIssue a where
  type IssueId a
  type IssueType a
  type IssueTypeIdentifier a

  issueId          :: a -> IssueId a
  issueStatus      :: a -> IssueStatus
  issueType        :: a -> IssueType a
  summarize        :: a -> String
  summarizeOneLine :: a -> String

class IsIssue (Issue backend) => IssueBackend backend where
  type Issue backend
  type IssueCreationData backend

  createIssue :: IssueCreationData backend -> backend -> AppM (IssueId (Issue backend))
  getIssueById :: IssueId (Issue backend) -> backend -> AppM (Issue backend)
  makeIssueTransition :: IssueId (Issue backend) -> String -> backend -> AppM ()

  issueUrl       :: IssueId (Issue backend) -> backend -> AppM String

  parseIssueId :: String -> backend -> AppM (IssueId (Issue backend))
  extractIssueId :: BranchName -> backend -> AppM (IssueId (Issue backend))
  toIssueTypeIdentifier :: String -> backend -> IssueTypeIdentifier (Issue backend)

  makeIssueCreationData :: IssueTypeIdentifier (Issue backend) -> String -> backend -> IssueCreationData backend

  getAvailableIssueTypes :: backend -> AppM [IssueType (Issue backend)]

  searchIssues :: SearchOptions -> String -> backend -> AppM [Issue backend]
  searchUrl    :: SearchOptions -> String -> backend -> AppM String

  startProgress :: IssueId (Issue backend)   -> backend -> AppM ()
  startProgress = flip makeIssueTransition "start"

  stopProgress :: IssueId (Issue backend)   -> backend -> AppM ()
  stopProgress = flip makeIssueTransition "stop"

  resolve :: IssueId (Issue backend)   -> backend -> AppM ()
  resolve = flip makeIssueTransition "resolve"

  close :: IssueId (Issue backend)   -> backend -> AppM ()
  close = flip makeIssueTransition "close"

  reopen :: IssueId (Issue backend)   -> backend -> AppM ()
  reopen = flip makeIssueTransition "reopen"

  testBackend :: backend -> AppM ()
