module App.Backends.Types where

import App.Types
import App.Git.Branch

class PullRequestBackend a where
  createPullRequest :: BranchName -> BranchName -> a -> AppM String
