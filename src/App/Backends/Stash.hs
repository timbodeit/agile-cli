module App.Backends.Stash (PullRequestBackend(..)) where

import           App.Backends.Types
import           App.Git.Branch     (BranchName (..))
import           App.Types

import           Control.Lens
import           Data.List          (intercalate)

instance PullRequestBackend StashConfig where
  createPullRequest (BranchName sourceBranch) (BranchName targetBranch) stashConfig =
    return $ stashConfig^.stashBaseUrl
          ++ "/projects/" ++ stashConfig^.stashProject
          ++ "/repos/" ++ stashConfig^.stashRepository
          ++ "/pull-requests?create"
          ++ "&sourceBranch=" ++ sourceBranch
          ++ "&targetBranch=" ++ targetBranch
          ++ "&reviewers=" ++ stashConfig^.stashReviewers.to renderArray
    where
      renderArray = intercalate "|!|"
