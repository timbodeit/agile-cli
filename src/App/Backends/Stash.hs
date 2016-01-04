module App.Backends.Stash (PullRequestBackend(..)) where

import           App.Backends.Types
import           App.Git.Branch          (BranchName (..))
import           App.Types
import           App.Util

import           Control.Lens
import           Data.List               (intercalate)
import           Data.String.Conversions

instance PullRequestBackend StashConfig where
  createPullRequestUrl (BranchName sourceBranch) (BranchName targetBranch) stashConfig =
    return $ stashConfig^.stashBaseUrl
          ++ "/projects/" ++ stashConfig^.stashProject
          ++ "/repos/" ++ stashConfig^.stashRepository
          ++ "/pull-requests?create"
          ++ "&sourceBranch=" ++ urlEncode sourceBranch
          ++ "&targetBranch=" ++ urlEncode targetBranch
          ++ "&reviewers=" ++ urlEncode (stashConfig^.stashReviewers.to renderArray)
    where
      renderArray = intercalate "|!|"
