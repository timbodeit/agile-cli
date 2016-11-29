module App.Backends.Stash (PullRequestBackend(..)) where

import           App.Backends.Types
import           App.Git.Branch          (toBranchString)
import           App.Types
import           App.Util

import           Control.Lens
import           Data.List               (intercalate)

instance PullRequestBackend StashConfig where
  createPullRequestUrl sourceBranch targetBranch stashConfig =
    return $ stashConfig^.stashBaseUrl
          ++ "/projects/" ++ stashConfig^.stashProject
          ++ "/repos/" ++ stashConfig^.stashRepository
          ++ "/pull-requests?create"
          ++ "&sourceBranch=" ++ urlEncode (toBranchString sourceBranch)
          ++ "&targetBranch=" ++ urlEncode (toBranchString targetBranch)
          ++ "&reviewers=" ++ urlEncode (stashConfig^.stashReviewers.to renderArray)
    where
      renderArray = intercalate "|!|"
