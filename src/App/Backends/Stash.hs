module App.Backends.Stash (PullRequestBackend(..)) where

import           App.Backends.Types
import           App.Git.Branch     (BranchName (..))
import           App.Types

import           Control.Lens
import           Data.List          (intercalate)
import           Network.HTTP.Types.URI    (urlEncode)
import           Data.String.Conversions

instance PullRequestBackend StashConfig where
  createPullRequest (BranchName sourceBranch) (BranchName targetBranch) stashConfig =
    return $ stashConfig^.stashBaseUrl
          ++ "/projects/" ++ stashConfig^.stashProject
          ++ "/repos/" ++ stashConfig^.stashRepository
          ++ "/pull-requests?create"
          ++ "&sourceBranch=" ++ urlEncode' sourceBranch
          ++ "&targetBranch=" ++ urlEncode' targetBranch
          ++ "&reviewers=" ++ urlEncode' (stashConfig^.stashReviewers.to renderArray)
    where
      renderArray = intercalate "|!|"
      urlEncode'  = cs . urlEncode True . cs
