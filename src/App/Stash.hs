module App.Stash where

import           App.Git
import           App.Types
import           App.Util

import           Control.Applicative
import           Control.Lens
import           Data.List

openPullRequest :: BranchName -> BranchName -> AppM ()
openPullRequest source target = openInBrowser =<< pullRequestUrl source target

pullRequestUrl :: BranchName -> BranchName -> AppM String
pullRequestUrl (BranchName source) (BranchName target) = do
  stashConfig <- view configStashConfig <$> getConfig
  return $ stashConfig^.stashBaseUrl
        ++ "/projects/" ++ stashConfig^.stashProject
        ++ "/repos/" ++ stashConfig^.stashRepository
        ++ "/pull-requests?create"
        ++ "&sourceBranch=" ++ source
        ++ "&targetBranch=" ++ target
        ++ "&reviewers=" ++ stashConfig^.stashReviewers.to renderArray
  where
    renderArray = intercalate "|!|"
