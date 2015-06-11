module App.Stash where

import           App.Types
import           App.Util

import           Control.Lens
import qualified Data.ByteString as BS
import           Data.Git
import           Data.List

openPullRequest :: RefName -> RefName -> AppM ()
openPullRequest source target = openInBrowser =<< pullRequestUrl source target

pullRequestUrl :: RefName -> RefName -> AppM String
pullRequestUrl (RefName source) (RefName target) = do
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
