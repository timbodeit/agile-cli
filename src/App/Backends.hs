{-# LANGUAGE Rank2Types #-}

module App.Backends ( module App.Backends.Github
                    , module App.Backends.Jira
                    , module App.Backends.Stash
                    , module App.Backends.Types
                    , withIssueBackend
                    , withPullRequestBackend
                    ) where

import           App.Backends.Github
import           App.Backends.Jira
import           App.Backends.Stash
import           App.Backends.Types
import           App.Types

import           Control.Lens
import           Control.Monad.Except (catchError)

withPullRequestBackend :: (forall p. PullRequestBackend p => p -> AppM a) -> AppM a
withPullRequestBackend f = do
  config <- getConfig
  isGithub <- isGithubEnv
  if isGithub
  then f $ config^.configGithubConfig
  else f $ config^.configStashConfig

withIssueBackend :: (forall i. IssueBackend i => i -> AppM a) -> AppM a
withIssueBackend f = do
  config   <- getConfig
  isGithub <- isGithubEnv
  if isGithub
  then f $ config^.configGithubConfig
  else f $ config^.configJiraConfig

isGithubEnv :: AppM Bool
isGithubEnv = testSuccess currentRepositoryRef
  where
    testSuccess m = fmap (const True) m `catchError` \_ -> return False
