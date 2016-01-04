{-# LANGUAGE Rank2Types #-}

module App.Backends ( module X
                    , withIssueBackend
                    , withPullRequestBackend
                    ) where

import           App.Backends.Github  as X
import           App.Backends.Jira    as X
import           App.Backends.Stash   as X
import           App.Backends.Types   as X
import           App.Config
import           App.Types
import           App.Util             (orMap)

import           Control.Monad.Except (catchError)

withPullRequestBackend :: (forall p. PullRequestBackend p => p -> AppM a) -> AppM a
withPullRequestBackend f = do
  config <- getConfig
  isGithub <- isGithubEnv
  if isGithub
  then f =<< takeGithubConfig config
  else do
    stashConfig <- takeStashConfig config `orMap` const ex
    f stashConfig
  where
    ex = ConfigException "No pull request backend configured."

withIssueBackend :: (forall i. IssueBackend i => i -> AppM a) -> AppM a
withIssueBackend f = do
  config   <- getConfig
  isGithub <- isGithubEnv
  if isGithub
  then f =<< takeGithubConfig config
  else do
    jiraConfig <- takeJiraConfig config `orMap` const ex
    f jiraConfig
  where
    ex = ConfigException "No issue backend configured."

isGithubEnv :: AppM Bool
isGithubEnv = testSuccess currentRepositoryRef
  where
    testSuccess m = fmap (const True) m `catchError` \_ -> return False

