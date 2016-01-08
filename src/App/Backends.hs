{-# LANGUAGE Rank2Types #-}

module App.Backends ( module X
                    , withIssueBackend
                    , withPullRequestBackend
                    ) where

import           App.Backends.Github  as X
import           App.Backends.Jira    as X
import           App.Backends.Stash   as X
import           App.Backends.Types   as X
import           App.Types

import           Control.Monad.Except (catchError, throwError)
import           Control.Lens

withPullRequestBackend :: (forall p. PullRequestBackend p => p -> AppM a) -> AppM a
withPullRequestBackend f = do
  config <- getConfig
  isGithub <- isGithubEnv

  case (isGithub, config^.configGithubConfig, config^.configStashConfig) of
    (_, Nothing, Nothing)         -> throwError $ ConfigException "No pull request backend configured"
    (True, Just githubConfig, _)  -> f githubConfig
    (_, _, Just stashConfig)      -> f stashConfig
    (_, Just githubConfig, _)     -> f githubConfig

withIssueBackend :: (forall i. IssueBackend i => i -> AppM a) -> AppM a
withIssueBackend f = do
  config   <- getConfig
  isGithub <- isGithubEnv

  case (isGithub, config^.configGithubConfig, config^.configJiraConfig) of
    (_, Nothing, Nothing)         -> throwError $ ConfigException "No issue backend configured"
    (True, Just githubConfig, _)  -> f githubConfig
    (_, _, Just jiraConfig)       -> f jiraConfig
    (_, Just githubConfig, _)     -> f githubConfig

isGithubEnv :: AppM Bool
isGithubEnv = testSuccess currentRepositoryRef
  where
    testSuccess m = fmap (const True) m `catchError` \_ -> return False

