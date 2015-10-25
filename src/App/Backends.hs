{-# LANGUAGE Rank2Types #-}

module App.Backends ( module App.Backends.Jira
                    , module App.Backends.Stash
                    , module App.Backends.Types
                    , withIssueBackend
                    , withPullRequestBackend
                    ) where

import           App.Backends.Jira
import           App.Backends.Stash
import           App.Backends.Types
import           App.Types

import           Control.Lens
import           Control.Monad.Reader

withPullRequestBackend :: (forall p. PullRequestBackend p => p -> AppM a) -> AppM a
withPullRequestBackend f = f =<< view configStashConfig <$> getConfig

withIssueBackend :: (forall i. IssueBackend i => i -> AppM a) -> AppM a
withIssueBackend f = f =<< view configJiraConfig <$> getConfig
