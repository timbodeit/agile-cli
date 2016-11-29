{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App.Git.Core where

import           App.Types                  hiding (GitException)
import qualified App.Types                  (AppException (..))
import           App.Util

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Either.Combinators
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Data.Typeable
import           Shelly                     hiding (find)

type GitCommand = T.Text
type GitOption  = T.Text

class ToGitOption a where
  toGitOption :: a -> GitOption

newtype GitException = GitException String deriving Typeable

instance Show GitException where
  show (GitException s) = s

instance Exception GitException where

newtype GitM a = GitM { unGitM :: EitherT GitException IO a
                      } deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadError GitException
                                 , MonadIO
                                 )

runGit :: GitM a -> IO (Either GitException a)
runGit = runEitherT . unGitM

liftGit :: GitM a -> AppM a
liftGit m = liftEitherIO $ mapLeft convertException <$> runGit m
  where
    convertException (GitException s) = App.Types.GitException s

git :: GitCommand -> [GitOption] -> GitM T.Text
git command' options = git' command' options >>= \case
  (output, err, code) | code == 0 -> return output
                      | otherwise -> throwError . GitException $ cs err

git' :: GitCommand -> [GitOption] -> GitM (T.Text, T.Text, Int)
git' command' options = shelly' $ (,,)
                       <$> run "git" (command' : options)
                       <*> lastStderr
                       <*> lastExitCode
  where
    shelly' = liftIO . shelly . silently . errExit False

withTempStash :: GitM a -> GitM a
withTempStash m = do
  void $ git "stash" []
  r <- m
  void $ git "stash" ["pop"]
  return r

getRev :: String -> GitM String
getRev ref = trim . cs <$> git "rev-parse" [cs ref]
