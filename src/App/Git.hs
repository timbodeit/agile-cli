{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App.Git where

import           App.Util

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Git
import           Data.Git.Revision
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Data.Typeable
import           Shelly                     hiding (find)

type GitCommand = T.Text
type GitOption  = T.Text

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


data BranchStatus = UpToDate | NoUpstream | NewCommits deriving ( Show, Eq )
data WorkingCopyStatus = Clean | Dirty deriving ( Show, Eq )

runGit :: GitM a -> IO (Either GitException a)
runGit = runEitherT . unGitM

branchStatus :: GitM BranchStatus
branchStatus = do
  local     <- git "rev-parse" ["HEAD"]
  mUpstream <- git "rev-parse" ["HEAD@{u}"] >>> tryMaybe
  return $ case mUpstream of
    Nothing                           -> NoUpstream
    Just upstream | upstream == local -> UpToDate
                  | otherwise         -> NewCommits

workingCopyStatus :: GitM WorkingCopyStatus
workingCopyStatus = do
  (_, err, code) <- git' "diff" ["--quiet", "HEAD"]
  case code of
    0 -> return Clean
    1 -> return Dirty
    _ -> throwError . GitException $ cs err

newBranch :: String -> String -> GitM ()
newBranch newbranchName baseBranchName = void $
  git "branch" ["--no-track", cs newbranchName, cs baseBranchName]

checkoutBranch :: RefName -> GitM ()
checkoutBranch branch =
  checkoutBranch' branch
  `orElse` withTempStash (checkoutBranch' branch)

checkoutRemoteBranch :: RefName -> GitM ()
checkoutRemoteBranch branch =
  checkoutRemoteBranch' branch
  `orElse` withTempStash (checkoutRemoteBranch' branch)

mergeBranch :: RefName -> RefName -> GitM ()
mergeBranch (RefName source) target = void $ do
  checkoutBranch' target
  git "merge" ["--no-ff", cs source]

checkoutBranch' :: RefName -> GitM ()
checkoutBranch' (RefName branch) = void $
  git "checkout" [cs branch]

checkoutRemoteBranch' :: RefName -> GitM ()
checkoutRemoteBranch' (RefName branch) = void $
  git "checkout" ["-t", cs branch]

resolveBranch :: String -> GitM Ref
resolveBranch branch = withGit $ \git -> do
  rev <- liftIO $ resolveRevision git (Revision branch [])
  rev `orThrow` GitException ("Unknown branch: " ++ branch)

removeBranch :: RefName -> GitM ()
removeBranch (RefName branch) = void $
  git "branch" ["-d", cs branch]

getLocalBranches :: GitM [RefName]
getLocalBranches = branchesFromOutput <$>
                   git "branch" ["--list"]

getLocalMergedBranches :: GitM [RefName]
getLocalMergedBranches = branchesFromOutput <$>
                         git "branch" ["--list", "--merged"]

getRemoteBranches :: GitM [RefName]
getRemoteBranches = branchesFromOutput <$>
                    git "branch" ["--list", "-r"]

getCurrentBranch :: GitM (Maybe RefName)
getCurrentBranch =
  git "rev-parse" ["--abbrev-ref", "HEAD"]
  >$< (trim . cs)
  >$< \case
    "HEAD"     -> Nothing
    branchName -> Just $ RefName branchName

branchesFromOutput :: T.Text -> [RefName]
branchesFromOutput = map (RefName . parse) . lines . cs
  where parse = trim . drop 2

withTempStash :: GitM a -> GitM a
withTempStash m = do
  git "stash" []
  r <- m
  git "stash" ["pop"]
  return r

git :: GitCommand -> [GitOption] -> GitM T.Text
git command options = git' command options >>= \case
  (output, err, code) | code == 0 -> return output
                      | otherwise -> throwError . GitException $ cs err

git' :: GitCommand -> [GitOption] -> GitM (T.Text, T.Text, Int)
git' command options = shelly' $ (,,)
                       <$> run "git" (command : options)
                       <*> lastStderr
                       <*> lastExitCode
  where
    shelly' = liftIO . shelly . silently . errExit False

withGit :: (Git -> GitM a) -> GitM a
withGit f = do
  m <- liftIO . withCurrentRepo $ \git -> runGit (f git)
  either throwError return m
