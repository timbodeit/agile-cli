{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App.Git where

import           App.Util

import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Git
import           Data.Git.Revision
import           Data.List
import           Data.Maybe
import qualified Data.Set                   as Set
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Shelly                     hiding (find)

type GitCommand = T.Text
type GitOption  = T.Text

newtype GitException = GitException String

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

data BranchStatus = UpToDate | NoUpstream | NewCommits
data WorkingCopyStatus = Clean | Dirty

runGit :: GitM a -> IO (Either GitException a)
runGit = runEitherT . unGitM

branchStatus :: GitM BranchStatus
branchStatus = do
  local    <- git "rev-parse" ["HEAD"]
  upstream <- tryMaybe $ git "rev-parse" ["HEAD@{u}"]
  return $ case upstream of
    Nothing                           -> NoUpstream
    Just upstream | upstream == local -> UpToDate
                  | otherwise         -> NewCommits
  where tryMaybe k = (Just <$> k) `catchError` const (return Nothing)

workingCopyStatus :: GitM WorkingCopyStatus
workingCopyStatus = do
  (output, err, code) <- git' "diff" ["--quiet","HEAD"]
  case code of
    0 -> return Clean
    1 -> return Dirty
    otherwise -> throwError $ GitException $ cs err

newBranch :: String -> String -> GitM ()
newBranch newbranchName baseBranchName = void $
  git "branch" [cs newbranchName, cs baseBranchName]

checkoutBranch :: RefName -> GitM ()
checkoutBranch branch =
  checkoutBranch' branch `catchError` const (stashAndCheckoutBranch branch)

mergeBranch :: RefName -> RefName -> GitM ()
mergeBranch (RefName source) target = void $ do
  checkoutBranch' target
  git "merge" ["--no-ff", cs source]

checkoutBranch' :: RefName -> GitM ()
checkoutBranch' (RefName branch) = void $
  git "checkout" [cs branch]

stashAndCheckoutBranch :: RefName -> GitM ()
stashAndCheckoutBranch branch = void $ do
  git "stash" []
  checkoutBranch' branch
  git "stash" ["pop"]

resolveBranch :: String -> GitM Ref
resolveBranch branch = withGit $ \git -> do
  rev <- liftIO $ resolveRevision git (Revision branch [])
  rev `orThrow` GitException ("Unknown branch: " ++ branch)

getBranches :: GitM [RefName]
getBranches = do
  output <- cs <$> git "branch" ["--list"]
  return . map (RefName . parse) $ lines output
  where parse = trim . drop 2

getCurrentBranch :: GitM (Maybe RefName)
getCurrentBranch = do
  output <- git "rev-parse" ["--abbrev-ref", "HEAD"]
  return $ case trim (cs output) of
    "HEAD"     -> Nothing
    branchName -> Just $ RefName branchName

git :: GitCommand -> [GitOption] -> GitM T.Text
git command options = do
  (output, err, code) <- git' command options
  if code == 0
    then return output
    else throwError $ GitException $ cs err

git' :: GitCommand -> [GitOption] -> GitM (T.Text, T.Text, Int)
git' command options = shelly' $ do
    output <- run "git" (command : options)
    err    <- lastStderr
    code   <- lastExitCode
    return (output, err, code)
  where
    shelly' = liftIO . shelly . silently . errExit False

withGit :: (Git -> GitM a) -> GitM a
withGit f = do
  m <- liftIO . withCurrentRepo $ \git -> runGit (f git)
  either throwError return m
