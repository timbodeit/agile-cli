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
import           Text.RegexPR

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

runGit :: GitM a -> IO (Either GitException a)
runGit = runEitherT . unGitM

newBranch :: String -> String -> GitM ()
newBranch newbranchName baseBranchName = void $
  git "branch" [cs newbranchName, cs baseBranchName]

checkoutBranch :: RefName -> GitM ()
checkoutBranch branch =
  checkoutBranch' branch `catchError` const (stashAndCheckoutBranch branch)

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

originUrl :: GitM String
originUrl = do
  output <- git "remote" ["show", "-n", "origin"]
  cs output =~~ "URL:\\s*(\\S+)"
    `orThrow` GitException "Unable to parse origin URL"

getBranches :: GitM [RefName]
getBranches = do
  output <- cs <$> git "branch" ["--list"]
  return . map (RefName . trim) $ lines output

getCurrentBranch :: GitM (Maybe RefName)
getCurrentBranch = do
  output <- git "rev-parse" ["--abbrev-ref", "HEAD"]
  return $ case trim (cs output) of
    "HEAD"     -> Nothing
    branchName -> Just $ RefName branchName

git :: GitCommand -> [GitOption] -> GitM T.Text
git command options = do
  result <- shelly' $ do
    output <- run "git" (command : options)
    code   <- lastExitCode
    if code == 0
    then return $ Right output
    else Left <$> lastStderr
  either (throwError . GitException . cs) return result
  where
    shelly' = liftIO . shelly . silently . errExit False

withGit :: (Git -> GitM a) -> GitM a
withGit f = do
  m <- liftIO . withCurrentRepo $ \git -> runGit (f git)
  either throwError return m

-- Match regex with string and return the first group match
(=~~) :: String -> String -> Maybe String
s =~~ regex = matchRegexPR regex s & view (_Just._2.to (lookup 1))
