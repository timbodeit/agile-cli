{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App.Git where

import           App.Util

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Git
import           Data.Git.Revision
import           Data.List                  (find, isInfixOf)
import           Data.String
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Data.Typeable
import           Jira.API                   (IssueKey)
import           Shelly                     hiding (find)
import           Text.RegexPR

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

data BranchStatus = UpToDate
                  | NoUpstream
                  | NewCommits
                  deriving (Show, Eq)

data WorkingCopyStatus = Clean
                       | Dirty
                       deriving (Show, Eq)

data FastForwardOption = Default
                       | OnlyFastForward
                       | NonFastForward
                       deriving (Show, Eq)

instance ToGitOption FastForwardOption where
  toGitOption Default         = "--ff"
  toGitOption OnlyFastForward = "--ff-only"
  toGitOption NonFastForward  = "--no-ff"

runGit :: GitM a -> IO (Either GitException a)
runGit = runEitherT . unGitM

fetch :: String -> GitM ()
fetch remote = void $ git "fetch" [cs remote]

branchStatus :: String -> IssueKey -> GitM BranchStatus
branchStatus remote issueKey = do
  localBranch  <- getCurrentBranch'
  localRev     <- getRev "HEAD"

  validTrackingBranch <||> validRemoteBranch localBranch localRev >>= \case
    Nothing -> return NoUpstream
    Just remoteBranch ->
      getBranchRev remoteBranch >>> tryMaybe >$< \case
        Nothing -> NoUpstream
        Just remoteRev | remoteRev == localRev -> UpToDate
                       | otherwise             -> NewCommits
  where
    validTrackingBranch = runMaybeT $ do
      trackingBranch  <- MaybeT getCurrentTrackingBranch
      (remote', name) <- hoistMaybe $ splitBranch trackingBranch
      guard $ remote == remote'
      guard $ containsIssueKey name
      return trackingBranch

    validRemoteBranch (RefName branchName) localRev = do
      remoteBranches <- filter (withRemoteName (== remote)) <$> getRemoteBranches
      upToDateRemoteBranches <- filterM pointsToSameAsLocal remoteBranches

      return $ find (withBranchName (== branchName))  upToDateRemoteBranches
           <|> find (withBranchName containsIssueKey) upToDateRemoteBranches
           <|> find (withBranchName (== branchName))  remoteBranches
           <|> find (withBranchName containsIssueKey) remoteBranches
      where
        pointsToSameAsLocal branch = do
          rev <- getBranchRev branch
          return $ rev == localRev
        withRemoteName f = maybe False (f . fst) . splitBranch
        withBranchName f = maybe False (f . snd) . splitBranch

    splitBranch :: RefName -> Maybe (String, String)
    splitBranch (RefName branch) = do
      match <- snd <$> matchRegexPR "^(.+?)/(.+)$" branch
      remotePart <- lookup 1 match
      namePart   <- lookup 2 match
      return (remotePart, namePart)

    containsIssueKey = isInfixOf (show issueKey)
    (<||>)  = liftM2 (<|>)

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

mergeBranch :: FastForwardOption -> RefName -> RefName -> GitM ()
mergeBranch ffOption (RefName source) target = void $ do
  checkoutBranch' target
  git "merge" [toGitOption ffOption, cs source]

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

getCurrentBranch' :: GitM RefName
getCurrentBranch' = liftMaybe (GitException "Cannot get current branch") =<< getCurrentBranch

getCurrentTrackingBranch :: GitM (Maybe RefName)
getCurrentTrackingBranch = tryMaybe $ RefName . trim . cs
                       <$> git "rev-parse" ["--abbrev-ref", "HEAD@{u}"]

getBranchRev :: RefName -> GitM String
getBranchRev (RefName branch) = getRev branch

getRev :: String -> GitM String
getRev ref = trim . cs <$> git "rev-parse" [cs ref]

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
