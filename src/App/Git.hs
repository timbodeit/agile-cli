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
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Git
import           Data.List                  (find, isInfixOf, isSuffixOf)
import           Data.Maybe                 (mapMaybe)
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

newtype BranchName = BranchName String deriving (Eq, Show, IsString)
newtype RemoteName = RemoteName String deriving (Eq, IsString)

instance Show RemoteName where
  show (RemoteName n) = n

data RemoteBranchName = RemoteBranchName { remoteName :: RemoteName
                                         , branchName :: BranchName
                                         } deriving (Eq, Show)

class IsBranchName a where
  parseBranchName    :: String -> Maybe a
  toBranchString :: a -> String

instance IsBranchName BranchName where
  parseBranchName s = do
    exitIf null
    mapM_ (exitIf . elem) forbiddenChars
    exitIf $ isInfixOf ".."
    exitIf $ isSuffixOf ".lock"
    exitIf $ isSuffixOf "/"
    return $ BranchName trimmed
    where
      trimmed = trim s
      exitIf f = guard . not $ f trimmed
      forbiddenChars = asciiControlChars ++ "~^: \\"
      asciiControlChars = map chr [0..31]

  toBranchString (BranchName b) = b

instance IsBranchName RemoteBranchName where
  parseBranchName s = do
    match  <- snd <$> matchRegexPR "^(.+?)/(.+)$" s
    remote <- RemoteName <$> lookup 1 match
    branch <- parseBranchName =<< lookup 2 match
    return $ RemoteBranchName remote branch

  toBranchString (RemoteBranchName (RemoteName remote) branch) =
    remote ++ "/" ++ toBranchString branch

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

branchStatus :: RemoteName -> IssueKey -> GitM BranchStatus
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
      trackingBranch <- MaybeT getCurrentTrackingBranch
      let RemoteBranchName remote' branch' = trackingBranch
      guard $ remote == remote'
      guard $ containsIssueKey (toBranchString branch')
      return trackingBranch

    validRemoteBranch localBranch localRev = do
      remoteBranches <- filter ((== remote) . remoteName) <$> getRemoteBranches
      upToDateRemoteBranches <- filterM pointsToSameAsLocal remoteBranches

      return $ find exactMatch upToDateRemoteBranches
           <|> find issueMatch upToDateRemoteBranches
           <|> find exactMatch remoteBranches
           <|> find issueMatch remoteBranches
      where
        pointsToSameAsLocal branch = do
          rev <- getBranchRev branch
          return $ rev == localRev
        exactMatch = (== localBranch) . branchName
        issueMatch = containsIssueKey . toBranchString . branchName

    containsIssueKey = isInfixOf (show issueKey)

workingCopyStatus :: GitM WorkingCopyStatus
workingCopyStatus = do
  (_, err, code) <- git' "diff" ["--quiet", "HEAD"]
  case code of
    0 -> return Clean
    1 -> return Dirty
    _ -> throwError . GitException $ cs err

newBranch :: IsBranchName b => BranchName -> Maybe b -> GitM ()
newBranch newbranchName baseBranchName = void $
  let baseBranchOpt = case baseBranchName of
        Nothing -> []
        Just n  -> [branchOpt n]
      opts = ["--no-track", branchOpt newbranchName] ++ baseBranchOpt
  in git "branch" opts

checkoutBranch :: BranchName -> GitM ()
checkoutBranch branch =
  checkoutBranch' branch
  `orElse` withTempStash (checkoutBranch' branch)

checkoutRemoteBranch :: RemoteBranchName -> GitM ()
checkoutRemoteBranch branch =
  checkoutRemoteBranch' branch
  `orElse` withTempStash (checkoutRemoteBranch' branch)

mergeBranch :: IsBranchName b => FastForwardOption -> b -> BranchName -> GitM ()
mergeBranch ffOption source target = void $ do
  checkoutBranch' target
  git "merge" [toGitOption ffOption, branchOpt source]

checkoutBranch' :: BranchName -> GitM ()
checkoutBranch' (BranchName branch) = void $
  git "checkout" [cs branch]

checkoutRemoteBranch' :: RemoteBranchName -> GitM ()
checkoutRemoteBranch' branch = void $
  git "checkout" ["-t", cs (toBranchString branch)]

removeBranch :: BranchName -> GitM ()
removeBranch branch = void $
  git "branch" ["-d", branchOpt branch]

getLocalBranches :: GitM [BranchName]
getLocalBranches = branchesFromOutput <$>
                   git "branch" ["--list"]

getLocalMergedBranches :: GitM [BranchName]
getLocalMergedBranches = branchesFromOutput <$>
                         git "branch" ["--list", "--merged"]

getRemoteBranches :: GitM [RemoteBranchName]
getRemoteBranches = branchesFromOutput <$>
                    git "branch" ["--list", "-r"]

getCurrentBranch :: GitM (Maybe BranchName)
getCurrentBranch =
  git "rev-parse" ["--abbrev-ref", "HEAD"]
  >$< (trim . cs)
  >>= \case
    "HEAD" -> return Nothing
    branch -> Just <$> parseBranchName' branch

getCurrentBranch' :: GitM BranchName
getCurrentBranch' = liftMaybe (GitException "Cannot get current branch") =<< getCurrentBranch

getCurrentTrackingBranch :: GitM (Maybe RemoteBranchName)
getCurrentTrackingBranch = tryMaybe $
  git "rev-parse" ["--abbrev-ref", "HEAD@{u}"]
  >$< trim . cs
  >>= parseBranchName'

getBranchRev :: IsBranchName b => b -> GitM String
getBranchRev = getRev . toBranchString

getRev :: String -> GitM String
getRev ref = trim . cs <$> git "rev-parse" [cs ref]

branchesFromOutput :: IsBranchName b => T.Text -> [b]
branchesFromOutput = mapMaybe (parseBranchName <=< takeBranchName) . lines . cs
  where
    takeBranchName = head' . words . trim . drop 2
    head' []    = Nothing
    head' (x:_) = Just x

withTempStash :: GitM a -> GitM a
withTempStash m = do
  git "stash" []
  r <- m
  git "stash" ["pop"]
  return r

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

withGit :: (Git -> GitM a) -> GitM a
withGit f = do
  m <- liftIO $ withCurrentRepo (runGit . f)
  either throwError return m

parseBranchName' :: IsBranchName b => String -> GitM b
parseBranchName' name = liftMaybe ex $ parseBranchName name
  where
    ex = GitException $ "Invalid branch name: " ++ name

branchOpt :: IsBranchName b => b -> GitOption
branchOpt = cs . toBranchString

(</>) :: RemoteName -> BranchName -> RemoteBranchName
(</>) = RemoteBranchName
