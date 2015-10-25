{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module App.Git.Branch where

import           App.Git.Core
import           App.Util

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.List                 (find, isInfixOf, isSuffixOf)
import           Data.Maybe                (mapMaybe)
import           Data.String
import           Data.String.Conversions
import qualified Data.Text                 as T
import           Text.RegexPR

newtype BranchName = BranchName String deriving (Eq, Show, IsString)

newtype RemoteName = RemoteName String deriving (Eq, IsString)

instance Show RemoteName where
  show (RemoteName n) = n

data BranchStatus = UpToDate
                  | NoUpstream
                  | NewCommits
                  deriving (Show, Eq)

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

data RemoteBranchName = RemoteBranchName { remoteName :: RemoteName
                                         , branchName :: BranchName
                                         } deriving (Eq, Show)

instance IsBranchName RemoteBranchName where
  parseBranchName s = do
    match  <- snd <$> matchRegexPR "^(.+?)/(.+)$" s
    remote <- RemoteName <$> lookup 1 match
    branch <- parseBranchName =<< lookup 2 match
    return $ RemoteBranchName remote branch

  toBranchString (RemoteBranchName (RemoteName remote) branch) =
    remote ++ "/" ++ toBranchString branch

data FastForwardOption = Default
                       | OnlyFastForward
                       | NonFastForward
                       deriving (Show, Eq)

instance ToGitOption FastForwardOption where
  toGitOption Default         = "--ff"
  toGitOption OnlyFastForward = "--ff-only"
  toGitOption NonFastForward  = "--no-ff"

getCurrentBranch :: GitM (Maybe BranchName)
getCurrentBranch =
  git "rev-parse" ["--abbrev-ref", "HEAD"]
  >$< (trim . cs)
  >>= \case
    "HEAD" -> return Nothing
    branch -> Just <$> parseBranchName' branch

getCurrentBranch' :: GitM BranchName
getCurrentBranch' = liftMaybe (GitException "Cannot get current branch") =<< getCurrentBranch

getLocalBranches :: GitM [BranchName]
getLocalBranches = branchesFromOutput <$>
                   git "branch" ["--list"]

getLocalMergedBranches :: GitM [BranchName]
getLocalMergedBranches = branchesFromOutput <$>
                         git "branch" ["--list", "--merged"]

getRemoteBranches :: GitM [RemoteBranchName]
getRemoteBranches = branchesFromOutput <$>
                    git "branch" ["--list", "-r"]

getCurrentTrackingBranch :: GitM (Maybe RemoteBranchName)
getCurrentTrackingBranch = tryMaybe $
  git "rev-parse" ["--abbrev-ref", "HEAD@{u}"]
  >$< trim . cs
  >>= parseBranchName'

newBranch :: IsBranchName b => BranchName -> Maybe b -> GitM ()
newBranch newbranchName baseBranchName = void $
  let baseBranchOpt = case baseBranchName of
        Nothing -> []
        Just n  -> [branchOpt n]
      opts = ["--no-track", branchOpt newbranchName] ++ baseBranchOpt
  in git "branch" opts

removeBranch :: BranchName -> GitM ()
removeBranch branch = void $
  git "branch" ["-d", branchOpt branch]

checkoutBranch :: BranchName -> GitM ()
checkoutBranch branch =
  checkoutBranch' branch
  `orElse` withTempStash (checkoutBranch' branch)

checkoutBranch' :: BranchName -> GitM ()
checkoutBranch' (BranchName branch) = void $
  git "checkout" [cs branch]

checkoutRemoteBranch :: RemoteBranchName -> GitM ()
checkoutRemoteBranch branch =
  checkoutRemoteBranch' branch
  `orElse` withTempStash (checkoutRemoteBranch' branch)

checkoutRemoteBranch' :: RemoteBranchName -> GitM ()
checkoutRemoteBranch' branch = void $
  git "checkout" ["-t", cs (toBranchString branch)]

mergeBranch :: IsBranchName b => FastForwardOption -> b -> BranchName -> GitM ()
mergeBranch ffOption source target = void $ do
  checkoutBranch' target
  git "merge" [toGitOption ffOption, branchOpt source]

branchStatus :: RemoteName -> String -> GitM BranchStatus
branchStatus remote issueId = do
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
      guard $ containsIssueId (toBranchString branch')
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
        issueMatch = containsIssueId . toBranchString . branchName

    containsIssueId = isInfixOf issueId

parseBranchName' :: IsBranchName b => String -> GitM b
parseBranchName' name = liftMaybe ex $ parseBranchName name
  where
    ex = GitException $ "Invalid branch name: " ++ name

(</>) :: RemoteName -> BranchName -> RemoteBranchName
(</>) = RemoteBranchName

-- Helpers

getBranchRev :: IsBranchName b => b -> GitM String
getBranchRev = getRev . toBranchString

branchesFromOutput :: IsBranchName b => T.Text -> [b]
branchesFromOutput = mapMaybe (parseBranchName <=< takeBranchName) . lines . cs
  where
    takeBranchName = head' . words . trim . drop 2
    head' []    = Nothing
    head' (x:_) = Just x

branchOpt :: IsBranchName b => b -> GitOption
branchOpt = cs . toBranchString
