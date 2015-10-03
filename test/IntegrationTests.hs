{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests (tests) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char       hiding (bracket)

import           App.Git
import           Jira.API

import           Control.Exception
import           Data.String
import           Shelly                               hiding (FilePath)
import           System.Directory
import           System.IO.Temp

tests :: [Test]
tests =
  [ testGroup "Integration Tests"
    [ testCase "Branch status tests" branchStatusTests
    , testCase "Working copy status tests" workingCopyStatusTests
    ]
  ]

branchStatusTests :: Assertion
branchStatusTests = withGitTestEnvironment $ \dir -> do
  let localShelly = shelly' . chdir (fromString dir)

  -- Set up a local feature branch
  let branchName = "feature/" ++ show issueKey ++ "-branch-status-test"
  localShelly $ run "git" ["checkout", "-b", fromString branchName]
  assertCurrentBranchStatus NoUpstream

  -- Push to a remote branch with the same name (without making it the tracking branch)
  let branchArg = branchName ++ ":" ++ branchName
  localShelly $ run "git" ["push", fromString remote, fromString branchArg]
  assertCurrentBranchStatus UpToDate

  -- Add local commit
  localShelly $ run "git" ["commit", "--allow-empty", "-am", "Changed something"]
  assertCurrentBranchStatus NewCommits

  -- Push to a non-tracking remote branch without the issue key in it
  let otherBranchArg = branchName ++ ":feature/TEST-43-something-else"
  localShelly $ run "git" ["push", fromString remote, fromString otherBranchArg]
  assertCurrentBranchStatus NewCommits

  -- Making it the tracking branch does not have an influence
  localShelly $ run "git" ["push", "-u", fromString remote, fromString otherBranchArg]
  assertCurrentBranchStatus NewCommits

  -- Push to a non-tracking remote branch on other remote
  withSystemTempDirectory "agile-test-other-remote" $ \otherRemoteDir -> do
    shelly' . chdir (fromString otherRemoteDir) $ run "git" ["init", "--bare"]
    localShelly $ run "git" ["remote", "add", "other", fromString otherRemoteDir]
    localShelly $ run "git" ["push", "other", fromString branchArg]
    assertCurrentBranchStatus NewCommits

    -- Making it the tracking branch does not have an influence
    localShelly $ run "git" ["push", "-u", "other", fromString branchArg]
    assertCurrentBranchStatus NewCommits

  -- Push to a branch on the right remote containing the issue key
  let branchArg' = branchName ++ ":feature/" ++ show issueKey ++ "-alternative"
  localShelly $ run "git" ["push", fromString remote, fromString branchArg']
  assertCurrentBranchStatus UpToDate

  -- Making it the tracking branch still works
  localShelly $ run "git" ["push", "-u", fromString remote, fromString branchArg']
  assertCurrentBranchStatus UpToDate

  -- Set up the canonical remote branch as tracking
  let remoteBranch = remote ++ "/" ++ branchName
  localShelly $ run "git" ["branch", "-u", fromString remoteBranch]
  assertCurrentBranchStatus NewCommits

  -- Push to tracking branch
  localShelly $ run "git" ["push", fromString remote]
  assertCurrentBranchStatus UpToDate
  where
    assertCurrentBranchStatus expected = runGit (branchStatus (RemoteName remote) issueKey) >>= \case
      Left e       -> assertFailure $ "Branch status should succeed. Failure is: " ++ show e
      Right status -> assertBranchStatus expected status
    assertBranchStatus = assertEqual "Unexpected branch status"
    issueKey = IssueKey "TEST" (IssueNumber 42)
    remote = "origin"

workingCopyStatusTests :: Assertion
workingCopyStatusTests = withGitTestEnvironment $ \dir -> do
  let localShelly = shelly' . chdir (fromString dir)

  -- Make sure we are clean
  localShelly $ run "git" ["commit", "--allow-empty", "-am", "Second commit"]
  assertCurrentWorkingCopyStatus Clean

  -- Create an untracked file
  localShelly $ writefile (fromString "newfile") "test"
  assertCurrentWorkingCopyStatus Clean

  -- Track new file
  localShelly $ run "git" ["add", "newfile"]
  assertCurrentWorkingCopyStatus Dirty

  -- Commit changes
  localShelly $ run "git" ["commit", "-am", "Third commit"]
  assertCurrentWorkingCopyStatus Clean

  -- Make a mess again
  localShelly $ writefile (fromString "newfile") "agile"
  assertCurrentWorkingCopyStatus Dirty
  where
    assertCurrentWorkingCopyStatus expected = runGit workingCopyStatus >>= \case
      Left _       -> assertFailure "Working copy status should succeed"
      Right status -> assertWorkingCopyStatus expected status
    assertWorkingCopyStatus = assertEqual "Unexpected working copy status"

withGitTestEnvironment :: (FilePath -> IO a) -> IO a
withGitTestEnvironment f =
  withSystemTempDirectory "agile-test" $ \dir ->
    withSystemTempDirectory "agile-test-remote" $ \remoteDir -> do
      shelly' . chdir (fromString remoteDir) $ run "git" ["init", "--bare"]
      shelly' . chdir (fromString dir) $ do
        run "git" ["init"]
        run "git" ["remote", "add", "origin", fromString remoteDir]
        writefile (fromString "testfile") "agile test"
        run "git" ["add", "testfile"]
        run "git" ["commit", "-am", "Initial commit"]
        run "git" ["push", "-u", "origin", "HEAD"]
      bracket getCurrentDirectory setCurrentDirectory . const $ do
        setCurrentDirectory dir
        f dir

shelly' :: Sh a -> IO a
shelly' = shelly . silently
