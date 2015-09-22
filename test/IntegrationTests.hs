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
  localShelly $ run "git" ["checkout", "-b", "feature/TEST-42-branch-status-test"]
  assertCurrentBranchStatus NoUpstream

  -- Setup an up-to-date tracking branch
  localShelly $ run "git" ["push", "-u", "origin", "HEAD"]
  assertCurrentBranchStatus UpToDate

  -- Add new local commit
  localShelly $ run "git" ["commit", "--allow-empty", "-am", "Second commit"]
  assertCurrentBranchStatus NewCommits
  where
    assertCurrentBranchStatus expected = runGit branchStatus >>= \case
      Left _       -> assertFailure "Branch status should succeed."
      Right status -> assertBranchStatus expected status
    assertBranchStatus = assertEqual "Unexpected branch status"

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
