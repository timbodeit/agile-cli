{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Github.IntegrationTests (tests) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)

import           App.Backends
import           App.CLI
import           App.CLI.Options
import           App.Config
import           App.Git.Branch                       (parseBranchName)
import           App.Types

import           Control.Exception
import           Control.Lens
import           Data.Proxy
import           Data.String                          (fromString)
import qualified Github.Issues                        as GH
import           Shelly                               hiding (FilePath)
import           System.Directory
import           System.IO.Temp

tests :: [Test]
tests =
  [ testGroup "Github Integration Tests"
    [ testCase "Get issue by ID" getIssueByIdTest
    , testCase "Get issue branch name" getIssueByBranchNameTest
    , testCase "Get issue for fixed repo" getIssueForFixedRepoTest
    , testCase "Search issues" searchIssuesTest
    ]
  ]

getIssueByIdTest :: Assertion
getIssueByIdTest = withTestEnvironment $ \_ -> do
  let Just ghConfig = testConfig^.configGithubConfig
  runApp' testConfig (getIssueById (GithubIssueId 1) ghConfig) >>= \case
    Left e -> assertFailure $ "Error while fetching issue:\n" ++ show e
    Right issue -> assertTestIssue issue

getIssueByBranchNameTest :: Assertion
getIssueByBranchNameTest = withTestEnvironment $ \_ -> do
  let Just ghConfig = testConfig^.configGithubConfig
  let fetchIssue = do
        let Just branchName = parseBranchName "#1-fix-the-bug"
        issueId <- extractIssueId branchName ghConfig
        getIssueById issueId ghConfig

  runApp' testConfig fetchIssue >>= \case
    Left e -> assertFailure $ "Error while fetching issue:\n" ++ show e
    Right issue -> assertTestIssue issue

getIssueForFixedRepoTest :: Assertion
getIssueForFixedRepoTest = withTestEnvironment $ \_ -> do
  let Just ghConfig = testConfig^.configGithubConfig
  shelly . silently $ run "git" ["remote", "remove", "origin"]
  runApp' testConfig (getIssueById (GithubIssueId 1) ghConfig) >>= \case
    Left _  -> return ()
    Right _ -> assertFailure "Should not be able to fetch issue"

  let fixedGhConfig = ghConfig { _githubUsername = Just "dsmatter"
                               , _githubRepo     = Just "testing-repo"
                               }

  runApp' testConfig (getIssueById (GithubIssueId 1) fixedGhConfig) >>= \case
    Left e -> assertFailure $ "Error while fetching issue:\n" ++ show e
    Right issue -> assertTestIssue issue

searchIssuesTest :: Assertion
searchIssuesTest = withTestEnvironment $ \_ -> do
  let Just ghConfig = testConfig^.configGithubConfig
  let options = SearchOptions False False False
  runApp' testConfig (searchIssues options "test in:title" ghConfig) >>= \case
    Left e -> assertFailure $ "Error while searching issue:\n" ++ show e
    Right issues -> assertBool "Expected to find issues" $ not (null issues)

assertTestIssue :: GH.Issue -> Assertion
assertTestIssue issue = do
  GH.issueNumber issue @?= 1
  GH.untagId (GH.issueId issue) @?= 123876204
  GH.issueTitle issue @?= "Test issue"

withTestEnvironment :: (FilePath -> IO a) -> IO a
withTestEnvironment f =
  withSystemTempDirectory "agile-test" $ \dir -> do
    shelly . silently . chdir (fromString dir) $ do
      run "git" ["init"]
      run "git" ["remote", "add", "origin", "https://github.com/dsmatter/testing-repo"]
    bracket getCurrentDirectory setCurrentDirectory . const $ do
      setCurrentDirectory dir
      f dir

runApp' :: Config -> AppM a -> IO (Either AppException a)
runApp' = runApp "(ad-hoc)"

testConfig :: Config
testConfig = emptyConfig { _configGithubConfig = Just emptyGithubConfig
                         , _configRemoteName   = "origin"
                         }
