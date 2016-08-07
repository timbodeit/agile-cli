{-# LANGUAGE OverloadedStrings #-}

module Github.RepositoryTests (tests) where

import           Data.Proxy
import           Data.String
import qualified Github.Data                          as GH
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                           hiding (Test)

import           App.Backends.Github

tests :: [Test]
tests =
  [ testGroup "Github Repository"
    [ testCase
        "Can parse repository names from sample remote URLs"
        sampleRepositoryUrlsTest
    , testCase
        "Can extract fork parent repository"
        sampleForkedFromRepoRef
    ]
  ]

sampleRepositoryUrlsTest :: Assertion
sampleRepositoryUrlsTest = mapM_ testSample samples
  where
    testSample (input, expectedOutput) = extractRepository input @?= expectedOutput
    samples = [ ( "git@github.com:dsmatter/testing-repo.git"
                , Just (GithubRepoRef "dsmatter" "testing-repo")
                )
              , ( "git@github.com/dsmatter/testing-repo"
                , Just (GithubRepoRef "dsmatter" "testing-repo")
                )
              , ( "git@github.com/dsmatter/"
                , Nothing
                )
              , ( "https://example.com/github/repo"
                , Nothing
                )
              , ( "https://github.com/dsmatter/testing-repo.git"
                , Just (GithubRepoRef "dsmatter" "testing-repo")
                )
              , ( "https://somethingelse.com/dsmatter/testing-repo.git"
                , Nothing
                )
              ]

sampleForkedFromRepoRef :: Assertion
sampleForkedFromRepoRef = do
  forkedFromRepoRef forkedRepo @?= Just (GithubRepoRef "user" "example")
  forkedFromRepoRef nonForkRepo @?= Nothing
  forkedFromRepoRef brokenFork @?= Nothing
  forkedFromRepoRef brokenParent @?= Nothing
  where
    forkedRepo   = emptyRepo { GH.repoFork = Just True
                             , GH.repoParent = Just (GH.RepoRef (mkUser "user") "example")
                             }
    nonForkRepo  = emptyRepo { GH.repoFork = Just False }
    brokenFork   = emptyRepo { GH.repoFork = Nothing }
    brokenParent = emptyRepo { GH.repoFork = Just True, GH.repoParent = Nothing }
    emptyRepo    = GH.Repo { GH.repoSshUrl = Nothing
                           , GH.repoDescription = Nothing
                           , GH.repoCreatedAt = Nothing
                           , GH.repoHtmlUrl = ""
                           , GH.repoSvnUrl = Nothing
                           , GH.repoForks = Nothing
                           , GH.repoHomepage = Nothing
                           , GH.repoFork = Nothing
                           , GH.repoGitUrl = Nothing
                           , GH.repoPrivate = False
                           , GH.repoCloneUrl = Nothing
                           , GH.repoSize = Nothing
                           , GH.repoUpdatedAt = Nothing
                           , GH.repoWatchers = Nothing
                           , GH.repoOwner = mkUser "User"
                           , GH.repoName = "Test"
                           , GH.repoLanguage = Nothing
                           , GH.repoMasterBranch = Nothing
                           , GH.repoPushedAt = Nothing
                           , GH.repoId = GH.mkId Proxy 1
                           , GH.repoUrl = ""
                           , GH.repoOpenIssues = Nothing
                           , GH.repoHasWiki = Nothing
                           , GH.repoHasIssues = Nothing
                           , GH.repoHasDownloads = Nothing
                           , GH.repoParent = Nothing
                           , GH.repoSource = Nothing
                           , GH.repoHooksUrl = ""
                           , GH.repoStargazersCount = 0
                           }
    mkUser name  = GH.GithubUser "" (fromString name) "" (GH.mkId Proxy 42) Nothing
