module Github.RepositoryTests (tests) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char

import App.Backends.Github

tests :: [Test]
tests =
  [ testGroup "Github Repository"
    [ testCase
        "Can parse repository names from sample remote URLs"
        sampleRepositoryUrlsTest
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
              , ( "https://github.com/dsmatter/testing-repo.git"
                , Just (GithubRepoRef "dsmatter" "testing-repo")
                )
              , ( "https://somethingelse.com/dsmatter/testing-repo.git"
                , Nothing
                )
              ]
