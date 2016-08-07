module ParserTests (tests) where

import           Data.Char
import           Data.Either
import           Jira.API
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char
import qualified Text.Parsec                          as P

import           App.Backends.Jira.Parsers

tests :: [Test]
tests =
  [ testGroup "Parsers"
    [ testProperty "parseIssueNumber" issueNumberParserPostiveIntProp
    , testProperty "parseIssueNumber" issueNumberParserNegativeIntProp
    , testCase     "parseIssueKey (leading dash)" testIssueKeyParserDashPrefix
    , testProperty "parseIssueKey" issueKeyParserProp
    , testProperty "parseIssueKey (negative check)" issueKeyParserNegativeProp
    , testProperty "parseIssueKeyWithDefaultProject" issueKeyParserWithDefaultProjectWithPrefixProp
    , testProperty "parseIssueKeyWithDefaultProject" issueKeyParserWithDefaultProjectWithoutPrefixProp
    ]
  ]

issueNumberParserPostiveIntProp :: Positive Int -> Bool
issueNumberParserPostiveIntProp (Positive n) =
  P.parse issueNumberParser "" (show n) == Right (IssueNumber n)

issueNumberParserNegativeIntProp :: Property
issueNumberParserNegativeIntProp = forAll negativeInt $ \n ->
  isLeft $ P.parse issueNumberParser "" (show n)

testIssueKeyParserDashPrefix :: Assertion
testIssueKeyParserDashPrefix =
  assertBool "Parsing should fail for leading dash" $
    isLeft $ P.parse issueKeyParser "" "-123"

issueKeyParserProp :: Positive Int -> Property
issueKeyParserProp (Positive n) = forAll (listOf1 upperAlpha) $ \project ->
  P.parse issueKeyParser "" (project ++ "-" ++ show n) ==
    Right (IssueKey project (IssueNumber n))

issueKeyParserNegativeProp :: Property
issueKeyParserNegativeProp =
  forAll (arbitrary `suchThat` any (not . isAlpha)) $ \project ->
  forAll (arbitrary `suchThat` (not . all isDigit))   $ \n ->
  isLeft $ P.parse issueKeyParser "" (project ++ "-" ++ n)

issueKeyParserWithDefaultProjectWithPrefixProp :: String -> Positive Int -> Property
issueKeyParserWithDefaultProjectWithPrefixProp s (Positive n) =
  forAll (listOf1 upperAlpha) $ \project ->
  P.parse (issueKeyParserWithDefaultProject s) "" (project ++ "-" ++ show n) ==
    Right (IssueKey project (IssueNumber n))

issueKeyParserWithDefaultProjectWithoutPrefixProp :: String -> Positive Int -> Bool
issueKeyParserWithDefaultProjectWithoutPrefixProp s (Positive n) =
  P.parse (issueKeyParserWithDefaultProject s) "" (show n) ==
    Right (IssueKey s (IssueNumber n))

negativeInt :: Gen Int
negativeInt = arbitrary `suchThat` (< 0)


