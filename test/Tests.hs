import           Control.Monad
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

import           App.CLI.Parsers
import           App.Config
import           App.Types
import           App.Util

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Parsers"
    [ testProperty "parseIssueNumber" issueNumberParserPostiveIntProp
    , testProperty "parseIssueNumber" issueNumberParserNegativeIntProp
    , testProperty "parseIssueKey" issueKeyParserProp
    , testProperty "parseIssueKey" issueKeyParserNegativeProp
    , testProperty "parseIssueKeyWithDefaultProject" issueKeyParserWithDefaultProjectWithPrefixProp
    , testProperty "parseIssueKeyWithDefaultProject" issueKeyParserWithDefaultProjectWithoutPrefixProp
    ]
  , testGroup "Utils"
    [ testCase "toMaybe" testToMaybe
    , testCase "liftMaybe" testLiftMaybe
    , testProperty "trim (prefix)" trimPrefixProp
    , testProperty "trim (postfix)" trimPostfixProp
    , testProperty "trim (prefix/postfix)" trimPrefixPostfixProp
    , testCase "trim (samples)" testTrim
    ]
  ]

testToMaybe :: Assertion
testToMaybe = do
  nothing @=? toMaybe (Left 42)
  Just 42 @=? toMaybe (Right 42)
  where nothing :: Maybe Int
        nothing = Nothing

testLiftMaybe :: Assertion
testLiftMaybe = do
  succeeded <- runApp "" defaultConfig $ liftMaybe exception (Just "foo")
  failed    <- runApp "" defaultConfig $ liftMaybe exception Nothing

  either (const rightExcpected) (void . return) succeeded
  either (void . return) (const leftExcpected) failed
  where
    exception = UserInputException "failed"
    rightExcpected = assertFailure "Successful computation should yield Right value"
    leftExcpected  = assertFailure "Failed computation should yield Light value"

testTrim :: Assertion
testTrim = do
  "" @=? trim ""
  "" @=? trim "  "
  "foo" @=? trim "foo"
  "foo" @=? trim " foo"
  "foo" @=? trim "   foo "
  "foo bar" @=? trim "   foo bar "

trimPrefixProp =
  forAll (listOf (oneof [whitespace, space])) $ \prefix ->
  forAll (listOf1 notSpace) $ \w ->
  trim (prefix ++ w) == w

trimPostfixProp =
  forAll (listOf (oneof [whitespace, space])) $ \postfix ->
  forAll (listOf1 notSpace) $ \w ->
  trim (w ++ postfix) == w

trimPrefixPostfixProp =
  forAll (listOf (oneof [whitespace, space])) $ \prefix ->
  forAll (listOf (oneof [whitespace, space])) $ \postfix ->
  forAll (listOf1 notSpace) $ \w ->
  trim (prefix ++ w ++ postfix) == w

issueNumberParserPostiveIntProp (Positive n) =
  P.parse issueNumberParser "" (show n) == Right (IssueNumber n)

issueNumberParserNegativeIntProp = forAll negativeInt $ \n ->
  isLeft $ P.parse issueNumberParser "" (show n)

issueNumberParserNonNumberProp = forAll (listOf notDigit) $ \s ->
  isLeft $ P.parse issueNumberParser "" s

issueKeyParserProp (Positive n) = forAll (listOf upperAlpha) $ \project ->
  P.parse issueKeyParser "" (project ++ "-" ++ show n) ==
    Right (IssueKey project (IssueNumber n))

issueKeyParserNegativeProp =
  forAll (arbitrary `suchThat` any (not . isAlpha)) $ \project ->
  forAll (arbitrary `suchThat` (not . all isDigit))   $ \n ->
  isLeft $ P.parse issueKeyParser "" (project ++ "-" ++ n)

issueKeyParserWithDefaultProjectWithPrefixProp s (Positive n) =
  forAll (listOf upperAlpha) $ \project ->
  P.parse (issueKeyParserWithDefaultProject s) "" (project ++ "-" ++ show n) ==
    Right (IssueKey project (IssueNumber n))

issueKeyParserWithDefaultProjectWithoutPrefixProp s (Positive n) =
  P.parse (issueKeyParserWithDefaultProject s) "" (show n) ==
    Right (IssueKey s (IssueNumber n))

notSpace :: Gen Char
notSpace = nonSpace `suchThat` (not . isSpace)

notDigit :: Gen Char
notDigit = arbitrary `suchThat` (not . isDigit)

negativeInt :: Gen Int
negativeInt = arbitrary `suchThat` (< 0)
