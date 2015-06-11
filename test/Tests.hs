import           Control.Monad
import           Data.Char
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char

import           App.Config
import           App.Types
import           App.Util

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Utils"
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

notSpace :: Gen Char
notSpace = nonSpace `suchThat` (not . isSpace)
