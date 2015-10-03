module Git.BranchParserTests (tests) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char

import           App.Git

tests :: [Test]
tests = [ testGroup "Git"
          [ testGroup "Branch Parsers"
            [ testProperty "parseBranchName" branchNameParseProp
            , testProperty "branchNameParseWhitespaceProp" branchNameParseWhitespaceProp
            , testProperty "remoteParseBranchName" remoteNameParseProp
            , testCase "branchNameSamples" branchNameSampleTests
            ]
          ]
        ]

branchNameParseProp :: Property
branchNameParseProp =
  forAll commonString $ \s ->
    parseBranchName s == Just (BranchName s)

branchNameParseWhitespaceProp :: Property
branchNameParseWhitespaceProp =
  forAll commonString $ \prefix ->
  forAll commonString $ \postfix ->
  forAll whitespace   $ \ws ->
    let branch = parseBranchName (prefix ++ [ws] ++ postfix) :: Maybe BranchName
    in isNothing branch

remoteNameParseProp :: Property
remoteNameParseProp =
  forAll commonString $ \branchPart ->
  forAll (commonString `suchThat` (not . isInfixOf "/")) $ \remotePart ->
    let parsedBranch = parseBranchName (remotePart ++ "/" ++ branchPart)
    in parsedBranch == Just (RemoteBranchName (RemoteName remotePart) (BranchName branchPart))

branchNameSampleTests :: Assertion
branchNameSampleTests = forM_ samples $ \(input, expected) ->
  assertEqual "Sample test failed" expected (parseBranchName input)
  where
    samples = [ shouldSucceed "simple"
              , shouldFail ""
              , shouldFail "invalid name"
              , shouldFail "with\nnewline"
              , shouldSucceed "feature/something"
              , shouldSucceed "bugfix/TIK-42"
              , shouldSucceed "bugfix/TIK-42-issue-description"
              ]
    shouldSucceed s = (s, Just (BranchName s))
    shouldFail    s = (s, Nothing)


-- Random Generation

commonString :: Gen String
commonString = listOf1 commonChar

commonChar :: Gen Char
commonChar = oneof [lowerAlpha, upperAlpha, numeric, separatorChar]

separatorChar :: Gen Char
separatorChar = oneof . map return $ "-_%$#@!?,="

