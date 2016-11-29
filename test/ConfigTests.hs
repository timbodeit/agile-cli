{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigTests  where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Char

import           App.Config                           (emptyConfig,
                                                       templateConfig,
                                                       emptyJiraConfig,
                                                       emptyStashConfig,
                                                       emptyGithubConfig,
                                                       fromPartialConfig,
                                                       parsePartialConfig)
import           App.ConfigBuilder
import           App.Types

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.List                            (isInfixOf)
import           Data.Semigroup
import           Data.String
import qualified Data.Text                            as T
import           Data.Vector                          (fromListN)
import           System.FilePath

tests :: [Test]
tests =
  [ testGroup "Config Builder"
    [ testProperty
        "PartialConfig semigroup law"
        partialConfigSemigroupProp
    , testProperty
        "PartialConfig monoid law"
        partialConfigMonoidProp

    , testProperty
        "ConfigPart semigroup law"
        configPartSemigroupProp
    , testProperty
        "ConfigPart ordering"
        configPartOrdProp
    , testCase "ConfigPath team file ordering" testConfigPathTeamOrdering
    , testCase "ConfigPath file system ordering" testConfigPathHierarchyOrdering
    , testCase "ConfigPart merging order" testMoreImportantConfigWinsMerge
    , testCase "ConfigPart merging sample" testConfigPartMergingSample

    , testCase "mergeObjects (samples)" testMergeObjects
    , testProperty "mergeObjects (left neutrality)" mergeObjectsNeutralLeftProp
    , testProperty "mergeObjects (right neutrality)" mergeObjectsNeutralRightProp

    , testCase "broken JSON config" testBrokenJsonConfig

    , testCase "missingConfigKeys (samples)" testMissingConfigKeys
    , testCase "missingConfigKeys (JIRA sample)" testMissingJiraConfigKeys
    , testCase "missingConfigKeys (Stash sample)" testMissingStashConfigKeys
    , testCase "missingConfigKeys (Github sample)" testMissingGithubConfigKeys
    , testProperty
        "missingConfigKeys (empty reference object)"
        missingConfigKeysEmptyReferenceProp

    , testCase "fillConfigKeys" testFillMissingConfigKeys
    , testCase "fillAllConfigKeys" testFillAllMissingKeys
    ]
  ]

testMergeObjects :: Assertion
testMergeObjects = a `mergeObjects` b @?= c
  where a = object [ "a" ~> Number 42
                   , "b" ~> Number 23
                   , "c" ~> object ["a" ~> Bool True]
                   ]
        b = object [ "a" ~> Number 23
                   , "c" ~> object ["c" ~> Bool False, "b" ~> Number 42]
                   ]
        c = object [ "a" ~> Number 23
                   , "b" ~> Number 23
                   , "c" ~> object [ "a" ~> Bool True
                                   , "b" ~> Number 42
                                   , "c" ~> Bool False
                                   ]
                   ]

testMissingConfigKeys :: Assertion
testMissingConfigKeys =
  missingConfigKeys (PartialConfig reference) (PartialConfig candidate) @?= expected
  where reference = object [ "a" ~> Number 42
                           , "b" ~> Null
                           , "c" ~> object [ "a" ~> Bool False
                                           , "b" ~> Null
                                           ]
                           ]
        candidate = object [ "a" ~> Null
                           , "c" ~> object ["b" ~> Null]
                           , "d" ~> object ["b" ~> Null]
                           ]
        expected = map configKey ["b", "c.a"]

testMissingJiraConfigKeys :: Assertion
testMissingJiraConfigKeys = do
  missingConfigKeys withJiraConfig withJiraConfig @?= []
  missingConfigKeys withJiraConfig withBrokenJiraConfig @?= [brokenKey]
  where
    withJiraConfig = PartialConfig . toJSON $ emptyConfig { _configJiraConfig = Just emptyJiraConfig }
    withBrokenJiraConfig = let (PartialConfig c) = withJiraConfig
                           in  PartialConfig $ c & keySetter brokenKey .~ Nothing
    brokenKey = configKey "JiraConfig.BaseUrl"

testMissingStashConfigKeys :: Assertion
testMissingStashConfigKeys = do
  missingConfigKeys withStashConfig withStashConfig @?= []
  missingConfigKeys withStashConfig withBrokenStashConfig @?= [brokenKey]
  where
    withStashConfig = PartialConfig . toJSON $ emptyConfig { _configStashConfig = Just emptyStashConfig }
    withBrokenStashConfig = let (PartialConfig c) = withStashConfig
                           in  PartialConfig $ c & keySetter brokenKey .~ Nothing
    brokenKey = configKey "StashConfig.BaseUrl"

testMissingGithubConfigKeys :: Assertion
testMissingGithubConfigKeys = do
  missingConfigKeys withGithubConfig withGithubConfig @?= []
  missingConfigKeys withGithubConfig withBrokenGithubConfig @?= [brokenKey]
  where
    withGithubConfig = PartialConfig . toJSON $ emptyConfig { _configGithubConfig = Just emptyGithubConfig }
    withBrokenGithubConfig = let (PartialConfig c) = withGithubConfig
                           in  PartialConfig $ c & keySetter brokenKey .~ Nothing
    brokenKey = configKey "GithubConfig.OAuthToken"

mergeObjectsNeutralLeftProp :: Property
mergeObjectsNeutralLeftProp = forAll anyJsonObject $ \o ->
  object [] `mergeObjects` o == o

mergeObjectsNeutralRightProp :: Property
mergeObjectsNeutralRightProp = forAll anyJsonObject $ \o ->
  o `mergeObjects` object [] == o

missingConfigKeysEmptyReferenceProp :: PartialConfig -> Bool
missingConfigKeysEmptyReferenceProp pc =
  null $ missingConfigKeys (PartialConfig $ object []) pc

partialConfigSemigroupProp :: PartialConfig -> PartialConfig -> PartialConfig -> Bool
partialConfigSemigroupProp pc1 pc2 pc3 =
  pc1 <> (pc2 <> pc3) == (pc1 <> pc2) <> pc3

partialConfigMonoidProp :: PartialConfig -> Bool
partialConfigMonoidProp pc =
  mempty <> pc == pc && pc <> mempty == pc

configPartSemigroupProp :: ConfigPart -> ConfigPart -> ConfigPart -> Bool
configPartSemigroupProp cp1 cp2 cp3 =
  cp1 <> (cp2 <> cp3) == (cp1 <> cp2) <> cp3

configPartOrdProp :: Property
configPartOrdProp =
  forAll randomPathLength $ \a ->
  forAll randomPathLength $ \b -> do
  path1 <- randomConfigPath a
  path2 <- randomConfigPath b
  return $ ConfigPart path1 mempty `compare` ConfigPart path2 mempty == a `compare` b
  where
    randomConfigPath n = ConfigPath . joinPath . (++ [".agile"]) <$> replicateM n randomPathPart

testConfigPathTeamOrdering :: Assertion
testConfigPathTeamOrdering =
  assertBool ".agile should be more important then .agile-team" $
     ConfigPath ".agile" > ConfigPath ".agile-team"
  && ConfigPath ".agile-team" > ConfigPath ".other-config"
  && ConfigPath "/a/.agile-team" > ConfigPath "/.agile"

testConfigPathHierarchyOrdering :: Assertion
testConfigPathHierarchyOrdering =
  assertBool "files in deeper directories are more important" $
  ConfigPath "/quite/deep/.agile-team" > ConfigPath "/shallow/.agile"

testMoreImportantConfigWinsMerge :: Assertion
testMoreImportantConfigWinsMerge =
  let cp1 = ConfigPart "/a/.agile"   . PartialConfig $ object [("a", Bool True)]
      cp2 = ConfigPart "/a/b/.agile" . PartialConfig $ object [("a", Bool False)]
  in case mergeConfigParts [cp1, cp2] of
       Nothing -> assertFailure "Configs should be mergable"
       Just (ConfigPart _ (PartialConfig o)) ->
         object [("a", Bool False)] @=? o

testConfigPartMergingSample :: Assertion
testConfigPartMergingSample =
  let baseConfigPart   = ConfigPart "/home/user/.agile" baseConfig
      teamConfigPart   = ConfigPart "/home/user/project/.agile-team" teamConfig
      myConfigPart     = ConfigPart "/home/user/project/.agile" myConfig
      configParts      = [baseConfigPart, teamConfigPart, myConfigPart]
      mergedConfigPart = mergeConfigParts configParts
  in case mergedConfigPart of
  Nothing ->
    assertFailure "Could not merge config parts"
  Just (ConfigPart path partialConfig) ->
    case fromPartialConfig partialConfig of
      Left e       -> assertFailure $ "Could not parse partial config\n" ++ show e
      Right config -> do
        path @?= "/home/user/project/.agile"
        config^.configJiraConfig._Just.jiraBaseUrl @?= "http://jira.example.com"
        config^.configJiraConfig._Just.jiraProject @?= "MY"
        config^.configJiraConfig._Just.jiraUsername @?= "myself"
  where
    baseConfig = PartialConfig . toJSON $ emptyConfig { _configJiraConfig = Just emptyJiraConfig }
    teamConfig = PartialConfig $ object [ "JiraConfig" ~> object
                                          [ "BaseUrl"  ~> "http://jira.example.com"
                                          , "Username" ~> "nobody"
                                          ]
                                        ]
    myConfig   = PartialConfig $ object [ "JiraConfig" ~> object
                                          [ "Project"   ~> "MY"
                                          , "Username"  ~> "myself"
                                          ]
                                        ]

testBrokenJsonConfig :: Assertion
testBrokenJsonConfig = case parsePartialConfig "(no file)" brokenJson of
  Right _ -> assertFailure "Parse should have failed for broken JSON."
  Left (ConfigException s) -> assertBool
                              "Exception message should contain 'Error while parsing JSON'" $
                              "Error while parsing JSON" `isInfixOf` s
  Left _ -> assertFailure "Broken config JSON should result in a ConfigException."
  where
    brokenJson = "{\"foo\":\"bar\",[}"

testFillMissingConfigKeys :: Assertion
testFillMissingConfigKeys = do
  missingConfigKeys reference testConfig @?= expectedMissingKeys
  missingConfigKeys reference (fillMissingConfigKeys reference testConfig expectedMissingKeys) @?= []
  where
    testConfig = (PartialConfig $ toJSON emptyConfig)
      & "JiraConfig.Username" ~~ "testuser"
      & "JiraConfig.BaseUrl"  ~~ "http://example.com"
    reference = testConfig
      & "JiraConfig.Project"  ~~ "testproject"

    expectedMissingKeys = [configKey "JiraConfig.Project"]
    (~~) ck v o = writeConfigKey (configKey ck) o (String v)

testFillAllMissingKeys :: Assertion
testFillAllMissingKeys =
  let keys = missingConfigKeys referenceConfig nullConfig
  in fillMissingConfigKeys referenceConfig nullConfig keys @?= referenceConfig
  where
    referenceConfig = PartialConfig $ toJSON templateConfig
    nullConfig      = PartialConfig $ object []

-- Random Generation

anyJsonObject :: Gen Value
anyJsonObject = do
  len <- choose (0, 10) :: Gen Int
  object <$> replicateM len randomJsonPair
  where
    randomJsonPair = (,) <$> randomJsonKey <*> arbitrary

anyJsonArray :: Gen Value
anyJsonArray = do
  len <- choose (0, 20) :: Gen Int
  v   <- fromListN len <$> arbitrary
  return $ Array v

randomJsonKey :: Gen T.Text
randomJsonKey = do
  len <- choose (1, 20)
  s   <- take len <$> listOf1 arbitrary
  return $ fromString s

instance Arbitrary T.Text where
  arbitrary = fromString <$> arbitrary

instance Arbitrary Value where
  arbitrary = do
    typeChooser <- choose (0, 100) :: Gen Int
    case typeChooser of
      n | n < 5  -> anyJsonObject
        | n < 10 -> anyJsonArray
        | n < 40 -> String <$> arbitrary
        | n < 70 -> Number . fromInteger <$> arbitrary
        | n < 95 -> Bool <$> arbitrary
        | otherwise -> pure Null

instance Arbitrary PartialConfig where
  arbitrary = PartialConfig <$> anyJsonObject

instance Arbitrary ConfigPath where
  arbitrary = ConfigPath <$> randomPath

instance Arbitrary ConfigPart where
  arbitrary = ConfigPart <$> arbitrary <*> arbitrary

randomPath :: Gen FilePath
randomPath = do
  len   <- choose (0, 10) :: Gen Int
  parts <- replicateM len randomPathPart
  return $ joinPath parts

randomPathPart :: Gen String
randomPathPart = listOf1 lowerAlpha

randomPathLength :: Gen Int
randomPathLength = choose (0, 20)

(~>) :: a -> b -> (a, b)
(~>) = (,)

