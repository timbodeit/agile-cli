{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module App.ConfigBuilder where

import           Prelude                 hiding (concatMap, foldl, foldr)

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (elemIndex, intercalate)
import qualified Data.List.NonEmpty      as NE
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           System.FilePath

configFileNames :: [FilePath]
configFileNames = [".agile", ".agile-team"]

preferredConfigFileName :: FilePath
preferredConfigFileName = head configFileNames

newtype ConfigKey = ConfigKey (NE.NonEmpty String) deriving (Eq)

instance Show ConfigKey where
  show (ConfigKey k) = intercalate "." $ NE.toList k

configKey :: String -> ConfigKey
configKey "" = error "Empty config key"
configKey c  = ConfigKey . NE.fromList . splitOn "." $ c

fromConfigKey :: ConfigKey -> NE.NonEmpty String
fromConfigKey (ConfigKey k) = k

newtype PartialConfig = PartialConfig Value deriving (Show, Eq, FromJSON, ToJSON)

instance Semigroup PartialConfig where
  PartialConfig c1 <> PartialConfig c2 = PartialConfig $ mergeObjects c1 c2

instance Monoid PartialConfig where
  mempty = PartialConfig $ object []
  mappend = (<>)

newtype ConfigPath = ConfigPath { unConfigPath :: FilePath } deriving (Show, Eq, IsString)

instance Ord ConfigPath where
  -- Compare by path "deepness" and config file name priority
  compare = comparing (length . splitPath . unConfigPath)
         <> comparing (configNameRank . takeFileName . unConfigPath)
    where
      configNameRank n = fromMaybe (-1) . elemIndex n . reverse $ configFileNames

data ConfigPart = ConfigPart { configPartPath   :: ConfigPath
                             , configPartConfig :: PartialConfig
                             } deriving (Show, Eq)

instance Ord ConfigPart where
  compare = comparing configPartPath

instance Semigroup ConfigPart where
  (ConfigPart p1 c1) <> (ConfigPart p2 c2) =
    let path   = max p1 p2
        config = c1 <> c2
    in ConfigPart path config

mergeConfigParts :: [ConfigPart] -> Maybe ConfigPart
mergeConfigParts = fmap mergeConfigParts' . NE.nonEmpty

mergeConfigParts' :: NE.NonEmpty ConfigPart -> ConfigPart
mergeConfigParts' = sconcat . NE.sort

readConfigKey :: ConfigKey -> PartialConfig -> Maybe Value
readConfigKey k (PartialConfig c) = c ^? keyGetter k

writeConfigKey :: ConfigKey -> PartialConfig -> Value -> PartialConfig
writeConfigKey key config value =
  let PartialConfig config' = foldr ensureObjectAt config $ prefixes key
  in PartialConfig $ config' & keySetter key ?~ value
  where
    ensureObjectAt :: ConfigKey -> PartialConfig -> PartialConfig
    ensureObjectAt k config = case readConfigKey k config of
      Nothing -> writeConfigKey k config (object [])
      Just _  -> config

    prefixes :: ConfigKey -> [ConfigKey]
    prefixes = map (ConfigKey . NE.fromList) . init . NE.tail . NE.inits . fromConfigKey

mergeObjects :: Value -> Value -> Value
mergeObjects base reference =
  let referenceKeys = reference ^. _Object . to H.keys
  in foldr mergeKey base referenceKeys
  where
    mergeKey k o =
      let referenceValue  = reference ^?! key k
          referenceObject = referenceValue ^? _Object
          baseObject      = o ^? key k . _Object
      in case (baseObject, referenceObject) of
           (Just bo, Just ro) -> o & setKey k ?~ mergeObjects (Object bo) (Object ro)
           _                  -> o & setKey k ?~ referenceValue

missingConfigKeys :: PartialConfig -> PartialConfig -> [ConfigKey]
missingConfigKeys (PartialConfig a) (PartialConfig b) = go a b []
  where
    go reference candidate path =
      let referenceKeys = reference ^. _Object . to H.keys . to S.fromList
          candidateKeys = candidate ^. _Object . to H.keys . to S.fromList
          commonKeys    = S.intersection referenceKeys candidateKeys
          missingKeys   = S.difference   referenceKeys candidateKeys
          missingPaths  = map toConfigKey $ S.toList missingKeys
          deepPaths     = concatMap (\k -> go (reference ^?! key k)
                                              (candidate ^?! key k)
                                              (path ++ [cs k]))
                                    (S.toList commonKeys)
      in missingPaths ++ deepPaths
      where
        toConfigKey = ConfigKey . NE.fromList . (path ++) . pure . cs

fillMissingConfigKeys :: PartialConfig -> [ConfigKey] -> PartialConfig -> PartialConfig
fillMissingConfigKeys config keys referenceConfig = foldr fillKey config keys
  where
    fillKey :: ConfigKey -> PartialConfig -> PartialConfig
    fillKey key config = case readConfigKey key referenceConfig of
      Just v  -> writeConfigKey key config v
      Nothing -> config

-- Lens helpers

-- Builds a lens from the provided config key.
keyGetter :: ConfigKey -> Traversal' Value Value
keyGetter (ConfigKey k) = keyGetter' (toList k)

-- Builds a lens from the provided key path.
keyGetter' :: [String] -> Traversal' Value Value
keyGetter' = foldl (\f part -> f . key (cs part)) id

-- Builds a lens which can modify/add/delete the value
-- associated with the provided config key.
keySetter :: ConfigKey -> Traversal' Value (Maybe Value)
keySetter (ConfigKey k) =
  let g = keyGetter' (NE.init k)
      s = setKey (cs (NE.last k))
  in  g . s

-- Aeson equivalent of at
setKey :: (AsValue t) => T.Text -> Traversal' t (Maybe Value)
setKey k = _Object . at k
