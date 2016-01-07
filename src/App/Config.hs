{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Config where

import           App.ConfigBuilder
import           App.Types
import           App.Util

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except       hiding (forM_)
import           Control.Monad.Trans.Either
import           Crypto.Types.PubKey.RSA    (PrivateKey (..))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as LBS
import           Data.Either.Combinators
import           Data.Foldable
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String.Conversions
import qualified Jira.API                   as J
import           System.Directory
import           System.FilePath

defaultJiraConfig :: JiraConfig
defaultJiraConfig = JiraConfig
  { _jiraBaseUrl               = "http://jira.example.com"
  , _jiraUsername              = "myusername"
  , _jiraProject               = "MAP"
  , _jiraFinishMergeTransition = "close"
  , _jiraIssueTypeAliases      = defaultIssueTypeMap
  , _jiraSearchAliases         = defaultSearchAliases
  , _jiraOAuthConsumerKey      = "agile-cli"
  , _jiraOAuthSigningKeyPath   = "/path/to/key.pem"
  , _jiraOAuthAccessToken      = ""
  , _jiraOAuthAccessSecret     = ""
  }

defaultStashConfig :: StashConfig
defaultStashConfig = StashConfig
  { _stashBaseUrl    = "http://stash.example.com"
  , _stashProject    = "MAP"
  , _stashRepository = "myrepo"
  , _stashReviewers  = []
  }

defaultGithubConfig :: GithubConfig
defaultGithubConfig = GithubConfig
  { _githubUsername   = Nothing
  , _githubRepo       = Nothing
  , _githubOAuthToken = ""
  }

defaultIssueTypeMap :: Map.Map String String
defaultIssueTypeMap = Map.fromList
  [ "b" ~> "Bug"
  , "f" ~> "New Feature"
  , "t" ~> "Task"
  , "i" ~> "Improvement"
  ]

defaultSearchAliases :: Map.Map String String
defaultSearchAliases = Map.fromList
  [ "open"       ~> "status = open"
  , "unresolved" ~> "resolution = unresolved"
  , "resolved"   ~> "status = resolved"
  , "next"       ~> "status = open or status = reopened order by priority"
  ]

defaultConfig :: Config
defaultConfig = Config
  { _configJiraConfig          = Nothing
  , _configStashConfig         = Nothing
  , _configGithubConfig        = Nothing
  , _configDevelopBranch       = "develop"
  , _configRemoteName          = "origin"
  , _configDefaultBranchPrefix = "feature/"
  , _configBranchPrefixMap     = Map.fromList [("Bug", "bugfix/")]
  , _configBrowserCommand      = "open"
  }

templateConfig :: Config
templateConfig = Config
  { _configJiraConfig          = Just defaultJiraConfig
  , _configStashConfig         = Just defaultStashConfig
  , _configGithubConfig        = Just defaultGithubConfig
  , _configDevelopBranch       = "develop"
  , _configRemoteName          = "origin"
  , _configDefaultBranchPrefix = "feature/"
  , _configBranchPrefixMap     = Map.fromList [("Bug", "bugfix/")]
  , _configBrowserCommand      = "open"
  }

emptyConfig :: Config
emptyConfig = Config
  { _configDevelopBranch       = ""
  , _configRemoteName          = ""
  , _configDefaultBranchPrefix = ""
  , _configBranchPrefixMap     = Map.empty
  , _configBrowserCommand      = ""
  , _configJiraConfig          = Nothing
  , _configStashConfig         = Nothing
  , _configGithubConfig        = Nothing
  }

emptyJiraConfig :: JiraConfig
emptyJiraConfig = JiraConfig
  { _jiraBaseUrl               = ""
  , _jiraUsername              = ""
  , _jiraProject               = ""
  , _jiraFinishMergeTransition = ""
  , _jiraIssueTypeAliases      = Map.empty
  , _jiraSearchAliases         = Map.empty
  , _jiraOAuthConsumerKey      = ""
  , _jiraOAuthSigningKeyPath   = ""
  , _jiraOAuthAccessToken      = ""
  , _jiraOAuthAccessSecret     = ""
  }

emptyStashConfig :: StashConfig
emptyStashConfig = StashConfig
  { _stashBaseUrl    = ""
  , _stashProject    = ""
  , _stashRepository = ""
  , _stashReviewers  = []
  }

emptyGithubConfig :: GithubConfig
emptyGithubConfig = GithubConfig
  { _githubUsername   = Nothing
  , _githubRepo       = Nothing
  , _githubOAuthToken = ""
  }

-- Config loading

-- Since AppM cannot be used without an existing config,
-- errors are captured as AppExceptions in an either type.
type AppIO a = IO (Either AppException a)

getJiraApiConfig :: JiraConfig -> AppIO J.JiraConfig
getJiraApiConfig jiraConfig =
  let authConfig pk = J.OAuthConfig (jiraConfig^.jiraOAuthConsumerKey)
                                    pk
                                    (jiraConfig^.jiraOAuthAccessToken)
                                    (jiraConfig^.jiraOAuthAccessSecret)
      keyPath       = jiraConfig^.jiraOAuthSigningKeyPath
      jiraApiConfig = J.JiraConfig (jiraConfig^.jiraBaseUrl) . authConfig
  in jiraApiConfig <$$> readPrivateKey keyPath

readPrivateKey :: FilePath -> AppIO PrivateKey
readPrivateKey path = tryWith toPrivateKeyException $
  J.readPemPrivateKey =<< readFile path
  where
    tryWith f = fmap (mapLeft f) . try
    toPrivateKeyException :: SomeException -> AppException
    toPrivateKeyException e = ConfigException $ unlines'
      [ "Failed to load your private key (using path: '" ++ path ++ "')"
      , ""
      , "Error was:"
      , show e
      ]

readConfig' :: AppIO (FilePath, Config)
readConfig' = runEitherT $
      EitherT searchConfigParts
  >$< map normalizeConfigPart
  >$< mergeConfigParts
  >>= \case
    Nothing -> throwError notFoundException
    Just (ConfigPart (ConfigPath configPath) partialConfig) ->
      case missingConfigKeys (referenceConfigFor partialConfig) partialConfig  of
      [] -> do
        config <- hoistEither $ fromPartialConfig partialConfig
        return (configPath, config)
      keys -> EitherT $ handleMissingKeys keys configPath
  where
    notFoundException =  ConfigException
                         "No config file found. Please try the init command to get started."

handleMissingKeys :: [ConfigKey] -> FilePath -> AppIO (FilePath, Config)
handleMissingKeys keys configPath = do
  putStrLn "There are missing keys in your (combined) config file:"
  forM_ keys $ \key -> putStrLn $ "- " ++ show key

  askYesNoWithDefault True ("Fill default values to config at " ++ configPath ++ "?") >>= \case
    False -> error "Please fix your config, then."
    True  -> runEitherT $ do
      rawConfig <- liftIO $ readFile configPath
      existingPartialConfig <- hoistEither $ parsePartialConfig configPath rawConfig

      let defaultPartialConfig = PartialConfig $ toJSON defaultConfig
          filledConfig         = fillMissingConfigKeys defaultPartialConfig existingPartialConfig keys
      liftIO $ LBS.writeFile configPath (prettyEncode filledConfig)
      EitherT readConfig'

writeConfig :: Config -> AppM ()
writeConfig config = do
  path <- getConfigPath
  liftIO $ LBS.writeFile path (prettyEncode config)

searchConfigParts :: AppIO [ConfigPart]
searchConfigParts = getCurrentDirectory >>= searchConfigParts'
  where
    searchConfigParts' dir = runEitherT $ do
      let paths = map (\fn -> joinPath [dir, fn]) configFileNames
      localConfigs <- forM paths $ \path -> do
        config <- EitherT $ loadConfigFile path
        return $ ConfigPart (ConfigPath path) <$> config
      otherConfigs <- EitherT getOtherConfigs
      return $ otherConfigs ++ catMaybes localConfigs
      where
        loadConfigFile :: FilePath -> AppIO (Maybe PartialConfig)
        loadConfigFile path = runEitherT $
          liftIO (doesFileExist path) >>= \case
            False -> return Nothing
            True  -> do
              rawConfig     <- liftIO $ readFile path
              partialConfig <- hoistEither $ parsePartialConfig path rawConfig
              return $ Just partialConfig

        getOtherConfigs :: AppIO [ConfigPart]
        getOtherConfigs =
          let parent = takeDirectory dir
          in if parent == dir
             then return (Right [])
             else searchConfigParts' parent

parsePartialConfig :: FilePath -> String -> Either AppException PartialConfig
parsePartialConfig path = mapLeft convertException . eitherDecode . cs
  where
    convertException msg = ConfigException $ unlines
        [ "Error while parsing JSON in config file at: " ++ path ++ ":"
        , msg
        ]

fromPartialConfig :: PartialConfig -> Either AppException Config
fromPartialConfig (PartialConfig o) = mapLeft ConfigException $ parseEither parseJSON o

normalizeConfigPart :: ConfigPart -> ConfigPart
normalizeConfigPart c@(ConfigPart configPath partialConfig) =
  case readConfigKey keyPathConfigKey partialConfig of
    Just (String keyPath) ->
      if isAbsolute (cs keyPath)
      then c
      else let configPathDir = takeDirectory $ unConfigPath configPath
               fullPath      = joinPath [configPathDir, cs keyPath]
               config'       = writeKeyPath fullPath
           in ConfigPart configPath config'
    _ -> c
  where
    writeKeyPath = writeConfigKey keyPathConfigKey partialConfig . String . cs
    keyPathConfigKey = configKey "JiraConfig.OAuthSigningKeyPath"

prettyEncode :: ToJSON a => a -> LBS.ByteString
prettyEncode o =
  let prettyConfig = P.defConfig { P.confIndent = 2, P.confCompare = compare }
  in  P.encodePretty' prettyConfig o

referenceConfigFor :: PartialConfig -> PartialConfig
referenceConfigFor baseConfig = PartialConfig . toJSON $ emptyConfig
  { _configJiraConfig   = emptyJiraConfig   `ifExists` readBaseConfigKey (configKey "JiraConfig")
  , _configStashConfig  = emptyStashConfig  `ifExists` readBaseConfigKey (configKey "StashConfig")
  , _configGithubConfig = emptyGithubConfig `ifExists` readBaseConfigKey (configKey "GithubConfig")
  }
  where
    readBaseConfigKey = flip readConfigKey baseConfig
    ifExists = fmap . const

-- Config Accessors

takeJiraConfig :: (Monad m, MonadError AppException m) => Config -> m JiraConfig
takeJiraConfig config = liftMaybe ex $ config^.configJiraConfig
  where ex = ConfigException "JIRA config missing"

takeStashConfig :: (Monad m, MonadError AppException m) => Config -> m StashConfig
takeStashConfig config = liftMaybe ex $ config^.configStashConfig
  where ex = ConfigException "Stash config missing"

takeGithubConfig :: (Monad m, MonadError AppException m) => Config -> m GithubConfig
takeGithubConfig config = liftMaybe ex $ config^.configGithubConfig
  where ex = ConfigException "Github config missing"

-- For easier reading of maps
(~>) :: a -> b -> (a, b)
(~>) = (,)
