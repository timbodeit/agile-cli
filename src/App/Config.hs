{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module App.Config where

import           App.Types
import           App.Util

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Crypto.Types.PubKey.RSA    (PrivateKey (..))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import qualified Data.ByteString.Lazy       as LBS
import           Data.Either.Combinators
import qualified Data.Map                   as Map
import           Data.String.Conversions
import qualified Jira.API                   as J
import           System.Directory
import           System.FilePath

configFileName :: FilePath
configFileName = ".agile"

defaultJiraConfig :: JiraConfig
defaultJiraConfig = JiraConfig
  { _jiraBaseUrl             = "http://jira.example.com"
  , _jiraUsername            = "myusername"
  , _jiraProject             = "MAP"
  , _jiraIssueTypeAliases    = defaultIssueTypeMap
  , _jiraSearchAliases       = defaultSearchAliases
  , _jiraOAuthConsumerKey    = "agile-cli"
  , _jiraOAuthSigningKeyPath = "/path/to/key.pem"
  , _jiraOAuthAccessToken    = ""
  , _jiraOAuthAccessSecret   = ""
  }

defaultStashConfig :: StashConfig
defaultStashConfig = StashConfig
  { _stashBaseUrl    = "http://stash.example.com"
  , _stashProject    = "MAP"
  , _stashRepository = "myrepo"
  , _stashReviewers  = []
  }

defaultConfig :: Config
defaultConfig = Config
  { _configJiraConfig       = defaultJiraConfig
  , _configStashConfig      = defaultStashConfig
  , _configDevelopBranch    = "develop"
  , _configRemoteName       = "origin"
  , _configBrowserCommand   = "open"
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

-- Config loading

-- Since AppM cannot be used without an existing config,
-- errors are captured as AppExceptions in an either type.
type AppIO a = IO (Either AppException a)

getJiraApiConfig :: Config -> AppIO J.JiraConfig
getJiraApiConfig config =
  let jiraConfig    = config^.configJiraConfig
      authConfig pk = J.OAuthConfig (jiraConfig^.jiraOAuthConsumerKey)
                                    pk
                                    (jiraConfig^.jiraOAuthAccessToken)
                                    (jiraConfig^.jiraOAuthAccessSecret)
      jiraApiConfig = J.JiraConfig (jiraConfig^.jiraBaseUrl) . authConfig
  in jiraApiConfig <$$> readPrivateKey (jiraConfig^.jiraOAuthSigningKeyPath)

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

readConfig :: FilePath -> AppIO Config
readConfig path = runEitherT $ do
  rawConfig <- hoistEitherIO $ readConfigFile path
  hoistEither $ parseConfig rawConfig
  where
    readConfigFile :: FilePath -> AppIO String
    readConfigFile = try . readFile

readConfig' :: AppIO (FilePath, Config)
readConfig' =
  searchConfig >>= \case
    Nothing   -> return $ Left (ConfigException "Config file not found")
    Just path -> (path,) <$$> readConfig path

writeConfig :: Config -> AppM ()
writeConfig config = do
  path <- getConfigPath
  liftIO $ LBS.writeFile path (prettyEncodeConfig config)

searchConfig :: IO (Maybe FilePath)
searchConfig = getCurrentDirectory >>= go
  where
    go :: FilePath -> IO (Maybe FilePath)
    go dir = do
      let path = joinPath [dir, configFileName]
      exists <- doesFileExist path
      if exists
      then return $ Just path
      else let parent = takeDirectory dir
           in if parent == dir
              then return Nothing
              else go parent

parseConfig :: String -> Either AppException Config
parseConfig = mapLeft convertException . eitherDecode . cs
  where
    convertException msg = ConfigException $ "Parse error: " ++ msg

prettyEncodeConfig :: Config -> LBS.ByteString
prettyEncodeConfig config =
  let prettyConfig = P.defConfig { P.confIndent = 2, P.confCompare = compare }
  in  P.encodePretty' prettyConfig config

-- For easier to read maps
(~>) :: a -> b -> (a, b)
(~>) = (,)
