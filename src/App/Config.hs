{-# LANGUAGE TupleSections #-}

module App.Config where

import           App.Types
import           App.Util

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as P
import qualified Data.ByteString.Lazy       as LBS
import           Data.Either.Combinators
import qualified Data.Map                   as Map
import           Data.String.Conversions
import qualified Jira.API                   as J
import           System.Directory
import           System.FilePath
import           System.Posix.Directory

configFileName :: FilePath
configFileName = ".agile"

defaultJiraConfig :: JiraConfig
defaultJiraConfig = JiraConfig
  { _jiraBaseUrl             = "http://jira.example.com"
  , _jiraUsername            = "myusername"
  , _jiraProject             = "MAP"
  , _jiraDefaultIssueType    = "Bug"
  , _jiraIssueTypeAliases    = defaultIssueTypeMap
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
  where (~>) = (,)

getJiraConfig :: Config -> IO J.JiraConfig
getJiraConfig c = do
  pk <- J.readPemPrivateKey =<< readFile (c^.configJiraConfig.jiraOAuthSigningKeyPath)
  let authConfig = J.OAuthConfig (c^.configJiraConfig.jiraOAuthConsumerKey)
                                 pk
                                 (c^.configJiraConfig.jiraOAuthAccessToken)
                                 (c^.configJiraConfig.jiraOAuthAccessSecret)

  return $ J.JiraConfig (c^.configJiraConfig.jiraBaseUrl) authConfig

readConfig :: FilePath -> IO (Either AppException Config)
readConfig path = runEitherT $ do
  rawConfig <- hoistEitherIO (mapLeft convertException <$> readConfigFile path)
  hoistEither $ parseConfig rawConfig
  where
    readConfigFile :: FilePath -> IO (Either SomeException String)
    readConfigFile = try . readFile

    convertException :: SomeException -> AppException
    convertException = ConfigException . show

readConfig' :: IO (Either AppException (FilePath, Config))
readConfig' = do
  path <- searchConfig
  case path of
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
