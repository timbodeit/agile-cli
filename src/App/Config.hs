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
import           Data.String.Conversions
import           Jira.API
import           System.Directory
import           System.FilePath
import           System.Posix.Directory

configFileName :: FilePath
configFileName = ".agile"

defaultConfig :: Config
defaultConfig = Config { _configBaseUrl = "http://jira.example.com"
                       , _configUsername = "myusername"
                       , _configProject = "MAP"
                       , _configDevelopBranch = "develop"
                       , _configBrowserCommand = "open"
                       , _configOAuthConsumerKey = "agile-cli"
                       , _configOAuthSigningKeyPath = "/path/to/key.pem"
                       , _configOAuthAccessToken = ""
                       , _configOAuthAccessSecret = ""
                       }

getJiraConfig :: Config -> IO JiraConfig
getJiraConfig c = do
  pk <- readPemPrivateKey =<< readFile (c^.configOAuthSigningKeyPath)
  let authConfig = OAuthConfig (c^.configOAuthConsumerKey)
                               pk
                               (c^.configOAuthAccessToken)
                               (c^.configOAuthAccessSecret)

  return $ JiraConfig (c^.configBaseUrl) authConfig

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
parseConfig = toEither parseException . decode . cs
  where
    toEither :: e -> Maybe r -> Either e r
    toEither e = maybe (Left e) Right

    parseException :: AppException
    parseException = ConfigException "Error while parsing config file"

prettyEncodeConfig :: Config -> LBS.ByteString
prettyEncodeConfig config =
  let prettyConfig = P.defConfig { P.confIndent = 2, P.confCompare = compare }
  in  P.encodePretty' prettyConfig config
