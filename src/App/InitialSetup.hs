{-# LANGUAGE OverloadedStrings #-}

module App.InitialSetup where

import           App.Config
import           App.Types
import           App.Util

import           Control.Lens
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Either.Combinators
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Jira.API
import           Network.HTTP.Client
import           System.Directory
import           System.Process
import           Text.Read
import           Web.Authenticate.OAuth

doInitSetup :: IO ()
doInitSetup = do
  existingConfig <- readConfig'
  case existingConfig of
    Left _  -> doSetupConfigFromScratch
    Right c -> doSetupFromExistingConfig c

doSetupConfigFromScratch :: IO ()
doSetupConfigFromScratch =
  runUserChoice
    "Please choose your preferred option to initialize your agile config:" $
    zip [ "Create a config file with default settings to adjust manually"
        , "Use the command-line wizard"
        ]
        [ dumpDefaultSettings
        , runWizard
        ]
  where
    dumpDefaultSettings = do
      writeConfigToLocalDir defaultConfig
      putStrLn $ "Config written to " ++ configFileName ++ "."
      putStrLn $ "Adjust the config file to your needs and call 'agile init'" ++
                 "again to initialize the JIRA authentication."

runWizard :: IO ()
runWizard = do
  config <- doSetupConfigInteractively
  writeConfigToLocalDir config
  doInitAuth config >>=
    maybe handleAuthError writeConfigToLocalDir

doSetupConfigInteractively :: IO Config
doSetupConfigInteractively = do
  baseUrl       <- ask "JIRA Base URL?"
  username      <- ask "JIRA username?"
  project       <- ask "JIRA project key?"
  consumerKey   <- ask "JIRA OAuth consumer key?"
  signingKey    <- ask "JIRA OAuth signing key?"
  developBranch <- ask "Git develop branch name?"
  openCommand   <- ask "Command to open URLs? (e.g. open on OS X)"

  return $ Config baseUrl
                  username
                  project
                  developBranch
                  openCommand
                  consumerKey
                  signingKey
                  "" -- Access Token (unknown yet)
                  "" -- Access Token Secret (unknown yet)

doSetupFromExistingConfig :: (FilePath, Config) -> IO ()
doSetupFromExistingConfig (configPath, config) =
  if (config^.configOAuthAccessToken) == "" || (config^.configOAuthAccessSecret == "")
  then doInitAuth config >>= maybe handleAuthError (writeConfigTo configPath)
  else
    let availableAnswers = if configPath == configFileName
                           then drop 1 answers
                           else answers
    in runUserChoice question availableAnswers
  where
    question = unlines
      [ "Config file with authentication info found at " ++ configPath
      , "What do you want to do?"
      ]
    answers = zip
      [ "Copy config to local directory"
      , "Create a new config in the local directory using the wizard"
      , "Re-authenticate with JIRA"
      ]
      [ copyFile configPath configFileName
      , runWizard
      , doInitAuth config >>= maybe handleAuthError (writeConfigTo configPath)
      ]

doInitAuth :: Config -> IO (Maybe Config)
doInitAuth config =
  doGetAccessToken >$$< \(token, tokenSecret) ->
      config & configOAuthAccessToken  .~ cs token
             & configOAuthAccessSecret .~ cs tokenSecret
  where
    doGetAccessToken :: IO (Maybe (BS.ByteString, BS.ByteString))
    doGetAccessToken = do
      manager <- newManager defaultManagerSettings
      pk <- readPemPrivateKey =<< readFile (config^.configOAuthSigningKeyPath)
      let oauth = getOAuth (config^.configBaseUrl)
                           (config^.configOAuthConsumerKey)
                           pk
      requestToken <- getTemporaryCredential oauth manager
      let authUrl = authorizeUrl oauth requestToken
      putStrLn "Please open the following link in your browser and log in."
      putStrLn "Hit enter when you are finished."
      openInBrowser' authUrl config
      print authUrl
      getChar
      accessToken <- getAccessToken oauth requestToken manager
      return $ extractAccessToken accessToken

    extractAccessToken :: Credential -> Maybe (BS.ByteString, BS.ByteString)
    extractAccessToken credential =
      let tokenMap = unCredential credential
          token =  view _2 <$> find (keyEquals "oauth_token") tokenMap
          secret = view _2 <$> find (keyEquals "oauth_token_secret") tokenMap
      in  (,) <$> token <*> secret

    keyEquals :: Eq a => a -> (a, b) -> Bool
    keyEquals r (a, _) = a == r

writeConfigTo :: FilePath -> Config -> IO ()
writeConfigTo path = LBS.writeFile path . prettyEncodeConfig

writeConfigToLocalDir :: Config -> IO ()
writeConfigToLocalDir = writeConfigTo configFileName

handleAuthError :: IO ()
handleAuthError = putStrLn "Authentication error. Please try again."
