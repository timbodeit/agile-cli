{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App.InitialSetup ( doInitSetup
                        , doSetupConfigFromScratch
                        , doSetupFromExistingConfig
                        , doSetupConfigInteractively
                        , runWizard
                        ) where

import           App.Config
import           App.ConfigBuilder
import           App.Types
import           App.Util
import           Control.Monad.Trans.Either

import           Control.Applicative
import           Control.Lens
import           Crypto.Types.PubKey.RSA    (PrivateKey (..))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.List
import           Data.String.Conversions
import qualified Data.Text                  as T
import qualified Jira.API                   as J
import           Network.HTTP.Client        hiding (path)
import           System.Directory
import           Web.Authenticate.OAuth

doInitSetup :: IO ()
doInitSetup =
  readConfig' >>= \case
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
      putStrLn $ "Config written to " ++ preferredConfigFileName ++ "."
      putStrLn $ "Adjust the config file to your needs and call 'agile init' " ++
                 "again to initialize the JIRA authentication."

runWizard :: IO ()
runWizard = do
  config <- doSetupConfigInteractively
  writeConfigToLocalDir config

  putStrLn ""
  putStrLn "Checking your config..."
  doInitAuth config >>=
    either handleAppException writeConfigToLocalDir

doSetupConfigInteractively :: IO Config
doSetupConfigInteractively = do
  jiraBaseUrl       <- ask "JIRA Base URL?"
  jiraUsername      <- ask "JIRA username?"
  jiraProject       <- ask "JIRA project key?"
  jiraConsumerKey   <- ask "JIRA OAuth consumer key?"
  jiraSigningKey    <- ask "JIRA OAuth signing key?"

  stashBaseUrl      <- ask "Stash Base URL?"
  stashProject      <- ask "Stash project key?"
  stashRepo         <- ask "Stash repo name?"
  stashReviewers    <- ask "Stash reviewers (comma-separated)?" >$< parseCommaList

  developBranch     <- askWithDefault "develop" "Git develop branch name?"
  remoteName        <- askWithDefault "origin"  "Name of the remote?"
  openCommand       <- askWithDefault "open"    "Command to open URLs? (e.g. open on OS X)"

  let jiraConfig = JiraConfig jiraBaseUrl
                              jiraUsername
                              jiraProject
                              "Close Issue" -- Finish with merge transition
                              defaultIssueTypeMap
                              defaultSearchAliases
                              jiraConsumerKey
                              jiraSigningKey
                              "" -- Access Token (unknown yet)
                              "" -- Access Token Secret (unknown yet)

  let stashConfig = StashConfig stashBaseUrl
                                stashProject
                                stashRepo
                                stashReviewers

  return $ Config jiraConfig
                  stashConfig
                  developBranch
                  remoteName
                  openCommand
  where
    parseCommaList = map (trim . cs) . T.splitOn "," . cs

doSetupFromExistingConfig :: (FilePath, Config) -> IO ()
doSetupFromExistingConfig (configPath, config) =
  if isAuthConfigured config
  then let availableAnswers = if configPath == preferredConfigFileName
                              then drop 1 answers
                              else answers
       in runUserChoice question availableAnswers
  else doInitAuth config >>= either handleAppException (writeConfigTo configPath)
  where
    question = unlines'
      [ "Config file with authentication info found at " ++ configPath
      , "What do you want to do?"
      ]
    answers = zip
      [ "Copy config to local directory"
      , "Create a new config in the local directory using the wizard"
      , "Re-authenticate with JIRA"
      ]
      [ copyFile configPath preferredConfigFileName
      , runWizard
      , doInitAuth config >>= either handleAppException (writeConfigTo configPath)
      ]

doInitAuth :: Config -> AppIO Config
doInitAuth config = runEitherT $ do
  privateKey  <- EitherT . readPrivateKey $ config^.configJiraConfig.jiraOAuthSigningKeyPath
  accessToken <- EitherT $ doGetAccessToken privateKey
  return $ uncurry updateConfig accessToken
  where
    updateConfig :: BS.ByteString -> BS.ByteString -> Config
    updateConfig token tokenSecret =
      config & configJiraConfig.jiraOAuthAccessToken  .~ cs token
             & configJiraConfig.jiraOAuthAccessSecret .~ cs tokenSecret

    doGetAccessToken :: PrivateKey -> AppIO (BS.ByteString, BS.ByteString)
    doGetAccessToken pk = do
      manager <- newManager defaultManagerSettings
      let oauth = J.getOAuth (config^.configJiraConfig.jiraBaseUrl)
                  (config^.configJiraConfig.jiraOAuthConsumerKey)
                  pk
      requestToken <- getTemporaryCredential oauth manager
      let authUrl = authorizeUrl oauth requestToken
      putStrLn "Please open the following link in your browser and log in."
      putStrLn "Hit enter when you are finished."
      openInBrowser' authUrl config
      print authUrl
      _ <- getChar
      accessToken <- getAccessToken oauth requestToken manager
      return $ toEither (ConfigException "Failed to extract access token")
                        (extractAccessToken accessToken)

    extractAccessToken :: Credential -> Maybe (BS.ByteString, BS.ByteString)
    extractAccessToken credential =
      let tokenMap = unCredential credential
          token =  snd <$> find (keyEquals "oauth_token") tokenMap
          secret = snd <$> find (keyEquals "oauth_token_secret") tokenMap
      in  (,) <$> token <*> secret

    keyEquals :: Eq a => a -> (a, b) -> Bool
    keyEquals r (a, _) = a == r

isAuthConfigured :: Config -> Bool
isAuthConfigured config =
     config^.configJiraConfig.jiraOAuthAccessToken  /= ""
  && config^.configJiraConfig.jiraOAuthAccessSecret /= ""

writeConfigTo :: FilePath -> Config -> IO ()
writeConfigTo path = LBS.writeFile path . prettyEncodeConfig

writeConfigToLocalDir :: Config -> IO ()
writeConfigToLocalDir = writeConfigTo preferredConfigFileName

handleAppException :: AppException -> IO ()
handleAppException (ConfigException s) = putStrLn s
handleAppException e = print e
