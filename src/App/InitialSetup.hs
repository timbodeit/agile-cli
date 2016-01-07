{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App.InitialSetup (runInitialConfiguration) where

import           App.Config
import           App.ConfigBuilder
import           App.Types
import           App.Util

import           Control.Applicative
import           Control.Lens               hiding (set')
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Crypto.Types.PubKey.RSA    (PrivateKey (..))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.List
import           Data.Maybe                 (isJust)
import           Data.String.Conversions
import qualified Data.Text                  as T
import qualified Jira.API                   as J
import           Network.HTTP.Client        hiding (path)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           System.Directory
import           Web.Authenticate.OAuth

data ConfigurableBackend = Jira | Stash | Github deriving (Eq, Show)

runInitialConfiguration :: IO ()
runInitialConfiguration =
  readConfig' >>= \case
    Left _  -> runConfigWizardFromScratch
    Right c -> runConfigWizardFromExistingConfig c

runConfigWizardFromScratch :: IO ()
runConfigWizardFromScratch =
  runUserChoice
    "Please choose your preferred option to initialize your agile config:" $
    zip [ "Create a config file with default settings to adjust manually"
        , "Use the command-line wizard"
        ]
        [ dumpDefaultSettings
        , runInteractiveConfigWizard
        ]
  where
    dumpDefaultSettings = do
      writeConfigTo preferredConfigFileName defaultConfig
      putStrLn $ "Config written to " ++ preferredConfigFileName ++ "."
      putStrLn $ "Adjust the config file to your needs and call 'agile init' " ++
                 "again to initialize the JIRA authentication."

runInteractiveConfigWizard :: IO ()
runInteractiveConfigWizard = do
  config <- getConfigInteractively
  let configPath = preferredConfigFileName
  writeConfigTo configPath config

  putStrLn ""
  putStrLn $ "Config written to " ++ configPath

  when (shouldPerformJiraAuth config) $
    runJiraAuth config >>= either handleAppException (writeConfigTo preferredConfigFileName)

runConfigWizardFromExistingConfig :: (FilePath, Config) -> IO ()
runConfigWizardFromExistingConfig (configPath, config) =
  let availableAnswers = if configPath == preferredConfigFileName
                              then drop 1 answers
                              else answers
  in runUserChoice question availableAnswers
  where
    question = unlines'
      [ "Config file with authentication info found at " ++ configPath
      , "What do you want to do?"
      ]
    answers = zip
      [ "Copy config to local directory"
      , "Create a new config in the local directory using the wizard"
      , "Authenticate with JIRA"
      ]
      [ copyFile configPath preferredConfigFileName
      , runInteractiveConfigWizard
      , runJiraAuth config >>= either handleAppException (writeConfigTo configPath)
      ]

getConfigInteractively :: IO Config
getConfigInteractively = do
  developBranch <- askWithDefault "develop" "Git develop branch name?"
  remoteName    <- askWithDefault "origin"  "Name of the remote?"
  openCommand   <- askWithDefault "open"    "Command to open URLs? (e.g. open on OS X)"

  let intermediateConfigRef = defaultConfig { _configDevelopBranch  = developBranch
                                            , _configRemoteName     = remoteName
                                            , _configBrowserCommand = openCommand
                                            }

  backendsToConfigure <- nub <$> sequence [askIssueBackend, askPullRequestBackend]
  let configModifiers = map configureAndUpdate backendsToConfigure
  foldr (=<<) (return intermediateConfigRef) configModifiers
  where
    askIssueBackend = userChoice "Which issue backend would you like to use?"
      [("JIRA", Jira), ("Github", Github)]
    askPullRequestBackend = userChoice "Which pull request backend would you like to use?"
      [("Stash", Stash), ("Github", Github)]

getJiraConfigInteractively :: IO JiraConfig
getJiraConfigInteractively = do
  baseUrl     <- ask "JIRA Base URL?"
  username    <- ask "JIRA username?"
  project     <- ask "JIRA project key?"
  consumerKey <- ask "JIRA OAuth consumer key?"
  signingKey  <- ask "JIRA OAuth signing key?"

  return $ defaultJiraConfig { _jiraBaseUrl             = baseUrl
                             , _jiraUsername            = username
                             , _jiraProject             = project
                             , _jiraOAuthConsumerKey    = consumerKey
                             , _jiraOAuthSigningKeyPath = signingKey
                             }

getStashConfigInteractively :: IO StashConfig
getStashConfigInteractively = do
  baseUrl   <- ask "Stash Base URL?"
  project   <- ask "Stash project key?"
  repo      <- ask "Stash repo name?"
  reviewers <- ask "Stash reviewers (comma-separated)?" >$< parseCommaList

  return $ defaultStashConfig { _stashBaseUrl    = baseUrl
                              , _stashProject    = project
                              , _stashRepository = repo
                              , _stashReviewers  = reviewers
                              }
  where
    parseCommaList = map (trim . cs) . T.splitOn "," . cs

getGithubConfigInteractively :: IO GithubConfig
getGithubConfigInteractively = do
  oAuthToken <- ask "Github OAuth token?"
  return $ defaultGithubConfig { _githubOAuthToken = oAuthToken }

runJiraAuth :: Config -> AppIO Config
runJiraAuth config = runEitherT $ do
  liftIO $ putStrLn "Performing JIRA OAuth authentication..."

  jiraConfig  <- takeJiraConfig config
  privateKey  <- EitherT . readPrivateKey $ jiraConfig^.jiraOAuthSigningKeyPath
  accessToken <- EitherT $ doGetAccessToken jiraConfig privateKey
  return $ uncurry updateConfig accessToken
  where
    updateConfig :: BS.ByteString -> BS.ByteString -> Config
    updateConfig token tokenSecret =
      config & configJiraConfig._Just.jiraOAuthAccessToken  .~ cs token
             & configJiraConfig._Just.jiraOAuthAccessSecret .~ cs tokenSecret

    doGetAccessToken :: JiraConfig -> PrivateKey -> AppIO (BS.ByteString, BS.ByteString)
    doGetAccessToken jiraConfig pk = do
      manager <- newManager tlsManagerSettings
      let oauth = J.getOAuth (jiraConfig^.jiraBaseUrl)
                             (jiraConfig^.jiraOAuthConsumerKey)
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

isJiraAuthConfigured :: Config -> Bool
isJiraAuthConfigured config =
     config^.configJiraConfig._Just.jiraOAuthAccessToken  /= ""
  && config^.configJiraConfig._Just.jiraOAuthAccessSecret /= ""

shouldPerformJiraAuth :: Config -> Bool
shouldPerformJiraAuth config = isJust (config^.configJiraConfig) && not (isJiraAuthConfigured config)

writeConfigTo :: FilePath -> Config -> IO ()
writeConfigTo path = LBS.writeFile path . prettyEncode

handleAppException :: AppException -> IO ()
handleAppException (ConfigException s) = putStrLn s
handleAppException e = print e

configureAndUpdate :: ConfigurableBackend -> Config -> IO Config
configureAndUpdate Jira config   = set' config configJiraConfig   <$> getJiraConfigInteractively
configureAndUpdate Stash config  = set' config configStashConfig  <$> getStashConfigInteractively
configureAndUpdate Github config = set' config configGithubConfig <$> getGithubConfigInteractively

set' :: s -> ASetter s t a (Maybe b) -> b -> t
set' o k v = o & k ?~ v
