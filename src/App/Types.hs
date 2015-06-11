{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module App.Types where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Aeson.TH
import           Data.Typeable
import           Jira.API                   (JiraException (..))

data JiraConfig = JiraConfig
  { _jiraBaseUrl             :: String
  , _jiraUsername            :: String
  , _jiraProject             :: String
  , _jiraOAuthConsumerKey    :: String
  , _jiraOAuthSigningKeyPath :: String
  , _jiraOAuthAccessToken    :: String
  , _jiraOAuthAccessSecret   :: String
  } deriving (Show, Eq)

makeLenses ''JiraConfig

$(deriveJSON defaultOptions { fieldLabelModifier = drop 5
                            } ''JiraConfig)

data StashConfig = StashConfig
  { _stashBaseUrl    :: String
  , _stashProject    :: String
  , _stashRepository :: String
  } deriving (Show, Eq)

makeLenses ''StashConfig

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6
                            } ''StashConfig)

data Config = Config
  { _configJiraConfig     :: JiraConfig
  , _configStashConfig    :: StashConfig
  , _configDevelopBranch  :: String
  , _configBrowserCommand :: String
  } deriving (Show, Eq)

makeLenses ''Config

$(deriveJSON defaultOptions { fieldLabelModifier = drop 7
                            } ''Config)

data AppException = JiraApiException JiraException
                  | ConfigException String
                  | AuthException String
                  | UserInputException String
                  | GitException String
                  deriving (Show, Typeable)

instance Exception AppException

newtype AppM a = AppM { unAppM :: ReaderT (FilePath, Config) (EitherT AppException IO) a
                      } deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadReader (FilePath, Config)
                                 , MonadError AppException
                                 , MonadIO
                                 )

runApp :: FilePath -> Config -> AppM a -> IO (Either AppException a)
runApp configPath config m =
  let unwrappedReader = runReaderT (unAppM m) (configPath, config)
  in  runEitherT unwrappedReader

getConfigPath :: AppM FilePath
getConfigPath = view _1 <$> ask

getConfig :: AppM Config
getConfig = view _2 <$> ask
