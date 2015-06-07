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
import           Jira.API                   hiding (getConfig)

data Config = Config { _configBaseUrl             :: String
                     , _configUsername            :: String
                     , _configProject             :: String
                     , _configDevelopBranch       :: String
                     , _configBrowserCommand      :: String
                     , _configOAuthConsumerKey    :: String
                     , _configOAuthSigningKeyPath :: String
                     , _configOAuthAccessToken    :: String
                     , _configOAuthAccessSecret   :: String
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
