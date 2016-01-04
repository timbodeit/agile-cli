{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module App.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson.TH
import qualified Data.Map                   as Map
import           Data.Typeable
import           Jira.API                   (JiraException (..))

data JiraConfig = JiraConfig
  { _jiraBaseUrl               :: String
  , _jiraUsername              :: String
  , _jiraProject               :: String
  , _jiraFinishMergeTransition :: String
  , _jiraIssueTypeAliases      :: Map.Map String String
  , _jiraSearchAliases         :: Map.Map String String
  , _jiraOAuthConsumerKey      :: String
  , _jiraOAuthSigningKeyPath   :: String
  , _jiraOAuthAccessToken      :: String
  , _jiraOAuthAccessSecret     :: String
  } deriving (Show, Eq)

makeLenses ''JiraConfig

$(deriveJSON defaultOptions { fieldLabelModifier = drop 5
                            , omitNothingFields = True
                            } ''JiraConfig)

data StashConfig = StashConfig
  { _stashBaseUrl    :: String
  , _stashProject    :: String
  , _stashRepository :: String
  , _stashReviewers  :: [String]
  } deriving (Show, Eq)

makeLenses ''StashConfig

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6
                            , omitNothingFields = True
                            } ''StashConfig)

data GithubConfig = GithubConfig
  { _githubUsername   :: Maybe String
  , _githubRepo       :: Maybe String
  , _githubOAuthToken :: String
  } deriving (Show, Eq)

makeLenses ''GithubConfig

$(deriveJSON defaultOptions { fieldLabelModifier = drop 7
                            , omitNothingFields = True
                            } ''GithubConfig)

data Config = Config
  { _configJiraConfig          :: Maybe JiraConfig
  , _configStashConfig         :: Maybe StashConfig
  , _configGithubConfig        :: Maybe GithubConfig
  , _configDevelopBranch       :: String
  , _configRemoteName          :: String
  , _configDefaultBranchPrefix :: String
  , _configBranchPrefixMap     :: Map.Map String String
  , _configBrowserCommand      :: String
  } deriving (Show, Eq)

makeLenses ''Config

$(deriveJSON defaultOptions { fieldLabelModifier = drop 7
                            , omitNothingFields = True
                            } ''Config)

data AppException = JiraApiException JiraException
                  | ConfigException String
                  | AuthException String
                  | UserInputException String
                  | GitException String
                  | IOException String
                  deriving (Show, Typeable)

instance Exception AppException

newtype AppM a = AppM { unAppM :: ReaderT (FilePath, Config) (EitherT AppException IO) a
                      } deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadReader (FilePath, Config)
                                 , MonadError AppException
                                 )

instance MonadIO AppM where
  liftIO m = (AppM . lift . lift $ try m) >>= either handleError return
    where
      handleError :: SomeException -> AppM a
      handleError e = throwError . IOException $ show e

runApp :: FilePath -> Config -> AppM a -> IO (Either AppException a)
runApp configPath config m =
  let unwrappedReader = runReaderT (unAppM m) (configPath, config)
  in  runEitherT unwrappedReader

getConfigPath :: AppM FilePath
getConfigPath = view _1 <$> ask

getConfig :: AppM Config
getConfig = view _2 <$> ask
