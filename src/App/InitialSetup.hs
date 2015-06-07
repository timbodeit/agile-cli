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
import           Data.String.Conversions
import           Jira.API
import           Network.HTTP.Client
import           System.Process
import           Web.Authenticate.OAuth

doInitSetup :: IO ()
doInitSetup = do
  config <- do existingConfig <- readConfig'
               case existingConfig of
                 Left _       -> doSetupConfigInteractively
                 Right (_, c) -> return c
  doInitAuth config >>= maybe handleAuthError writeConfigToLocalDir
  where
    writeConfigToLocalDir = LBS.writeFile configFileName . prettyEncodeConfig
    handleAuthError = putStrLn "Authentication error"

doSetupConfigInteractively :: IO Config
doSetupConfigInteractively = do
  ask "JIRA Base URL? > "
  baseUrl <- getLine'
  ask "JIRA username? > "
  username <- getLine'
  ask "JIRA project key? > "
  project <- getLine'
  ask "JIRA OAuth consumer key? > "
  consumerKey <- getLine'
  ask "JIRA OAuth signing key? > "
  signingKey <- getLine'
  ask "Git develop branch name? > "
  developBranch <- getLine'
  ask "Command to open URLs? (e.g. open on OS X) > "
  openCommand <- getLine'

  return $ Config baseUrl
                  username
                  project
                  developBranch
                  openCommand
                  consumerKey
                  signingKey
                  "" -- Access Token (unknown yet)
                  "" -- Access Token Secret (unknown yet)

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
