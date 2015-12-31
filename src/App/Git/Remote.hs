{-# LANGUAGE OverloadedStrings #-}

module App.Git.Remote where

import           App.Git.Core
import           App.Util

import           Control.Lens
import           Control.Monad.Except
import           Data.Maybe              (mapMaybe)
import           Data.String.Conversions (cs)

fetch :: String -> GitM ()
fetch remote = void $ git "fetch" [cs remote]

remoteUrl :: String -> GitM String
remoteUrl remote = do
  output <- git "remote" ["-v"]
  let candidates = mapMaybe extractUrl $
                   filter lineCorrespondsToRemote $
                   lines (cs output)

  liftMaybe urlException $ safeHead candidates
  where
    lineCorrespondsToRemote line =
      maybe False (== remote) $ safeHead (words line)

    extractUrl line = line ^? to words . ix 1

    urlException = GitException $ "Cannot get URL of remote: '" ++ remote ++ "''"

    safeHead []    = Nothing
    safeHead (x:_) = Just x
