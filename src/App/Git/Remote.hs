{-# LANGUAGE OverloadedStrings #-}

module App.Git.Remote where

import           App.Git.Core

import           Control.Monad           (void)
import           Data.String.Conversions (cs)

fetch :: String -> GitM ()
fetch remote = void $ git "fetch" [cs remote]
