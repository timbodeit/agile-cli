{-# LANGUAGE OverloadedStrings #-}

module App.Git.WorkingCopy where

import           App.Git.Core

import           Control.Monad.Except    (throwError)
import           Data.String.Conversions (cs)

data WorkingCopyStatus = Clean
                       | Dirty
                       deriving (Show, Eq)

workingCopyStatus :: GitM WorkingCopyStatus
workingCopyStatus = do
  (_, err, code) <- git' "diff" ["--quiet", "HEAD"]
  case code of
    0 -> return Clean
    1 -> return Dirty
    _ -> throwError . GitException $ cs err

