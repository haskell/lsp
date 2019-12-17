{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Capture where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics

data Event = FromClient UTCTime Value
           | FromServer UTCTime Value
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

captureFromServer :: ToJSON a => a -> Maybe FilePath -> IO ()
captureFromServer _ Nothing = return ()
captureFromServer msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromServer time $ toJSON msg
  BSL.appendFile fp $ BSL.append (encode entry) "\n"

captureFromClient :: ToJSON a => a -> Maybe FilePath -> IO ()
captureFromClient _ Nothing = return ()
captureFromClient msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromClient time $ toJSON msg
  BSL.appendFile fp $ BSL.append (encode entry) "\n"
