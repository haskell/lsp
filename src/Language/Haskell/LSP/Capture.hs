{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Capture (captureFromServer, captureFromClient) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics

data Event a = FromClient UTCTime a
             | FromServer UTCTime a
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

captureFromServer :: ToJSON a => a -> FilePath -> IO ()
captureFromServer msg fp = do
  time <- getCurrentTime
  let entry = FromServer time msg
  
  BSL.appendFile fp $ BSL.append (encode entry) "\n"

captureFromClient :: ToJSON a => a -> FilePath -> IO ()
captureFromClient msg fp = do
  time <- getCurrentTime
  let entry = FromClient time msg
  
  BSL.appendFile fp $ BSL.append (encode entry) "\n"
