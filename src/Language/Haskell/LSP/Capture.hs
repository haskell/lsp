{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Capture (captureFromServer, captureFromClient) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics
import Language.Haskell.LSP.Messages

data Event = FromClient UTCTime FromClientMessage
           | FromServer UTCTime FromServerMessage
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

captureFromServer :: FromServerMessage -> Maybe FilePath -> IO ()
captureFromServer _ Nothing = return ()
captureFromServer msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromServer time msg
  
  BSL.appendFile fp $ BSL.append (encode entry) "\n"

captureFromClient :: FromClientMessage -> Maybe FilePath -> IO ()
captureFromClient _ Nothing = return ()
captureFromClient msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromClient time msg
  
  BSL.appendFile fp $ BSL.append (encode entry) "\n"
