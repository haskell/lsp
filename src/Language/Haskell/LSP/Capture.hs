{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Capture
  ( Event(..)
  , CaptureContext
  , noCapture
  , captureToFile
  , captureFromClient
  , captureFromServer
  ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics
import Language.Haskell.LSP.Messages
import System.IO

data Event = FromClient UTCTime FromClientMessage
           | FromServer UTCTime FromServerMessage
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CaptureContext = NoCapture | CaptureToFile Handle

noCapture :: CaptureContext
noCapture = NoCapture

captureToFile :: FilePath -> IO CaptureContext
captureToFile fname = CaptureToFile <$> openFile fname WriteMode

captureFromServer :: FromServerMessage -> CaptureContext -> IO ()
captureFromServer _ NoCapture = return ()
captureFromServer msg (CaptureToFile fp) = do
  time <- getCurrentTime
  let entry = FromServer time msg

  BSL.hPutStrLn fp (encode entry)

captureFromClient :: FromClientMessage -> CaptureContext -> IO ()
captureFromClient _ NoCapture = return ()
captureFromClient msg (CaptureToFile fp) = do
  time <- getCurrentTime
  let entry = FromClient time msg

  BSL.hPutStrLn fp $ BSL.append (encode entry) "\n"
