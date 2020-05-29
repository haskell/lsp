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
import Language.Haskell.LSP.Utility
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM

data Event = FromClient !UTCTime !FromClientMessage
           | FromServer !UTCTime !FromServerMessage
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CaptureContext = NoCapture | Capture (TChan Event)

noCapture :: CaptureContext
noCapture = NoCapture

captureToFile :: FilePath -> IO CaptureContext
captureToFile fname = do
    logs $ "haskell-lsp:Logging to " ++ fname
    chan <- newTChanIO
    _tid <- forkIO $ withFile fname WriteMode $ writeToHandle chan
    return $ Capture chan

captureFromServer :: FromServerMessage -> CaptureContext -> IO ()
captureFromServer _ NoCapture = return ()
captureFromServer msg (Capture chan) = do
  time <- getCurrentTime
  atomically $ writeTChan chan $ FromServer time msg

captureFromClient :: FromClientMessage -> CaptureContext -> IO ()
captureFromClient _ NoCapture = return ()
captureFromClient msg (Capture chan) = do
  time <- getCurrentTime
  atomically $ writeTChan chan $ FromClient time msg

writeToHandle :: TChan Event -> Handle -> IO ()
writeToHandle chan hdl = forever $ do
    ev <- atomically $ readTChan chan
    BSL.hPutStrLn hdl $ encode ev
