{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Control
  (
    run
  , runWith
  , runWithHandles
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Time.Clock
import           Data.Time.Format
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Language.Haskell.LSP.Capture
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Utility
import           System.IO
import           System.FilePath

-- ---------------------------------------------------------------------

-- | Convenience function for 'runWithHandles stdin stdout'.
run :: (Show configs) => Core.InitializeCallbacks configs
                -- ^ function to be called once initialize has
                -- been received from the client. Further message
                -- processing will start only after this returns.
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -- ^ File to capture the session to.
    -> IO Int
run = runWithHandles stdin stdout

-- | Convenience function for 'runWith' using the specified handles.
runWithHandles :: (Show config) =>
       Handle
    -- ^ Handle to read client input from.
    -> Handle
    -- ^ Handle to write output to.
    -> Core.InitializeCallbacks config
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -> IO Int         -- exit code
runWithHandles hin hout initializeCallbacks h o captureFp = do
  hSetBuffering hin NoBuffering
  hSetEncoding  hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding  hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BSL.hPut hout out
      hFlush hout

  runWith clientIn clientOut initializeCallbacks h o captureFp

-- | Starts listening and sending requests and responses
-- using the specified I/O.
runWith :: (Show config) =>
       IO BS.ByteString
    -- ^ Client input.
    -> (BSL.ByteString -> IO ())
    -- ^ Function to provide output to.
    -> Core.InitializeCallbacks config
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -> IO Int         -- exit code
runWith clientIn clientOut initializeCallbacks h o captureFp = do

  logm $ B.pack "\n\n\n\n\nhaskell-lsp:Starting up server ..."

  timestamp <- formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%S")) <$> getCurrentTime
  let timestampCaptureFp = fmap (\f -> dropExtension f ++ timestamp ++ takeExtension f)
                                captureFp
  captureCtx <- maybe (return noCapture) captureToFile timestampCaptureFp

  cout <- atomically newTChan :: IO (TChan FromServerMessage)
  _rhpid <- forkIO $ sendServer cout clientOut captureCtx


  let sendFunc :: Core.SendFunc
      sendFunc msg = atomically $ writeTChan cout msg
  let lf = error "LifeCycle error, ClientCapabilities not set yet via initialize maessage"

  tvarId <- atomically $ newTVar 0
  initVFS $ \vfs -> do
    tvarDat <- atomically $ newTVar $ Core.defaultLanguageContextData h o lf tvarId sendFunc captureCtx vfs

    ioLoop clientIn initializeCallbacks tvarDat

  return 1


-- ---------------------------------------------------------------------

ioLoop :: (Show config) => IO BS.ByteString
                   -> Core.InitializeCallbacks config
                   -> TVar (Core.LanguageContextData config)
                   -> IO ()
ioLoop clientIn dispatcherProc tvarDat =
  go (parse parser "")
  where
    go :: Result BS.ByteString -> IO ()
    go (Fail _ ctxs err) = logm $ B.pack
      "\nhaskell-lsp: Failed to parse message header:\n" <> B.intercalate " > " (map str2lbs ctxs) <> ": " <>
      str2lbs err <> "\n exiting 1 ...\n"
    go (Partial c) = do
      bs <- clientIn
      if BS.null bs
        then logm $ B.pack "\nhaskell-lsp:Got EOF, exiting 1 ...\n"
        else go (c bs)
    go (Done remainder msg) = do
      logm $ B.pack "---> " <> BSL.fromStrict msg
      Core.handleMessage dispatcherProc tvarDat (BSL.fromStrict msg)
      go (parse parser remainder)
    parser = do
      _ <- string "Content-Length: "
      len <- decimal
      _ <- string _TWO_CRLF
      Attoparsec.take len

-- ---------------------------------------------------------------------

-- | Simple server to make sure all output is serialised
sendServer :: TChan FromServerMessage -> (BSL.ByteString -> IO ()) -> CaptureContext -> IO ()
sendServer msgChan clientOut captureCtxt =
  forever $ do
    msg <- atomically $ readTChan msgChan

    -- We need to make sure we only send over the content of the message,
    -- and no other tags/wrapper stuff
    let str = J.encode $
                J.genericToJSON (J.defaultOptions { J.sumEncoding = J.UntaggedValue }) msg

    let out = BSL.concat
                 [ str2lbs $ "Content-Length: " ++ show (BSL.length str)
                 , BSL.fromStrict _TWO_CRLF
                 , str ]

    clientOut out
    logm $ B.pack "<--2--" <> str

    captureFromServer msg captureCtxt

-- |
--
--
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"


