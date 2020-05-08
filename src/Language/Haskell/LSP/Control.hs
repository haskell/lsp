{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.LSP.Control
  (
    run
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
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Utility
import           System.IO

-- ---------------------------------------------------------------------

-- | Convenience function for 'runWithHandles stdin stdout'.
run :: (Show configs) => Core.InitializeCallbacks configs
                -- ^ function to be called once initialize has
                -- been received from the client. Further message
                -- processing will start only after this returns.
    -> Core.Handlers
    -> Core.Options
    -- ^ File to capture the session to.
    -> IO Int
run = runWithHandles stdin stdout

-- | Starts listening and sending requests and responses
-- at the specified handles.
runWithHandles :: (Show config) =>
       Handle
    -- ^ Handle to read client input from.
    -> Handle
    -- ^ Handle to write output to.
    -> Core.InitializeCallbacks config
    -> Core.Handlers
    -> Core.Options
    -> IO Int         -- exit code
runWithHandles hin hout initializeCallbacks h o = do

  logm $ B.pack "\n\n\n\n\nhaskell-lsp:Starting up server ..."
  hSetBuffering hin NoBuffering
  hSetEncoding  hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding  hout utf8

  cout <- atomically newTChan :: IO (TChan J.Value)
  _rhpid <- forkIO $ sendServer cout hout


  let sendFunc :: Core.SendFunc
      sendFunc msg = atomically $ writeTChan cout $ J.toJSON msg
  let lf = error "LifeCycle error, ClientCapabilities not set yet via initialize maessage"

  tvarId <- atomically $ newTVar 0

  initVFS $ \vfs -> do
    tvarDat <- atomically $ newTVar $ Core.defaultLanguageContextData h o lf tvarId sendFunc vfs

    ioLoop hin initializeCallbacks tvarDat

  return 1

-- ---------------------------------------------------------------------

ioLoop :: (Show config) => Handle
                   -> Core.InitializeCallbacks config
                   -> TVar (Core.LanguageContextData config)
                   -> IO ()
ioLoop hin dispatcherProc tvarDat =
  go (parse parser "")
  where
    go :: Result BS.ByteString -> IO ()
    go (Fail _ ctxs err) = logm $ B.pack
      "\nhaskell-lsp: Failed to parse message header:\n" <> B.intercalate " > " (map str2lbs ctxs) <> ": " <>
      str2lbs err <> "\n exiting 1 ...\n"
    go (Partial c) = do
      bs <- BS.hGetSome hin defaultChunkSize
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
sendServer :: TChan J.Value -> Handle -> IO ()
sendServer msgChan clientH = do
  forever $ do
    msg <- atomically $ readTChan msgChan

    -- We need to make sure we only send over the content of the message,
    -- and no other tags/wrapper stuff
    let str = J.encode msg

    let out = BSL.concat
                 [ str2lbs $ "Content-Length: " ++ show (BSL.length str)
                 , BSL.fromStrict _TWO_CRLF
                 , str ]

    BSL.hPut clientH out
    hFlush clientH
    logm $ B.pack "<--2--" <> str

-- |
--
--
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"


