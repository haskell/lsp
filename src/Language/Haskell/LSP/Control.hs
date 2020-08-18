{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.LSP.Control
  (
    run
  , runWith
  , runWithHandles
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
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

-- | Convenience function for 'runWith' using the specified handles.
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

  hSetBuffering hin NoBuffering
  hSetEncoding  hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding  hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BSL.hPut hout out
      hFlush hout

  runWith clientIn clientOut initializeCallbacks h o

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
    -> IO Int         -- exit code
runWith clientIn clientOut initializeCallbacks h o = do

  logm $ B.pack "\n\n\n\n\nhaskell-lsp:Starting up server ..."

  cout <- atomically newTChan :: IO (TChan J.Value)
  _rhpid <- forkIO $ sendServer cout clientOut

  let sendMsg msg = atomically $ writeTChan cout $ J.toJSON msg

  initVFS $ \vfs -> do
    ioLoop clientIn initializeCallbacks vfs h o sendMsg

  return 1

-- ---------------------------------------------------------------------

ioLoop
  :: Show config
  => IO BS.ByteString
  -> Core.InitializeCallbacks config
  -> VFS
  -> Core.Handlers
  -> Core.Options
  -> (Core.FromServerMessage -> IO ())
  -> IO ()
ioLoop clientIn initializeCallbacks vfs h o sendMsg = do
  minitialize <- parseOne (parse parser "")
  case minitialize of
    Nothing -> pure ()
    Just (msg,remainder) -> do
      case J.eitherDecode $ BSL.fromStrict msg of
        Left err ->
          logm $ B.pack
            "\nhaskell-lsp: Got error while decoding initialize:\n" <> str2lbs err <> "\n exiting 1 ...\n"
        Right initialize -> do
          mInitResp <- Core.initializeRequestHandler initializeCallbacks vfs h o sendMsg initialize
          case mInitResp of
            Nothing -> pure ()
            Just env -> loop env (parse parser remainder)
  where

    parseOne :: Result BS.ByteString -> IO (Maybe (BS.ByteString,BS.ByteString))
    parseOne (Fail _ ctxs err) = do
      logm $ B.pack
        "\nhaskell-lsp: Failed to parse message header:\n" <> B.intercalate " > " (map str2lbs ctxs) <> ": " <>
        str2lbs err <> "\n exiting 1 ...\n"
      pure Nothing
    parseOne (Partial c) = do
      bs <- clientIn
      if BS.null bs
        then do
          logm $ B.pack "\nhaskell-lsp:Got EOF, exiting 1 ...\n"
          pure Nothing
        else parseOne (c bs)
    parseOne (Done remainder msg) = do
      logm $ B.pack "---> " <> BSL.fromStrict msg
      pure $ Just (msg,remainder)

    loop env = go
      where
        go r = do
          res <- parseOne r
          case res of
            Nothing -> pure ()
            Just (msg,remainder) -> do
              Core.runReaderT (Core.handleMessage $ BSL.fromStrict msg) env
              go (parse parser remainder)

    parser = do
      _ <- string "Content-Length: "
      len <- decimal
      _ <- string _TWO_CRLF
      Attoparsec.take len

-- ---------------------------------------------------------------------

-- | Simple server to make sure all output is serialised
sendServer :: TChan J.Value -> (BSL.ByteString -> IO ()) -> IO ()
sendServer msgChan clientOut = do
  forever $ do
    msg <- atomically $ readTChan msgChan

    -- We need to make sure we only send over the content of the message,
    -- and no other tags/wrapper stuff
    let str = J.encode msg

    let out = BSL.concat
                 [ str2lbs $ "Content-Length: " ++ show (BSL.length str)
                 , BSL.fromStrict _TWO_CRLF
                 , str ]

    clientOut out
    logm $ B.pack "<--2--" <> str

-- |
--
--
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"


