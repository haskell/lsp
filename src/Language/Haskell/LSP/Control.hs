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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.VFS
import           System.IO
import           System.Log.Logger

-- ---------------------------------------------------------------------

-- | Convenience function for 'runWithHandles stdin stdout'.
run :: (Show config) => Core.InitializeCallbacks config
                -- ^ function to be called once initialize has
                -- been received from the client. Further message
                -- processing will start only after this returns.
    -> Core.Handlers config
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
    -> Core.Handlers config
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
    -> Core.Handlers config
    -> Core.Options
    -> IO Int         -- exit code
runWith clientIn clientOut initializeCallbacks h o = do

  infoM "haskell-lsp.runWith" "\n\n\n\n\nhaskell-lsp:Starting up server ..."

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
  -> Core.Handlers config
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
          errorM "haskell-lsp.ioLoop" $
            "Got error while decoding initialize:\n" <> err <> "\n exiting 1 ...\n"
        Right initialize -> do
          mInitResp <- Core.initializeRequestHandler initializeCallbacks vfs h o sendMsg initialize
          case mInitResp of
            Nothing -> pure ()
            Just env -> loop env (parse parser remainder)
  where

    parseOne :: Result BS.ByteString -> IO (Maybe (BS.ByteString,BS.ByteString))
    parseOne (Fail _ ctxs err) = do
      errorM "haskell-lsp.parseOne" $
        "Failed to parse message header:\n" <> intercalate " > " ctxs <> ": " <>
        err <> "\n exiting 1 ...\n"
      pure Nothing
    parseOne (Partial c) = do
      bs <- clientIn
      if BS.null bs
        then do
          errorM "haskell-lsp.parseON" "haskell-lsp:Got EOF, exiting 1 ...\n"
          pure Nothing
        else parseOne (c bs)
    parseOne (Done remainder msg) = do
      debugM "haskell-lsp.parseOne" $ "---> " <> T.unpack (T.decodeUtf8 msg)
      pure $ Just (msg,remainder)

    loop env = go
      where
        go r = do
          res <- parseOne r
          case res of
            Nothing -> pure ()
            Just (msg,remainder) -> do
              Core.runReaderT (Core.runLspT (Core.processMessage $ BSL.fromStrict msg)) env
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
                [ TL.encodeUtf8 $ TL.pack $ "Content-Length: " ++ show (BSL.length str)
                , BSL.fromStrict _TWO_CRLF
                , str ]

    clientOut out
    debugM "haskell-lsp.sendServer" $ "<--2--" <> TL.unpack (TL.decodeUtf8 str)

-- |
--
--
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"


