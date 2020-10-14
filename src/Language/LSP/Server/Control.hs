{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Server.Control
  (
  -- * Running
    runServer
  , runServerWith
  , runServerWithHandles
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
import           Language.LSP.Server.Core
import           Language.LSP.Server.Processing
import           Language.LSP.Types
import           Language.LSP.VFS
import           System.IO
import           System.Log.Logger


-- ---------------------------------------------------------------------

-- | Convenience function for 'runServerWithHandles stdin stdout'.
runServer :: ServerDefinition config
                -- ^ function to be called once initialize has
                -- been received from the client. Further message
                -- processing will start only after this returns.
    -> IO Int
runServer = runServerWithHandles stdin stdout

-- | Starts a language server over the specified handles. 
-- This function will return once the @exit@ notification is received.
runServerWithHandles ::
       Handle
    -- ^ Handle to read client input from.
    -> Handle
    -- ^ Handle to write output to.
    -> ServerDefinition config
    -> IO Int         -- exit code
runServerWithHandles hin hout serverDefinition = do

  hSetBuffering hin NoBuffering
  hSetEncoding  hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding  hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BSL.hPut hout out
      hFlush hout

  runServerWith clientIn clientOut serverDefinition

-- | Starts listening and sending requests and responses
-- using the specified I/O.
runServerWith ::
       IO BS.ByteString
    -- ^ Client input.
    -> (BSL.ByteString -> IO ())
    -- ^ Function to provide output to.
    -> ServerDefinition config
    -> IO Int         -- exit code
runServerWith clientIn clientOut serverDefinition = do

  infoM "haskell-lsp.runWith" "\n\n\n\n\nhaskell-lsp:Starting up server ..."

  cout <- atomically newTChan :: IO (TChan J.Value)
  _rhpid <- forkIO $ sendServer cout clientOut

  let sendMsg msg = atomically $ writeTChan cout $ J.toJSON msg

  initVFS $ \vfs -> do
    ioLoop clientIn serverDefinition vfs sendMsg

  return 1

-- ---------------------------------------------------------------------

ioLoop ::
     IO BS.ByteString
  -> ServerDefinition config
  -> VFS
  -> (FromServerMessage -> IO ())
  -> IO ()
ioLoop clientIn serverDefinition vfs sendMsg = do
  minitialize <- parseOne (parse parser "")
  case minitialize of
    Nothing -> pure ()
    Just (msg,remainder) -> do
      case J.eitherDecode $ BSL.fromStrict msg of
        Left err ->
          errorM "haskell-lsp.ioLoop" $
            "Got error while decoding initialize:\n" <> err <> "\n exiting 1 ...\n"
        Right initialize -> do
          mInitResp <- initializeRequestHandler serverDefinition vfs sendMsg initialize
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
              runLspT env $ processMessage $ BSL.fromStrict msg
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


