{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Language.LSP.Server.Control (
  -- * Running
  runServer,
  runServerWith,
  runServerWithHandles,
  LspServerLog (..),
) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson qualified as J
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.Attoparsec.ByteString.Char8
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.ByteString.Lazy qualified as BSL
import Data.List
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Prettyprint.Doc
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Message
import Language.LSP.Server.Core
import Language.LSP.Server.Processing qualified as Processing
import Language.LSP.VFS
import System.IO

data LspServerLog
  = LspProcessingLog Processing.LspProcessingLog
  | DecodeInitializeError String
  | HeaderParseFail [String] String
  | EOF
  | Starting
  | ParsedMsg T.Text
  | SendMsg TL.Text
  deriving (Show)

instance Pretty LspServerLog where
  pretty (LspProcessingLog l) = pretty l
  pretty (DecodeInitializeError err) =
    vsep
      [ "Got error while decoding initialize:"
      , pretty err
      ]
  pretty (HeaderParseFail ctxs err) =
    vsep
      [ "Failed to parse message header:"
      , pretty (intercalate " > " ctxs) <> ": " <+> pretty err
      ]
  pretty EOF = "Got EOF"
  pretty Starting = "Starting server"
  pretty (ParsedMsg msg) = "---> " <> pretty msg
  pretty (SendMsg msg) = "<--2-- " <> pretty msg

-- ---------------------------------------------------------------------

{- | Convenience function for 'runServerWithHandles' which:
     (1) reads from stdin;
     (2) writes to stdout; and
     (3) logs to stderr and to the client, with some basic filtering.
-}
runServer :: forall config. ServerDefinition config -> IO Int
runServer =
  runServerWithHandles
    ioLogger
    lspLogger
    stdin
    stdout
 where
  prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)
  ioLogger :: LogAction IO (WithSeverity LspServerLog)
  ioLogger = L.cmap (show . prettyMsg) L.logStringStderr
  lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
  lspLogger =
    let clientLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger
     in clientLogger <> L.hoistLogAction liftIO ioLogger

{- | Starts a language server over the specified handles.
 This function will return once the @exit@ notification is received.
-}
runServerWithHandles ::
  -- | The logger to use outside the main body of the server where we can't assume the ability to send messages.
  LogAction IO (WithSeverity LspServerLog) ->
  -- | The logger to use once the server has started and can successfully send messages.
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  -- | Handle to read client input from.
  Handle ->
  -- | Handle to write output to.
  Handle ->
  ServerDefinition config ->
  IO Int -- exit code
runServerWithHandles ioLogger logger hin hout serverDefinition = do
  hSetBuffering hin NoBuffering
  hSetEncoding hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BSL.hPut hout out
      hFlush hout

  runServerWith ioLogger logger clientIn clientOut serverDefinition

{- | Starts listening and sending requests and responses
 using the specified I/O.
-}
runServerWith ::
  -- | The logger to use outside the main body of the server where we can't assume the ability to send messages.
  LogAction IO (WithSeverity LspServerLog) ->
  -- | The logger to use once the server has started and can successfully send messages.
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  -- | Client input.
  IO BS.ByteString ->
  -- | Function to provide output to.
  (BSL.ByteString -> IO ()) ->
  ServerDefinition config ->
  IO Int -- exit code
runServerWith ioLogger logger clientIn clientOut serverDefinition = do
  ioLogger <& Starting `WithSeverity` Info

  cout <- atomically newTChan :: IO (TChan J.Value)
  _rhpid <- forkIO $ sendServer ioLogger cout clientOut

  let sendMsg msg = atomically $ writeTChan cout $ J.toJSON msg

  ioLoop ioLogger logger clientIn serverDefinition emptyVFS sendMsg

  return 1

-- ---------------------------------------------------------------------

ioLoop ::
  forall config.
  LogAction IO (WithSeverity LspServerLog) ->
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  IO BS.ByteString ->
  ServerDefinition config ->
  VFS ->
  (FromServerMessage -> IO ()) ->
  IO ()
ioLoop ioLogger logger clientIn serverDefinition vfs sendMsg = do
  minitialize <- parseOne ioLogger clientIn (parse parser "")
  case minitialize of
    Nothing -> pure ()
    Just (msg, remainder) -> do
      case J.eitherDecode $ BSL.fromStrict msg of
        Left err -> ioLogger <& DecodeInitializeError err `WithSeverity` Error
        Right initialize -> do
          mInitResp <- Processing.initializeRequestHandler pioLogger serverDefinition vfs sendMsg initialize
          case mInitResp of
            Nothing -> pure ()
            Just env -> runLspT env $ loop (parse parser remainder)
 where
  pioLogger = L.cmap (fmap LspProcessingLog) ioLogger
  pLogger = L.cmap (fmap LspProcessingLog) logger

  loop :: Result BS.ByteString -> LspM config ()
  loop = go
   where
    go r = do
      res <- parseOne logger clientIn r
      case res of
        Nothing -> pure ()
        Just (msg, remainder) -> do
          Processing.processMessage pLogger $ BSL.fromStrict msg
          go (parse parser remainder)

  parser = do
    try contentType <|> (return ())
    len <- contentLength
    try contentType <|> (return ())
    _ <- string _ONE_CRLF
    Attoparsec.take len

  contentLength = do
    _ <- string "Content-Length: "
    len <- decimal
    _ <- string _ONE_CRLF
    return len

  contentType = do
    _ <- string "Content-Type: "
    skipWhile (/= '\r')
    _ <- string _ONE_CRLF
    return ()

parseOne ::
  MonadIO m =>
  LogAction m (WithSeverity LspServerLog) ->
  IO BS.ByteString ->
  Result BS.ByteString ->
  m (Maybe (BS.ByteString, BS.ByteString))
parseOne logger clientIn = go
 where
  go (Fail _ ctxs err) = do
    logger <& HeaderParseFail ctxs err `WithSeverity` Error
    pure Nothing
  go (Partial c) = do
    bs <- liftIO clientIn
    if BS.null bs
      then do
        logger <& EOF `WithSeverity` Error
        pure Nothing
      else go (c bs)
  go (Done remainder msg) = do
    -- TODO: figure out how to re-enable
    -- This can lead to infinite recursion in logging, see https://github.com/haskell/lsp/issues/447
    -- logger <& ParsedMsg (T.decodeUtf8 msg) `WithSeverity` Debug
    pure $ Just (msg, remainder)

-- ---------------------------------------------------------------------

-- | Simple server to make sure all output is serialised
sendServer :: LogAction IO (WithSeverity LspServerLog) -> TChan J.Value -> (BSL.ByteString -> IO ()) -> IO ()
sendServer _logger msgChan clientOut = do
  forever $ do
    msg <- atomically $ readTChan msgChan

    -- We need to make sure we only send over the content of the message,
    -- and no other tags/wrapper stuff
    let str = J.encode msg

    let out =
          BSL.concat
            [ TL.encodeUtf8 $ TL.pack $ "Content-Length: " ++ show (BSL.length str)
            , BSL.fromStrict _TWO_CRLF
            , str
            ]

    clientOut out

-- TODO: figure out how to re-enable
-- This can lead to infinite recursion in logging, see https://github.com/haskell/lsp/issues/447
-- logger <& SendMsg (TL.decodeUtf8 str) `WithSeverity` Debug

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"
