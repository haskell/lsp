{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.Control (
  -- * Running
  runServerWith,
  runServerWithConfig,
  ServerConfig (..),
  LspServerLog (..),

  -- ** Using standard 'IO' 'Handle's
  runServer,

  -- ** Using 'Handle's
  runServerWithHandles,
  prependHeader,
  parseHeaders,

  -- **  Using websockets
  WebsocketConfig (..),
  withWebsocket,
  withWebsocketRunServer,
) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Exception (finally)
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
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Message
import Language.LSP.Server.Core
import Language.LSP.Server.Processing qualified as Processing
import Language.LSP.VFS
import Network.WebSockets qualified as WS
import Prettyprinter
import System.IO

data LspServerLog
  = LspProcessingLog Processing.LspProcessingLog
  | DecodeInitializeError String
  | HeaderParseFail [String] String
  | EOF
  | Starting
  | ParsedMsg T.Text
  | SendMsg TL.Text
  | WebsocketLog WebsocketLog
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
  pretty (WebsocketLog msg) = "Websocket:" <+> pretty msg

-- ---------------------------------------------------------------------

{- | Convenience function for 'runServerWithHandles' which:
     (1) reads from stdin;
     (2) writes to stdout; and
     (3) logs to stderr and to the client, with some basic filtering.
-}
runServer :: forall config. ServerDefinition config -> IO Int
runServer =
  runServerWithHandles
    defaultIOLogger
    defaultLspLogger
    stdin
    stdout

defaultIOLogger :: LogAction IO (WithSeverity LspServerLog)
defaultIOLogger = L.cmap (show . prettyMsg) L.logStringStderr
 where
  prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)

defaultLspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
defaultLspLogger =
  let clientLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger
   in clientLogger <> L.hoistLogAction liftIO defaultIOLogger

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

 Assumes that the client sends (and wants to receive) the Content-Length
 header. If you do not want this to be the case, use 'runServerWithConfig'
-}
runServerWith ::
  -- | The logger to use outside the main body of the server where we can't assume the ability to send messages.
  LogAction IO (WithSeverity LspServerLog) ->
  -- | The logger to use once the server has started and can successfully send messages.
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  -- | Client input.
  IO BS.StrictByteString ->
  -- | Function to provide output to.
  (BSL.LazyByteString -> IO ()) ->
  ServerDefinition config ->
  IO Int -- exit code
runServerWith ioLogger lspLogger inwards outwards =
  runServerWithConfig ServerConfig{prepareOutwards = prependHeader, parseInwards = parseHeaders, ..}

-- ---------------------------------------------------------------------

data ServerConfig config = ServerConfig
  { ioLogger :: LogAction IO (WithSeverity LspServerLog)
  -- ^ The logger to use outside the main body of the server where we can't assume the ability to send messages.
  , lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
  -- ^ The logger to use once the server has started and can successfully send messages.
  , inwards :: IO BS.StrictByteString
  -- ^ Client input.
  , outwards :: BSL.LazyByteString -> IO ()
  -- ^ Function to provide output to.
  , prepareOutwards :: BSL.LazyByteString -> BSL.LazyByteString
  -- ^ how to prepare an outgoing response for sending. This can be used, to e.g. prepend the Content-Length header, c.f. 'prependHeader'
  , parseInwards :: Attoparsec.Parser BS.StrictByteString
  -- ^ how to parse the input. This can be used to consume the Content-Length and Content-Type headers, c.f. 'parseHeaders'
  }

runServerWithConfig ::
  ServerConfig config ->
  ServerDefinition config ->
  IO Int
runServerWithConfig ServerConfig{..} serverDefinition = do
  ioLogger <& Starting `WithSeverity` Info

  cout <- atomically newTChan :: IO (TChan J.Value)
  _rhpid <- forkIO $ sendServer ioLogger cout outwards prepareOutwards

  let sendMsg msg = atomically $ writeTChan cout $ J.toJSON msg

  ioLoop ioLogger lspLogger inwards parseInwards serverDefinition emptyVFS sendMsg

  return 1

-- ---------------------------------------------------------------------

ioLoop ::
  forall config.
  LogAction IO (WithSeverity LspServerLog) ->
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  IO BS.StrictByteString ->
  Attoparsec.Parser BS.StrictByteString ->
  ServerDefinition config ->
  VFS ->
  (FromServerMessage -> IO ()) ->
  IO ()
ioLoop ioLogger logger clientIn parser serverDefinition vfs sendMsg = do
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

  loop :: Result BS.StrictByteString -> LspM config ()
  loop = go
   where
    go r = do
      res <- parseOne logger clientIn r
      case res of
        Nothing -> pure ()
        Just (msg, remainder) -> do
          Processing.processMessage pLogger $ BSL.fromStrict msg
          go (parse parser remainder)

parseOne ::
  MonadIO m =>
  LogAction m (WithSeverity LspServerLog) ->
  IO BS.StrictByteString ->
  Result BS.StrictByteString ->
  m (Maybe (BS.ByteString, BS.ByteString))
parseOne logger clientIn = go
 where
  go (Fail _ ctxs err) = do
    logger <& HeaderParseFail ctxs err `WithSeverity` Error
    pure Nothing
  go (Partial c) = do
    bs <- liftIO clientIn
    go (c bs)
  go (Done remainder msg) = do
    -- TODO: figure out how to re-enable
    -- This can lead to infinite recursion in logging, see https://github.com/haskell/lsp/issues/447
    -- logger <& ParsedMsg (T.decodeUtf8 msg) `WithSeverity` Debug
    pure $ Just (msg, remainder)

-- ---------------------------------------------------------------------

data WebsocketLog
  = WebsocketShutDown
  | WebsocketNewConnection
  | WebsocketConnectionClosed
  | WebsocketPing
  | WebsocketStarted
  | WebsocketIncomingRequest
  | WebsocketOutgoingResponse
  deriving stock (Show)

instance Pretty WebsocketLog where
  pretty l = case l of
    WebsocketPing -> "Ping"
    WebsocketStarted -> "Started Server, waiting for connections"
    WebsocketShutDown -> "Shut down server"
    WebsocketNewConnection -> "New connection established"
    WebsocketIncomingRequest -> "Received request"
    WebsocketConnectionClosed -> "Closed connection to client"
    WebsocketOutgoingResponse -> "Sent response"

-- | 'host' and 'port' of the websocket server to set up
data WebsocketConfig = WebsocketConfig
  { host :: !String
  -- ^ the host of the websocket server, e.g. @"localhost"@
  , port :: !Int
  -- ^ the port of the websocket server, e.g. @8080@
  }

-- | Set up a websocket server, then call call the continuation (in our case this corresponds to the language server) after accepting a connection
withWebsocket ::
  -- | The logger
  LogAction IO (WithSeverity LspServerLog) ->
  -- | The configuration of the websocket server
  WebsocketConfig ->
  -- | invoke the lsp server, passing communication functions
  (IO BS.StrictByteString -> (BSL.LazyByteString -> IO ()) -> IO r) ->
  IO ()
withWebsocket logger conf startLspServer = do
  let wsLogger = L.cmap (fmap WebsocketLog) logger

  WS.runServer (host conf) (port conf) $ \pending -> do
    conn <- WS.acceptRequest pending
    wsLogger <& WebsocketNewConnection `WithSeverity` Debug

    outChan <- newChan
    inChan <- newChan

    let inwards = readChan inChan
        outwards = writeChan outChan

    WS.withPingThread conn 30 (wsLogger <& WebsocketPing `WithSeverity` Debug) $ do
      withAsync (startLspServer inwards outwards) $ \lspAsync ->
        ( do
            link lspAsync

            race_
              ( forever $ do
                  msg <- readChan outChan
                  wsLogger <& WebsocketOutgoingResponse `WithSeverity` Debug
                  WS.sendTextData conn msg
              )
              ( forever $ do
                  msg <- WS.receiveData conn
                  wsLogger <& WebsocketIncomingRequest `WithSeverity` Debug
                  writeChan inChan msg
                  -- NOTE: since the parser assumes to consume messages
                  -- incrementally,we need to somehow signal that the
                  -- content has terminated - we do this by sending the
                  -- empty string (instead of parsing exactly the content
                  -- length, like in the stdio case)
                  writeChan inChan ""
              )
        )
          `finally` do
            wsLogger <& WebsocketConnectionClosed `WithSeverity` Debug

{- | Given a 'WebsocketConfig', wait for connections using a websocket server.
The continuation passed is called for every new connection and can be used
to initialize state that is specific to that respective connection.

This combines 'withWebsocket' and 'runServerWithConfig'.
-}
withWebsocketRunServer ::
  -- | Configuration for the websocket
  WebsocketConfig ->
  -- | How to set up a new 'ServerDefinition' for a specific configuration. z
  --   This is passed as CPS'd 'IO' to allow for setting (- and cleaning) up
  --   a server per websocket connection
  ((ServerDefinition config -> IO Int) -> IO Int) ->
  -- | The 'IO' logger
  LogAction IO (WithSeverity LspServerLog) ->
  -- | The logger that logs in 'LspM' to the client
  LogAction (LspM config) (WithSeverity LspServerLog) ->
  IO ()
withWebsocketRunServer wsConf withLspDefinition ioLogger lspLogger =
  withWebsocket ioLogger wsConf $ \inwards outwards -> do
    withLspDefinition $ \lspDefinition ->
      runServerWithConfig
        ServerConfig
          { ioLogger
          , lspLogger
          , inwards
          , outwards
          , -- NOTE: if you run the language server on websockets, you do not
            -- need to prepend headers to requests and responses, because
            -- the chunking is already handled by the websocket, i.e. there's
            -- no situation where the client or the server has to rely on input/
            -- output chunking
            prepareOutwards = id
          , parseInwards = Attoparsec.takeByteString
          }
        lspDefinition

-- ---------------------------------------------------------------------

-- | Simple server to make sure all output is serialised
sendServer :: LogAction IO (WithSeverity LspServerLog) -> TChan J.Value -> (BSL.LazyByteString -> IO ()) -> (BSL.LazyByteString -> BSL.LazyByteString) -> IO ()
sendServer _logger msgChan clientOut prepareMessage = do
  forever $ do
    msg <- atomically $ readTChan msgChan

    -- We need to make sure we only send over the content of the message,
    -- and no other tags/wrapper stuff
    let str = J.encode msg
    let out = prepareMessage str

    clientOut out

-- TODO: figure out how to re-enable
-- This can lead to infinite recursion in logging, see https://github.com/haskell/lsp/issues/447
-- logger <& SendMsg (TL.decodeUtf8 str) `WithSeverity` Debug

-- | prepend a Content-Length header to the given message
prependHeader :: BSL.LazyByteString -> BSL.LazyByteString
prependHeader str =
  BSL.concat
    [ TL.encodeUtf8 $ TL.pack $ "Content-Length: " ++ show (BSL.length str)
    , BSL.fromStrict _TWO_CRLF
    , str
    ]

{- | parse Content-Length and Content-Type headers and then consume
  input with length of the Content-Length
-}
parseHeaders :: Attoparsec.Parser BS.StrictByteString
parseHeaders = do
  try contentType <|> return ()
  len <- contentLength
  try contentType <|> return ()
  _ <- string _ONE_CRLF
  Attoparsec.take len
 where
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

_ONE_CRLF :: BS.StrictByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.StrictByteString
_TWO_CRLF = "\r\n\r\n"
