{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Server.Control.Websocket (
  -- **  Using websockets
  WebsocketConfig (..),
  withWebsocket,
  withWebsocketRunServer,
) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent.Async
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Language.LSP.Server.Control
import Language.LSP.Server.Core
import Prettyprinter

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Network.WebSockets qualified as WS

data WebsocketServerLog
  = WsLspServerLog LspServerLog
  | WsWebsocketLog WebsocketLog
  deriving (Show)

instance Pretty WebsocketServerLog where
  pretty (WsLspServerLog msg) = pretty msg
  pretty (WsWebsocketLog msg) = "Websocket:" <+> pretty msg

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
  LogAction IO (WithSeverity WebsocketServerLog) ->
  -- | The configuration of the websocket server
  WebsocketConfig ->
  -- | invoke the lsp server, passing communication functions
  (IO BS.StrictByteString -> (BSL.LazyByteString -> IO ()) -> IO r) ->
  IO ()
withWebsocket logger conf startLspServer = do
  let wsLogger = L.cmap (fmap WsWebsocketLog) logger

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
  {- | How to set up a new 'ServerDefinition' for a specific configuration. z
  This is passed as CPS'd 'IO' to allow for setting (- and cleaning) up
  a server per websocket connection
  -}
  ((ServerDefinition config -> IO Int) -> IO Int) ->
  -- | The 'IO' logger
  LogAction IO (WithSeverity WebsocketServerLog) ->
  -- | The logger that logs in 'LspM' to the client
  LogAction (LspM config) (WithSeverity WebsocketServerLog) ->
  IO ()
withWebsocketRunServer wsConf withLspDefinition ioLogger lspLogger =
  withWebsocket ioLogger wsConf $ \inwards outwards -> do
    withLspDefinition $ \lspDefinition ->
      runServerWithConfig
        ServerConfig
          { ioLogger = L.cmap (fmap WsLspServerLog) ioLogger
          , lspLogger = L.cmap (fmap WsLspServerLog) lspLogger
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
