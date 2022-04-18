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

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), cmap, (<&))
import Colog.Core qualified as L
import Control.Applicative ((<|>))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Text qualified as T
import Data.Text.Prettyprint.Doc
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Message
import Language.LSP.Server.Core
import Language.LSP.Server.IO qualified as IO
import Language.LSP.Server.Processing qualified as Processing
import Language.LSP.VFS
import System.IO

data LspServerLog
  = LspProcessingLog Processing.LspProcessingLog
  | LspIoLog IO.LspIoLog
  | Starting
  | Stopping
  deriving (Show)

instance Pretty LspServerLog where
  pretty (LspProcessingLog l) = pretty l
  pretty (LspIoLog l) = pretty l
  pretty Starting = "Starting server"
  pretty Stopping = "Stopping server"

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
      BS.hPut hout out
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
  (BS.ByteString -> IO ()) ->
  ServerDefinition config ->
  IO Int -- exit code
runServerWith ioLogger logger clientIn clientOut serverDefinition = do
  ioLogger <& Starting `WithSeverity` Info

  cout <- atomically newTChan
  cin <- atomically newTChan

  let serverOut = IO.serverOut (cmap (fmap LspIoLog) ioLogger) (atomically $ readTChan cout) clientOut
      serverIn = IO.serverIn (cmap (fmap LspIoLog) ioLogger) (atomically . writeTChan cin) clientIn

      sendMsg msg = atomically $ writeTChan cout $ J.toJSON msg
      recvMsg = atomically $ readTChan cin

      processingLoop = initVFS $ \vfs ->
        Processing.processingLoop
          (cmap (fmap LspProcessingLog) ioLogger)
          (cmap (fmap LspProcessingLog) logger)
          vfs
          serverDefinition
          sendMsg
          recvMsg

  -- Bind all the threads together so that any of them terminating will terminate everything
  serverOut `Async.race_` serverIn `Async.race_` processingLoop

  ioLogger <& Stopping `WithSeverity` Info
  return 0
