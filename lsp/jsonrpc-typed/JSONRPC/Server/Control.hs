{-# LANGUAGE OverloadedLabels #-}

module JSONRPC.Server.Control where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Colog.Core.Action (cmap)
import Control.Applicative
import Control.Concurrent.Async qualified as Async
import Control.Lens
import Control.Monad.Catch
import JSONRPC.IO qualified as IO
import JSONRPC.RPC qualified as RPC
import JSONRPC.Server.Core as Server
import JSONRPC.Server.Dispatch qualified as Dispatch
import Prettyprinter
import System.IO

data ServerLog
  = DispatchLog Dispatch.DispatchLog
  | IoLog IO.IoLog
  | Starting
  | Stopping
  | ExceptionalStop SomeException
  deriving stock (Show)

instance Pretty ServerLog where
  pretty (DispatchLog l) = pretty l
  pretty (IoLog l) = pretty l
  pretty Starting = "Starting server"
  pretty Stopping = "Stopping server"
  pretty (ExceptionalStop e) = "Stopping triggered by exception:" <+> pretty (displayException e)

-- ---------------------------------------------------------------------

runServer :: ServerFunction -> IO (ServerHandle, IO ())
runServer serverDefinition = do
  rpc <- RPC.initRpcWithHandles (cmap (fmap IoLog) logger) stdin stdout
  runServerIn logger rpc serverDefinition
 where
  prettyMsg l = brackets (viaShow (L.getSeverity l)) <+> pretty (L.getMsg l)
  logger :: LogAction IO (WithSeverity ServerLog)
  logger = L.cmap (show . prettyMsg) L.logStringStderr

runServerIn ::
  LogAction IO (WithSeverity ServerLog) ->
  (RPC.RpcHandle, IO ()) ->
  ServerFunction ->
  IO (ServerHandle, IO ())
runServerIn logger (rpcHandle, rpcThreads) serverDefinition = do
  serverHandle <- Server.new rpcHandle serverDefinition

  let dispatch =
        Dispatch.dispatchLoop
          (cmap (fmap DispatchLog) logger)
          serverHandle
          (rpcHandle ^. #conn)

  -- Bind all the threads together so that any of them terminating will terminate everything
  let act =
        bracket_
          (logger <& Starting `WithSeverity` Info)
          (logger <& Stopping `WithSeverity` Info)
          $ (Async.runConcurrently $ Async.Concurrently rpcThreads <|> Async.Concurrently dispatch)
            `catch` (\(e :: SomeException) -> logger <& ExceptionalStop e `WithSeverity` Info >> throwM e)

  pure (serverHandle, act)
