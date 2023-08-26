{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.Shutdown (
  ShutdownHandle,
  new,
  shutdownHandlers,
  isShuttingDown,
  waitShuttingDown,
)
where

import Colog.Core
import Control.Concurrent.Extra
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server.Core
import System.Exit

new :: LogAction IO (WithSeverity LspShutdownLog) -> IO ShutdownHandle
new shutdownLogger = do
  shutdownBarrier <- newBarrier
  pure $ ShutdownHandle{..}

-- | Default Shutdown handler
handleShutdown ::
  ShutdownHandle ->
  MethodParams LSP.Method_Shutdown ->
  IO (Either (ResponseError LSP.Method_Shutdown) (MethodResult LSP.Method_Shutdown))
handleShutdown handle _ = do
  handle.shutdownLogger <& ShuttingDown `WithSeverity` Info
  signalBarrier handle.shutdownBarrier ()
  pure $ Right LSP.Null

handleExit :: ShutdownHandle -> MethodParams LSP.Method_Exit -> IO ()
handleExit handle _ = do
  handle.shutdownLogger <& Exiting `WithSeverity` Info
  exitSuccess

shutdownHandlers :: ShutdownHandle -> Handlers Server LSP.Method
shutdownHandlers handle =
  ( requestHandler LSP.SMethod_Shutdown $
      mkRequestHandler $
        handleShutdown handle
  )
    <> ( notificationHandler LSP.SMethod_Exit $
          mkNotificationHandler $
            handleExit handle
       )

-- | Checks if the server has received a 'shutdown' request.
isShuttingDown :: ShutdownHandle -> IO Bool
isShuttingDown handle = do
  r <- waitBarrierMaybe handle.shutdownBarrier
  pure $ case r of
    Just _ -> True
    Nothing -> False

-- | Blocks until the server receives a 'shutdown' request.
waitShuttingDown :: ShutdownHandle -> IO ()
waitShuttingDown handle = waitBarrier handle.shutdownBarrier

{- Note [Shutdown]
The 'shutdown' request basically tells the server to clean up and stop doing things.
In particular, it allows us to ignore or reject all further messages apart from 'exit'.

We also provide a `Barrier` that indicates whether or not we are shutdown, this can
be convenient, e.g. you can race a thread against `waitBarrier` to have it automatically
be cancelled when we receive `shutdown`.

Shutdown is a request, and the client won't send `exit` until a server responds, so if you
want to be sure that some cleanup happens, you need to ensure we don't respond to `shutdown`
until it's done. The best way to do this is just to install a specific `shutdown` handler.

After the `shutdown` request, we don't handle any more requests and notifications other than
`exit`. We also don't handle any more responses to requests we have sent but just throw the
responses away.
-}
