{- ORMOLU_DISABLE -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.LSP.Test.Session
  ( Session(..)
  , SessionConfig(..)
  , defaultConfig
  , SessionContext(..)
  , SessionState(..)
  , runSession'
  , modifyStatePure
  , modifyStatePure_
  , modifyStateM
  , ask
  , asks
  , sendMessage
  , updateState
  -- , withTimeout
  , logMsg
  , LogMsgType(..)
  , documentChangeUri
  )

where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Applicative
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Process (gracefullyWaitForProcess)
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Session.UpdateState
import Language.LSP.Test.Types
import Language.LSP.VFS
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.Timeout (timeout)

#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif

#ifndef mingw32_HOST_OS
import System.Process (waitForProcess)
#endif

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSession' :: forall m a. (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m
  )
  => Handle -- ^ Server in
  -> Handle -- ^ Server out
  -> Maybe ProcessHandle -- ^ Server process
  -> (Handle -> SessionContext -> IO ()) -- ^ Server listener
  -> SessionConfig
  -> ClientCapabilities
  -> FilePath -- ^ Root directory
  -> Session m () -- ^ To exit the Server properly
  -> Session m a
  -> m a
runSession' serverIn serverOut mServerProc serverHandler config caps rootDir exitServer session = do
  absRootDir <- liftIO $ canonicalizePath rootDir

  liftIO $ hSetBuffering serverIn  NoBuffering
  liftIO $ hSetBuffering serverOut NoBuffering
  -- This is required to make sure that we don’t get any
  -- newline conversion or weird encoding issues.
  liftIO $ hSetBinaryMode serverIn True
  liftIO $ hSetBinaryMode serverOut True

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  initRsp <- newEmptyMVar

  mainThreadId <- myThreadId

  let initState = SessionState
        0
        emptyVFS
        mempty
        False
        Nothing
        mempty
        (lspConfig config)
        mempty
        (ignoreLogNotifications config)
        (ignoreConfigurationRequests config)
        (ignoreRegistrationRequests config)

  isShuttingDownVar <- newMVar False

  stateVar <- newMVar initState

  let context = SessionContext
        serverIn
        absRootDir
        messageChan
        reqMap
        initRsp
        isShuttingDownVar
        config
        caps
        stateVar

  let doShutdown = do
        modifyMVar_ (isShuttingDown context) (const $ pure True)
        timeout (messageTimeout config * 10^(6 :: Int)) (runReaderT (unwrapSession exitServer) context) >>= \case
          Just () -> return ()
          Nothing -> logErrorN "Timeout when shutting down server"

  flip finally (whenJust mServerProc (teardownProcess config serverIn serverOut)) $
    withAsync (flip runReaderT context $ forwardServerMessages serverOut) $ \_ ->
      flip finally doShutdown $
        flip withException (\(e :: SomeException) -> logErrorN ("Exception in session: " <> T.pack (show e))) $ do
          runReaderT (unwrapSession session) context

teardownProcess :: MonadLoggerIO m => SessionConfig -> Handle -> Handle -> ProcessHandle -> m ()
teardownProcess config servIn servOut sp = do
  -- Give the server some time to exit cleanly
  -- It makes the server hangs in windows so we have to avoid it
  logInfoN "Beginning to wait for process"
  gracefullyWaitForProcess (messageTimeout config * 10^(6 :: Int)) sp
  liftIO $ cleanupProcess (Just servIn, Just servOut, Nothing, sp)

-- | Read messages from the server output and write them to the messageChan
forwardServerMessages :: (MonadLoggerIO m, MonadUnliftIO m, MonadReader SessionContext m) => Handle -> m ()
forwardServerMessages serverOut = forever $ do
  ctx <- ask

  msgBytes <- liftIO $ getNextMessage serverOut

  msg <- modifyMVar (requestMap ctx) (\reqMap -> pure (decodeFromServerMsg reqMap msgBytes))

  case msg of
    FromServerMess SMethod_WindowLogMessage (TNotificationMessage { _params=(LogMessageParams level text) }) ->
      -- Give a more concise log message for window/logMessage notifications
      logMsg LogServer ("window/logMessage: (" <> (T.pack (show level)) <> ") " <> text)
    _ -> logMsg LogServer msg

  catch (updateState msg) $ \(e :: SomeException) -> do
    logErrorN [i|Exception when updating state in response to message #{msg}: #{e}|]

  -- Auto-respond to some message types (unless shutdown command has been sent)
  withMVar (isShuttingDown ctx) $ \shuttingDown ->
    unless (shuttingDown) $
      catch (autoRespond msg) $ \(e :: SomeException) -> do
        logErrorN [i|Exception when doing automatic responses in response to message #{msg}: #{e}|]

  state <- readMVar (sessionState ctx)
  unless (
    (ignoringLogNotifications state && isLogNotification msg)
    || (ignoringConfigurationRequests state && isConfigRequest msg)
    || (ignoringRegistrationRequests state && isRegistrationRequest msg)) $
    writeChan (messageChan ctx) msg
  where
    isLogNotification (FromServerMess SMethod_WindowShowMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowLogMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowShowDocument _) = True
    isLogNotification _ = False

    isConfigRequest (FromServerMess SMethod_WorkspaceConfiguration _) = True
    isConfigRequest _ = False

    isRegistrationRequest (FromServerMess SMethod_ClientRegisterCapability _) = True
    isRegistrationRequest (FromServerMess SMethod_ClientUnregisterCapability _) = True
    isRegistrationRequest _ = False

-- -- | Execute a block f that will throw a 'Language.LSP.Test.Exception.Timeout' exception
-- -- after duration seconds. This will override the global timeout
-- -- for waiting for messages to arrive defined in 'SessionConfig'.
-- withTimeout :: MonadIO m => Int -> Session m a -> Session m a
-- withTimeout duration f = do
--   chan <- asks messageChan
--   timeoutId <- getCurTimeoutId
--   modifyStatePure_ $ \s -> s { overridingTimeout = True }
--   tid <- liftIO $ forkIO $ do
--     threadDelay (duration * 1000000)
--     writeChan chan (TimeoutMessage timeoutId)
--   res <- f
--   liftIO $ killThread tid
--   bumpTimeoutId timeoutId
--   modifyStatePure_ $ \s -> s { overridingTimeout = False }
--   return res

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

-- | Automatically respond to some common message types
autoRespond :: (MonadLoggerIO m, MonadReader SessionContext m) => FromServerMessage -> m ()
autoRespond (FromServerMess SMethod_WindowWorkDoneProgressCreate req) =
  sendMessage $ TResponseMessage "2.0" (Just $ req ^. L.id) (Right Null)
autoRespond (FromServerMess SMethod_WorkspaceApplyEdit r) = do
  sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right $ ApplyWorkspaceEditResult True Nothing Nothing)
autoRespond (FromServerMess SMethod_WorkspaceConfiguration r) = do
  let requestedSections = mapMaybe (\i -> i ^? L.section . _Just) $ r ^. L.params . L.items
  ctx <- ask
  state <- readMVar (sessionState ctx)
  let o = curLspConfig state
  -- check for each requested section whether we have it
  let configsOrErrs = flip fmap requestedSections $ \section ->
        case o ^. at (fromString $ T.unpack section) of
          Just config -> Right config
          Nothing -> Left section

  let (errs, configs) = partitionEithers configsOrErrs

  -- we have to return exactly the number of sections requested, so if we can't find all of them then that's an error
  sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) $
    if null errs
    then Right configs
    else Left $ TResponseError (InL LSPErrorCodes_RequestFailed) ("No configuration for requested sections: " <> T.pack (show errs)) Nothing
autoRespond _ = pure ()
