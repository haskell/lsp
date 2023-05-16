{-|
This module provides utilities to run an LSP `Session` in `IO`.
-}

module Language.LSP.Client where

import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks, runReaderT)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Dependent.Map qualified as DMap
import Data.Either (fromLeft)
import Data.Generics.Labels ()
import Language.LSP.Client.Decoding
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Client.Session
import Language.LSP.Types (From, Method, MethodType, SMethod (..))
import Language.LSP.Types qualified as LSP
import Language.LSP.VFS (initVFS)
import System.IO (Handle, stdin, stdout)
import UnliftIO (concurrently_, race)
import Prelude

-- | Starts a new session, using the specified handles to communicate with the
-- server.
runSessionWithHandles
    :: Handle      -- ^ The input handle: messages sent from the server to the client will be read from here
    -> Handle      -- ^ The output handle: messages sent by the client will be written here
    -> Session a   -- ^ Session actions
    -> IO a
runSessionWithHandles input output action = initVFS $ \vfs -> do
    initialState <- defaultSessionState vfs
    flip runReaderT initialState $ do
        actionResult <- race action $ do
            let send = do
                    message <- asks outgoing >>= liftIO . atomically . readTQueue
                    liftIO $ LazyByteString.hPut output $ encode message
            let receive = do
                    serverBytes <- liftIO $ getNextMessage input
                    (serverMessage, requestCallback) <-
                        asks pendingRequests
                            >>= liftIO . atomically . flip stateTVar (decodeFromServerMsg serverBytes)
                    handleServerMessage serverMessage
                    liftIO requestCallback
                    case serverMessage of
                        LSP.FromServerMess smethod msg -> case methodType smethod of
                            Notification -> do
                                handlers :: NotificationMap <- asks notificationHandlers >>= liftIO . readTVarIO
                                let NotificationCallback cb = DMap.findWithDefault (NotificationCallback (const $ pure ())) smethod handlers
                                liftIO $ cb msg
                            _ -> pure ()
                        _ -> pure ()
            concurrently_ (forever send) (forever receive)
        pure $ fromLeft (error "runSessionWithHandle: send/receive thread should not exit") actionResult

data SMethodType (t :: MethodType) where
    Notification :: SMethodType 'LSP.Notification
    Request :: SMethodType 'LSP.Request

methodType :: forall (f :: From) (t :: MethodType) (m :: Method f t). SMethod m -> SMethodType t
methodType SInitialize = Request
methodType SInitialized = Notification
methodType SShutdown = Request
methodType SExit = Notification
methodType SWorkspaceDidChangeWorkspaceFolders = Notification
methodType SWorkspaceDidChangeConfiguration = Notification
methodType SWorkspaceDidChangeWatchedFiles = Notification
methodType SWorkspaceSymbol = Request
methodType SWorkspaceExecuteCommand = Request
methodType STextDocumentDidOpen = Notification
methodType STextDocumentDidChange = Notification
methodType STextDocumentWillSave = Notification
methodType STextDocumentWillSaveWaitUntil = Request
methodType STextDocumentDidSave = Notification
methodType STextDocumentDidClose = Notification
methodType STextDocumentCompletion = Request
methodType SCompletionItemResolve = Request
methodType STextDocumentHover = Request
methodType STextDocumentSignatureHelp = Request
methodType STextDocumentDeclaration = Request
methodType STextDocumentDefinition = Request
methodType STextDocumentTypeDefinition = Request
methodType STextDocumentImplementation = Request
methodType STextDocumentReferences = Request
methodType STextDocumentDocumentHighlight = Request
methodType STextDocumentDocumentSymbol = Request
methodType STextDocumentCodeAction = Request
methodType STextDocumentCodeLens = Request
methodType SCodeLensResolve = Request
methodType STextDocumentDocumentLink = Request
methodType SDocumentLinkResolve = Request
methodType STextDocumentDocumentColor = Request
methodType STextDocumentColorPresentation = Request
methodType STextDocumentFormatting = Request
methodType STextDocumentRangeFormatting = Request
methodType STextDocumentOnTypeFormatting = Request
methodType STextDocumentRename = Request
methodType STextDocumentPrepareRename = Request
methodType STextDocumentFoldingRange = Request
methodType STextDocumentSelectionRange = Request
methodType STextDocumentPrepareCallHierarchy = Request
methodType SCallHierarchyIncomingCalls = Request
methodType SCallHierarchyOutgoingCalls = Request
methodType STextDocumentSemanticTokens = Request
methodType STextDocumentSemanticTokensFull = Request
methodType STextDocumentSemanticTokensFullDelta = Request
methodType STextDocumentSemanticTokensRange = Request
methodType SWindowShowMessage = Notification
methodType SWindowShowMessageRequest = Request
methodType SWindowShowDocument = Request
methodType SWindowLogMessage = Notification
methodType SWindowWorkDoneProgressCancel = Notification
methodType SWindowWorkDoneProgressCreate = Request
methodType SProgress = Notification
methodType STelemetryEvent = Notification
methodType SClientRegisterCapability = Request
methodType SClientUnregisterCapability = Request
methodType SWorkspaceWorkspaceFolders = Request
methodType SWorkspaceConfiguration = Request
methodType SWorkspaceApplyEdit = Request
methodType SWorkspaceSemanticTokensRefresh = Request
methodType STextDocumentPublishDiagnostics = Notification
methodType SCancelRequest = Notification
methodType (SCustomMethod _) = undefined
