{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
-}

module Language.LSP.Client.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (throw)
import Control.Lens hiding (Empty, List)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, execState)
import Data.Default (def)
import Data.Foldable (foldl', foldr', forM_, toList)
import Data.Function (on)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope (Rope)
import GHC.Generics (Generic)
import Language.LSP.Client.Compat (getCurrentProcessID, lspClientInfo)
import Language.LSP.Client.Decoding
import Language.LSP.Client.Exceptions (SessionException (UnexpectedResponseError))
import Language.LSP.Types
import Language.LSP.Types.Capabilities (ClientCapabilities, fullCaps)
import Language.LSP.Types.Lens hiding (applyEdit, capabilities, executeCommand, id, message, rename, to)
import Language.LSP.Types.Lens qualified as LSP
import Language.LSP.VFS
    ( VFS
    , VfsLog
    , VirtualFile (..)
    , changeFromClientVFS
    , changeFromServerVFS
    , closeVFS
    , lsp_version
    , openVFS
    , vfsMap
    , virtualFileVersion
    )
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, (</>))
import System.FilePath.Glob qualified as Glob
import Prelude

deriving stock instance Generic ProgressToken

deriving anyclass instance Hashable ProgressToken

data SessionState = SessionState
    { initialized :: TMVar InitializeResult
    -- ^ The response of the initialization handshake, if any.
    , pendingRequests :: TVar RequestMap
    -- ^ Response callbacks for sent requests waiting for a response. Once a response arrives the request is removed from this map.
    , notificationHandlers :: TVar NotificationMap
    -- ^ Notification callbacks that fire whenever a notification of their type is received.
    , lastRequestId :: TVar Int32
    -- ^ A counter to send each request to the server is sent with a unique ID, allowing us to pair it back with its response.
    , serverCapabilities :: TVar (HashMap Text SomeRegistration)
    -- ^ The capabilities that the server has dynamically registered with us so far.
    , clientCapabilities :: ClientCapabilities
    -- ^ The client capabilities advertised to the server. Not a `TVar` because it does not change during the session.
    , progressTokens :: TVar (HashSet ProgressToken)
    -- ^ Progress messages received from the server.
    , outgoing :: TQueue FromClientMessage
    -- ^ Messages that have been serialised but not yet written to the output handle.
    , vfs :: TVar VFS
    -- ^ Virtual, in-memory file system of the files known to the LSP.
    , rootDir :: FilePath
    -- ^ The root of the project as sent to the server. Document URIs are relative to it. Not a `TVar` because it does not change during the session.
    }

defaultSessionState :: VFS -> IO SessionState
defaultSessionState vfs' = do
    initialized <- newEmptyTMVarIO
    pendingRequests <- newTVarIO emptyRequestMap
    notificationHandlers <- newTVarIO emptyNotificationMap
    lastRequestId <- newTVarIO 0
    serverCapabilities <- newTVarIO mempty
    progressTokens <- newTVarIO mempty
    outgoing <- newTQueueIO
    vfs <- newTVarIO vfs'
    pure
        SessionState
            { rootDir = "."
            , clientCapabilities = def
            , ..
            }

-- | A session representing one instance of launching and connecting to a server.
-- It is essentially an STM-backed `StateT`: despite it being `ReaderT`, it can still
-- mutate `TVar` values.
type Session = ReaderT SessionState IO

documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri

-- | Fires whenever the client receives a message from the server. Updates the session state as needed.
-- Note that this does not provide any business logic beyond updating the session state; you most likely
-- want to use `sendRequest` and `receiveNotification` to register callbacks for specific messages.
handleServerMessage :: FromServerMessage -> Session ()

handleServerMessage (FromServerMess SProgress req) = do
    let update = asks progressTokens >>= liftIO . flip modifyTVarIO (HashSet.insert $ req ^. params . token)
    case req ^. params . value of
        Begin{} -> update
        End{} -> update
        Report{} -> pure ()

handleServerMessage (FromServerMess SClientRegisterCapability req) = do
    let List newRegs = req ^. params . registrations <&> \sr@(SomeRegistration r) -> (r ^. LSP.id, sr)
    asks serverCapabilities >>= liftIO . flip modifyTVarIO (HashMap.union (HashMap.fromList newRegs))

handleServerMessage (FromServerMess SClientUnregisterCapability req) = do
    let List unRegs = req ^. params . unregisterations <&> (^. LSP.id)
    asks serverCapabilities >>= liftIO . flip modifyTVarIO (flip (foldr' HashMap.delete) unRegs)

handleServerMessage (FromServerMess SWorkspaceApplyEdit r) = do
    -- First, prefer the versioned documentChanges field
    allChangeParams <- case r ^. params . edit . documentChanges of
        Just (List cs) -> do
            mapM_ (checkIfNeedsOpened . documentChangeUri) cs
            -- replace the user provided version numbers with the VFS ones + 1
            -- (technically we should check that the user versions match the VFS ones)
            cs' <- traverseOf (traverse . _InL . textDocument) bumpNewestVersion cs
            return $ mapMaybe getParamsFromDocumentChange cs'
        -- Then fall back to the changes field
        Nothing -> case r ^. params . edit . changes of
            Just cs -> do
                mapM_ checkIfNeedsOpened (HashMap.keys cs)
                concat <$> mapM (uncurry getChangeParams) (HashMap.toList cs)
            Nothing ->
                error "WorkspaceEdit contains neither documentChanges nor changes!"

    asks vfs >>= liftIO . flip modifyTVarIO (execState $ changeFromServerVFS logger r)

    let groupedParams = groupBy (\a b -> a ^. textDocument == b ^. textDocument) allChangeParams
        mergedParams = mergeParams <$> groupedParams

    forM_ mergedParams (sendNotification STextDocumentDidChange)

    -- Update VFS to new document versions
    let sortedVersions = sortBy (compare `on` (^. textDocument . version)) <$> groupedParams
        latestVersions = (^. textDocument) . last <$> sortedVersions

    forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
        asks vfs
            >>= liftIO
                . flip
                    modifyTVarIO
                    ( \vfs -> do
                        let update (VirtualFile oldV file_ver t) = VirtualFile (fromMaybe oldV v) (file_ver + 1) t
                         in vfs & vfsMap . ix (toNormalizedUri uri) %~ update
                    )
    sendResponse
        r
        $ Right
            ApplyWorkspaceEditResponseBody
                { _applied = True
                , _failureReason = Nothing
                , _failedChange = Nothing
                }
  where
    logger :: LogAction (StateT VFS Identity) (WithSeverity VfsLog)
    logger = LogAction $ \(WithSeverity msg sev) -> case sev of Error -> error $ show msg; _ -> pure ()
    checkIfNeedsOpened uri = do
        isOpen <- asks vfs >>= liftIO . readTVarIO <&> has (vfsMap . ix (toNormalizedUri uri))

        -- if its not open, open it
        unless isOpen $ do
            contents <- maybe (pure "") (liftIO . Text.readFile) (uriToFilePath uri)
            sendNotification
                STextDocumentDidOpen
                DidOpenTextDocumentParams
                    { _textDocument =
                        TextDocumentItem
                            { _uri = uri
                            , _languageId = ""
                            , _version = 0
                            , _text = contents
                            }
                    }

    getParamsFromTextDocumentEdit :: TextDocumentEdit -> DidChangeTextDocumentParams
    getParamsFromTextDocumentEdit (TextDocumentEdit docId (List edits)) = do
        DidChangeTextDocumentParams docId (List $ editToChangeEvent <$> edits)

    editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
    editToChangeEvent (InR e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)
    editToChangeEvent (InL e) = TextDocumentContentChangeEvent (Just $ e ^. range) Nothing (e ^. newText)

    getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
    getParamsFromDocumentChange (InL textDocumentEdit) = Just $ getParamsFromTextDocumentEdit textDocumentEdit
    getParamsFromDocumentChange _ = Nothing

    bumpNewestVersion (VersionedTextDocumentIdentifier uri _) = head <$> textDocumentVersions uri

    -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
    -- where n is the current version
    textDocumentVersions :: Uri -> Session [VersionedTextDocumentIdentifier]
    textDocumentVersions uri = do
        vfs <- asks vfs >>= liftIO . readTVarIO
        let curVer = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . lsp_version
        pure $ VersionedTextDocumentIdentifier uri . Just <$> [curVer + 1 ..]

    textDocumentEdits uri edits = do
        vers <- textDocumentVersions uri
        pure $ zipWith (\v e -> TextDocumentEdit v (List [InL e])) vers edits

    getChangeParams uri (List edits) = fmap getParamsFromTextDocumentEdit <$> textDocumentEdits uri (reverse edits)

    mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
    mergeParams params =
        let events = concat $ toList $ toList . (^. contentChanges) <$> params
         in DidChangeTextDocumentParams (head params ^. textDocument) (List events)

handleServerMessage (FromServerMess SWindowWorkDoneProgressCreate req) = sendResponse req $ Right Empty

handleServerMessage _ = pure ()

overTVar :: (a -> a) -> TVar a -> STM a
overTVar f var = stateTVar var (\x -> (f x, f x))

overTVarIO :: (a -> a) -> TVar a -> IO a
overTVarIO = (atomically .) . overTVar

modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO = (atomically .) . modifyTVar

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO = (atomically .) . writeTVar

-- | Sends a request to the server, with a callback that fires when the response arrives.
-- Multiple requests can be waiting at the same time.
sendRequest
    :: forall (m :: Method 'FromClient 'Request)
     . Message m ~ RequestMessage m
    => SMethod m
    -> MessageParams m
    -> (ResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest requestMethod params requestCallback = do
    reqId <- asks lastRequestId >>= liftIO . overTVarIO (+ 1) <&> IdInt
    asks pendingRequests >>= liftIO . flip modifyTVarIO (updateRequestMap reqId RequestCallback{..})
    sendMessage $ fromClientReq $ RequestMessage "2.0" reqId requestMethod params
    pure reqId

-- | Send a response to the server. This is used internally to acknowledge server requests.
-- Users of this library cannot register callbacks to server requests, so this function is probably of no use to them.
sendResponse
    :: forall (m :: Method 'FromServer 'Request)
     . RequestMessage m
    -> Either ResponseError (ResponseResult m)
    -> Session ()
sendResponse req =
    sendMessage
        . FromClientRsp (req ^. LSP.method)
        . ResponseMessage (req ^. jsonrpc) (Just $ req ^. LSP.id)

-- | Sends a request to the server and synchronously waits for its response.
request
    :: forall (m :: Method 'FromClient 'Request)
     . Message m ~ RequestMessage m
    => SMethod m
    -> MessageParams m
    -> Session (ResponseMessage m)
request method params = do
    done <- liftIO newEmptyMVar
    void $ sendRequest method params $ putMVar done
    liftIO $ takeMVar done

{- | Checks the response for errors and throws an exception if needed.
 Returns the result if successful.
-}
getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult response = either err id $ response ^. result
  where
    lid = SomeLspId $ fromJust $ response ^. LSP.id
    err = throw . UnexpectedResponseError lid

-- | Sends a notification to the server. Updates the VFS if the notification is a document update.
sendNotification
    :: forall (m :: Method 'FromClient 'Notification)
     . Message m ~ NotificationMessage m
    => SMethod m
    -> MessageParams m
    -> Session ()
sendNotification m params = do
    let n = NotificationMessage "2.0" m params
    vfs <- asks vfs
    case m of
        STextDocumentDidOpen -> liftIO $ modifyTVarIO vfs (execState $ openVFS mempty n)
        STextDocumentDidClose -> liftIO $ modifyTVarIO vfs (execState $ closeVFS mempty n)
        STextDocumentDidChange -> liftIO $ modifyTVarIO vfs (execState $ changeFromClientVFS mempty n)
        _ -> pure ()
    sendMessage $ fromClientNot n

-- | Registers a callback for notifications received from the server.
-- If multiple callbacks are registered for the same notification method, they will all be called.
receiveNotification
    :: forall (m :: Method 'FromServer 'Notification)
     . SMethod m
    -> (Message m -> IO ())
    -> Session ()
receiveNotification method notificationCallback =
    asks notificationHandlers
        >>= liftIO
            . flip
                modifyTVarIO
                ( appendNotificationCallback method NotificationCallback{..}
                )

-- | Clears the registered callback for the given notification method, if any.
-- If multiple callbacks have been registered, this clears /all/ of them.
clearNotificationCallback
    :: forall (m :: Method 'FromServer 'Notification)
     . SMethod m
    -> Session ()
clearNotificationCallback method =
    asks notificationHandlers
        >>= liftIO
            . flip
                modifyTVarIO
                ( removeNotificationCallback method
                )

-- | Queues a message to be sent to the server at the client's earliest convenience.
sendMessage :: FromClientMessage -> Session ()
sendMessage msg = asks outgoing >>= liftIO . atomically . (`writeTQueue` msg)

-- | Performs the initialisation handshake and synchronously waits for its completion.
-- When the function completes, the session is initialised.
initialize :: Session ()
initialize = do
    pid <- liftIO getCurrentProcessID
    response <-
        request
            SInitialize
            InitializeParams
                { _workDoneToken = Nothing
                , _processId = Just $ fromIntegral pid
                , _clientInfo = Just lspClientInfo
                , _rootPath = Nothing
                , _rootUri = Nothing
                , _initializationOptions = Nothing
                , _capabilities = fullCaps
                , _trace = Just TraceOff
                , _workspaceFolders = Nothing
                }
    asks initialized >>= liftIO . atomically . flip putTMVar (getResponseResult response)
    sendNotification SInitialized $ Just InitializedParams

{- | /Creates/ a new text document. This is different from 'openDoc'
 as it sends a @workspace/didChangeWatchedFiles@ notification letting the server
 know that a file was created within the workspace, __provided that the server
 has registered for it__, and the file matches any patterns the server
 registered for.
 It /does not/ actually create a file on disk, but is useful for convincing
 the server that one does exist.
-}
createDoc
    :: FilePath
    -- ^ The path to the document to open, __relative to the root directory__.
    -> Text
    -- ^ The text document's language identifier, e.g. @"haskell"@.
    -> Text
    -- ^ The content of the text document to create.
    -> Session TextDocumentIdentifier
    -- ^ The identifier of the document just created.
createDoc file language contents = do
    serverCaps <- asks serverCapabilities >>= liftIO . readTVarIO
    clientCaps <- asks clientCapabilities
    rootDir <- asks rootDir
    absFile <- liftIO $ canonicalizePath (rootDir </> file)
    let pred :: SomeRegistration -> [Registration 'WorkspaceDidChangeWatchedFiles]
        pred (SomeRegistration r@(Registration _ SWorkspaceDidChangeWatchedFiles _)) = [r]
        pred _ = mempty
        regs :: [Registration 'WorkspaceDidChangeWatchedFiles]
        regs = concatMap pred $ HashMap.elems serverCaps
        watchHits :: FileSystemWatcher -> Bool
        watchHits (FileSystemWatcher pattern kind) =
            -- If WatchKind is excluded, defaults to all true as per spec
            fileMatches (Text.unpack pattern) && maybe True (view watchCreate) kind

        fileMatches pattern = Glob.match (Glob.compile pattern) (if isAbsolute pattern then absFile else file)

        regHits :: Registration 'WorkspaceDidChangeWatchedFiles -> Bool
        regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. registerOptions . _Just . watchers)

        clientCapsSupports =
            clientCaps
                ^? workspace
                    . _Just
                    . didChangeWatchedFiles
                    . _Just
                    . dynamicRegistration
                    . _Just
                == Just True
        shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

    when shouldSend $
        sendNotification SWorkspaceDidChangeWatchedFiles $
            DidChangeWatchedFilesParams $
                List [FileEvent (filePathToUri (rootDir </> file)) FcCreated]
    openDoc' file language contents

{- | Opens a text document that /exists on disk/, and sends a
 @textDocument/didOpen@ notification to the server.
-}
openDoc :: FilePath -> Text -> Session TextDocumentIdentifier
openDoc file language = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    contents <- liftIO $ Text.readFile fp
    openDoc' file language contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: FilePath -> Text -> Text -> Session TextDocumentIdentifier
openDoc' file language contents = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
        uri = filePathToUri fp
        item = TextDocumentItem uri language 0 contents
    sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams item)
    pure $ TextDocumentIdentifier uri

-- | Closes a text document and sends a @textDocument/didClose@ notification to the server.
closeDoc :: TextDocumentIdentifier -> Session ()
closeDoc docId = do
    let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
    sendNotification STextDocumentDidClose params

-- | Changes a text document and sends a @textDocument/didChange@ notification to the server.
changeDoc :: TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
    verDoc <- getVersionedDoc docId
    let params = DidChangeTextDocumentParams (verDoc & version . non 0 +~ 1) (List changes)
    sendNotification STextDocumentDidChange params

-- | Gets the Uri for the file relative to the session's root directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    return $ filePathToUri fp

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session (Maybe Rope)
documentContents (TextDocumentIdentifier uri) = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    pure $ vfs ^? vfsMap . ix (toNormalizedUri uri) . to _file_text

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: TextDocumentIdentifier -> Session VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier uri) = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    let ver = vfs ^? vfsMap . ix (toNormalizedUri uri) . to virtualFileVersion
    pure $ VersionedTextDocumentIdentifier uri ver
