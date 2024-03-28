{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

{- |
Module      : Language.LSP.Test
Description : A functional testing framework for LSP servers.
Maintainer  : luke_lau@icloud.com
Stability   : experimental
Portability : non-portable

Provides the framework to start functionally testing
<https://github.com/Microsoft/language-server-protocol Language Server Protocol servers>.
You should import "Language.LSP.Types" alongside this.
-}
module Language.LSP.Test (
  -- * Sessions
  Session,
  runSession,
  runSessionWithConfig,
  runSessionWithConfigCustomProcess,
  runSessionWithHandles,
  runSessionWithHandles',
  setIgnoringLogNotifications,
  setIgnoringConfigurationRequests,
  setIgnoringRegistrationRequests,

  -- ** Config
  SessionConfig (..),
  defaultConfig,
  C.fullCaps,

  -- ** Exceptions
  module Language.LSP.Test.Exceptions,
  withTimeout,

  -- * Sending
  request,
  request_,
  sendRequest,
  sendNotification,
  sendResponse,

  -- * Receiving
  module Language.LSP.Test.Parsing,

  -- * Utilities

  -- | Quick helper functions for common tasks.

  -- ** Initialization
  initializeResponse,

  -- ** Config
  modifyConfig,
  setConfig,
  modifyConfigSection,
  setConfigSection,

  -- ** Documents
  createDoc,
  openDoc,
  closeDoc,
  changeDoc,
  documentContents,
  getDocumentEdit,
  getDocUri,
  getVersionedDoc,

  -- ** Symbols
  getDocumentSymbols,

  -- ** Diagnostics
  waitForDiagnostics,
  waitForDiagnosticsSource,
  noDiagnostics,
  getCurrentDiagnostics,
  getIncompleteProgressSessions,

  -- ** Commands
  executeCommand,

  -- ** Code Actions
  getCodeActions,
  getAndResolveCodeActions,
  getAllCodeActions,
  executeCodeAction,
  resolveCodeAction,
  resolveAndExecuteCodeAction,

  -- ** Completions
  getCompletions,
  getAndResolveCompletions,

  -- ** References
  getReferences,

  -- ** Definitions
  getDeclarations,
  getDefinitions,
  getTypeDefinitions,
  getImplementations,

  -- ** Renaming
  rename,

  -- ** Hover
  getHover,

  -- ** Highlights
  getHighlights,

  -- ** Formatting
  formatDoc,
  formatRange,

  -- ** Edits
  applyEdit,

  -- ** Code lenses
  getCodeLenses,
  getAndResolveCodeLenses,
  resolveCodeLens,

  -- ** Call hierarchy
  prepareCallHierarchy,
  incomingCalls,
  outgoingCalls,

  -- ** SemanticTokens
  getSemanticTokens,

  -- ** Capabilities
  getRegisteredCapabilities,
) where

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Exception
import Control.Lens hiding (Empty, List, (.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (execState)
import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.Default
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (field')
import Data.List
import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Language.LSP.Protocol.Capabilities qualified as C
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message (MessageDirection (..), MessageKind (..), Method (..), SMethod (..))
import Language.LSP.Protocol.Message qualified as L
import Language.LSP.Protocol.Types (ClientCapabilities, type (|?) (..))
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Parsing
import Language.LSP.Test.Server
import Language.LSP.Test.Session
import Language.LSP.VFS
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Glob qualified as Glob
import System.IO
import System.Process (CreateProcess, ProcessHandle)

{- | Starts a new session.

 > runSession "hie" fullCaps "path/to/root/dir" $ do
 >   doc <- openDoc "Desktop/simple.hs" "haskell"
 >   diags <- waitForDiagnostics
 >   let pos = Position 12 5
 >       params = TextDocumentPositionParams doc
 >   hover <- request STextdocumentHover params
-}
runSession ::
  -- | The command to run the server.
  String ->
  -- | The capabilities that the client should declare.
  ClientCapabilities ->
  -- | The filepath to the root directory for the session.
  FilePath ->
  -- | The session to run.
  Session a ->
  IO a
runSession = runSessionWithConfig def

-- | Starts a new session with a custom configuration.
runSessionWithConfig ::
  -- | Configuration options for the session.
  SessionConfig ->
  -- | The command to run the server.
  String ->
  -- | The capabilities that the client should declare.
  ClientCapabilities ->
  -- | The filepath to the root directory for the session.
  FilePath ->
  -- | The session to run.
  Session a ->
  IO a
runSessionWithConfig = runSessionWithConfigCustomProcess Prelude.id

-- | Starts a new session with a custom configuration and server 'CreateProcess'.
runSessionWithConfigCustomProcess ::
  -- | Tweak the 'CreateProcess' used to start the server.
  (CreateProcess -> CreateProcess) ->
  -- | Configuration options for the session.
  SessionConfig ->
  -- | The command to run the server.
  String ->
  -- | The capabilities that the client should declare.
  ClientCapabilities ->
  -- | The filepath to the root directory for the session.
  FilePath ->
  -- | The session to run.
  Session a ->
  IO a
runSessionWithConfigCustomProcess modifyCreateProcess config' serverExe caps rootDir session = do
  config <- envOverrideConfig config'
  withServer serverExe (logStdErr config) modifyCreateProcess $ \serverIn serverOut serverProc ->
    runSessionWithHandles' (Just serverProc) serverIn serverOut config caps rootDir session

{- | Starts a new session, using the specified handles to communicate with the
 server. You can use this to host the server within the same process.
 An example with lsp might look like:

 > (hinRead, hinWrite) <- createPipe
 > (houtRead, houtWrite) <- createPipe
 >
 > forkIO $ void $ runServerWithHandles hinRead houtWrite serverDefinition
 > runSessionWithHandles hinWrite houtRead defaultConfig fullCaps "." $ do
 >   -- ...
-}
runSessionWithHandles ::
  -- | The input handle
  Handle ->
  -- | The output handle
  Handle ->
  SessionConfig ->
  -- | The capabilities that the client should declare.
  ClientCapabilities ->
  -- | The filepath to the root directory for the session.
  FilePath ->
  -- | The session to run.
  Session a ->
  IO a
runSessionWithHandles = runSessionWithHandles' Nothing

runSessionWithHandles' ::
  Maybe ProcessHandle ->
  -- | The input handle
  Handle ->
  -- | The output handle
  Handle ->
  SessionConfig ->
  -- | The capabilities that the client should declare.
  ClientCapabilities ->
  -- | The filepath to the root directory for the session.
  FilePath ->
  -- | The session to run.
  Session a ->
  IO a
runSessionWithHandles' serverProc serverIn serverOut config' caps rootDir session = do
  pid <- getCurrentProcessID
  absRootDir <- canonicalizePath rootDir

  config <- envOverrideConfig config'

  let initializeParams =
        L.InitializeParams
          Nothing
          -- Narrowing to Int32 here, but it's unlikely that a PID will
          -- be outside the range
          (InL $ fromIntegral pid)
          (Just lspTestClientInfo)
          (Just $ T.pack absRootDir)
          Nothing
          (InL $ L.filePathToUri absRootDir)
          caps
          -- TODO: make this configurable?
          (Just $ Object $ lspConfig config')
          (Just L.TraceValue_Off)
          (fmap InL $ initialWorkspaceFolders config)
  runSession' serverIn serverOut serverProc listenServer config caps rootDir exitServer $ do
    -- Wrap the session around initialize and shutdown calls
    initReqId <- sendRequest SMethod_Initialize initializeParams

    -- Because messages can be sent in between the request and response,
    -- collect them and then...
    (inBetween, initRspMsg) <- manyTill_ anyMessage (responseForId SMethod_Initialize initReqId)

    case initRspMsg.result of
      Left error -> liftIO $ putStrLn ("Error while initializing: " ++ show error)
      Right _ -> pure ()

    initRspVar <- initRsp <$> ask
    liftIO $ putMVar initRspVar initRspMsg
    sendNotification SMethod_Initialized L.InitializedParams

    -- ... relay them back to the user Session so they can match on them!
    -- As long as they are allowed.
    forM_ inBetween checkLegalBetweenMessage
    msgChan <- asks messageChan
    liftIO $ writeList2Chan msgChan (ServerMessage <$> inBetween)

    -- Run the actual test
    session
 where
  -- \| Asks the server to shutdown and exit politely
  exitServer :: Session ()
  exitServer = request_ SMethod_Shutdown Nothing >> sendNotification SMethod_Exit Nothing

  -- \| Listens to the server output until the shutdown ACK,
  -- makes sure it matches the record and signals any semaphores
  listenServer :: Handle -> SessionContext -> IO ()
  listenServer serverOut context = do
    msgBytes <- getNextMessage serverOut

    msg <- modifyMVar (requestMap context) $ \reqMap ->
      pure $ decodeFromServerMsg reqMap msgBytes
    writeChan (messageChan context) (ServerMessage msg)

    case msg of
      (L.FromServerRsp SMethod_Shutdown _) -> return ()
      _ -> listenServer serverOut context

  -- \| Is this message allowed to be sent by the server between the intialize
  -- request and response?
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
  checkLegalBetweenMessage :: L.FromServerMessage -> Session ()
  checkLegalBetweenMessage (L.FromServerMess SMethod_WindowShowMessage _) = pure ()
  checkLegalBetweenMessage (L.FromServerMess SMethod_WindowLogMessage _) = pure ()
  checkLegalBetweenMessage (L.FromServerMess SMethod_TelemetryEvent _) = pure ()
  checkLegalBetweenMessage (L.FromServerMess SMethod_WindowShowMessageRequest _) = pure ()
  checkLegalBetweenMessage msg = throw (IllegalInitSequenceMessage msg)

-- | Check environment variables to override the config
envOverrideConfig :: SessionConfig -> IO SessionConfig
envOverrideConfig cfg = do
  logMessages' <- fromMaybe (logMessages cfg) <$> checkEnv "LSP_TEST_LOG_MESSAGES"
  logStdErr' <- fromMaybe (logStdErr cfg) <$> checkEnv "LSP_TEST_LOG_STDERR"
  return $ cfg{logMessages = logMessages', logStdErr = logStdErr'}
 where
  checkEnv :: String -> IO (Maybe Bool)
  checkEnv s = fmap convertVal <$> lookupEnv s
  convertVal "0" = False
  convertVal _ = True

-- | The current text contents of a document.
documentContents :: L.TextDocumentIdentifier -> Session T.Text
documentContents doc = do
  vfs <- vfs <$> get
  let Just file = vfs ^. #vfsMap . at (L.toNormalizedUri (doc.uri))
  return (virtualFileText file)

{- | Parses an ApplyEditRequest, checks that it is for the passed document
 and returns the new content
-}
getDocumentEdit :: L.TextDocumentIdentifier -> Session T.Text
getDocumentEdit doc = do
  req <- message SMethod_WorkspaceApplyEdit

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $
      throw (IncorrectApplyEditRequest (show req))

  documentContents doc
 where
  checkDocumentChanges req =
    let changes = req.params.edit.documentChanges
        maybeDocs = fmap (fmap documentChangeUri) changes
     in case maybeDocs of
          Just docs -> (doc.uri) `elem` docs
          Nothing -> False
  checkChanges req =
    let mMap = req.params.edit.changes
     in maybe False (Map.member (doc.uri)) mMap

{- | Sends a request to the server and waits for its response.
 Will skip any messages in between the request and the response
 @
 rsp <- request STextDocumentDocumentSymbol params
 @
 Note: will skip any messages in between the request and the response.
-}
request :: L.SClientMethod m -> L.MessageParams m -> Session (L.TResponseMessage m)
request m = sendRequest m >=> skipManyTill anyMessage . responseForId m

-- | The same as 'sendRequest', but discard the response.
request_ :: L.SClientMethod (m :: Method ClientToServer Request) -> L.MessageParams m -> Session ()
request_ p = void . request p

-- | Sends a request to the server. Unlike 'request', this doesn't wait for the response.
sendRequest ::
  -- | The request method.
  L.SClientMethod m ->
  -- | The request parameters.
  L.MessageParams m ->
  -- | The id of the request that was sent.
  Session (L.LspId m)
sendRequest method params = do
  idn <- curReqId <$> get
  modify $ \c -> c{curReqId = idn + 1}
  let id = L.IdInt idn

  let mess = L.TRequestMessage "2.0" id method params

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $
    modifyMVar_ reqMap $
      \r -> return $ fromJust $ updateRequestMap r id method

  ~() <- case L.splitClientMethod method of
    L.IsClientReq -> sendMessage mess
    L.IsClientEither -> sendMessage $ L.ReqMess mess

  return id

-- | Sends a notification to the server.
sendNotification ::
  -- | The notification method.
  L.SClientMethod (m :: Method ClientToServer Notification) ->
  -- | The notification parameters.
  L.MessageParams m ->
  Session ()
-- Open a virtual file if we send a did open text document notification
sendNotification SMethod_TextDocumentDidOpen params = do
  let n = L.TNotificationMessage "2.0" SMethod_TextDocumentDidOpen params
  oldVFS <- vfs <$> get
  let newVFS = flip execState oldVFS $ openVFS mempty n
  modify (\s -> s{vfs = newVFS})
  sendMessage n

-- Close a virtual file if we send a close text document notification
sendNotification SMethod_TextDocumentDidClose params = do
  let n = L.TNotificationMessage "2.0" SMethod_TextDocumentDidClose params
  oldVFS <- vfs <$> get
  let newVFS = flip execState oldVFS $ closeVFS mempty n
  modify (\s -> s{vfs = newVFS})
  sendMessage n
sendNotification SMethod_TextDocumentDidChange params = do
  let n = L.TNotificationMessage "2.0" SMethod_TextDocumentDidChange params
  oldVFS <- vfs <$> get
  let newVFS = flip execState oldVFS $ changeFromClientVFS mempty n
  modify (\s -> s{vfs = newVFS})
  sendMessage n
sendNotification method params =
  case L.splitClientMethod method of
    L.IsClientNot -> sendMessage (L.TNotificationMessage "2.0" method params)
    L.IsClientEither -> sendMessage (L.NotMess $ L.TNotificationMessage "2.0" method params)

-- | Sends a response to the server.
sendResponse :: (ToJSON (L.MessageResult m), ToJSON (L.ErrorData m)) => L.TResponseMessage m -> Session ()
sendResponse = sendMessage

{- | Returns the initialize response that was received from the server.
 The initialize requests and responses are not included the session,
 so if you need to test it use this.
-}
initializeResponse :: Session (L.TResponseMessage Method_Initialize)
initializeResponse = ask >>= (liftIO . readMVar) . initRsp

setIgnoringLogNotifications :: Bool -> Session ()
setIgnoringLogNotifications value = do
  modify (\ss -> ss{ignoringLogNotifications = value})

setIgnoringConfigurationRequests :: Bool -> Session ()
setIgnoringConfigurationRequests value = do
  modify (\ss -> ss{ignoringConfigurationRequests = value})

setIgnoringRegistrationRequests :: Bool -> Session ()
setIgnoringRegistrationRequests value = do
  modify (\ss -> ss{ignoringRegistrationRequests = value})

{- | Modify the client config. This will send a notification to the server that the
 config has changed.
-}
modifyConfig :: (Object -> Object) -> Session ()
modifyConfig f = do
  oldConfig <- curLspConfig <$> get
  let newConfig = f oldConfig
  modify (\ss -> ss{curLspConfig = newConfig})

  -- We're going to be difficult and follow the new direction of the spec as much
  -- as possible. That means _not_ sending didChangeConfiguration notifications
  -- unless the server has registered for them
  registeredCaps <- getRegisteredCapabilities
  let
    requestedSections :: Maybe [T.Text]
    requestedSections = flip firstJust registeredCaps $ \(L.SomeRegistration (L.TRegistration _ regMethod regOpts)) ->
      case regMethod of
        SMethod_WorkspaceDidChangeConfiguration -> case regOpts of
          Just (L.DidChangeConfigurationRegistrationOptions{section = section}) -> case section of
            Just (InL s) -> Just [s]
            Just (InR ss) -> Just ss
            Nothing -> Nothing
          _ -> Nothing
        _ -> Nothing
    requestedSectionKeys :: Maybe [J.Key]
    requestedSectionKeys = (fmap . fmap) (fromString . T.unpack) requestedSections
  let configToSend = case requestedSectionKeys of
        Just ss -> Object $ J.filterWithKey (\k _ -> k `elem` ss) newConfig
        Nothing -> Object newConfig
  sendNotification SMethod_WorkspaceDidChangeConfiguration $ L.DidChangeConfigurationParams configToSend

{- | Set the client config. This will send a notification to the server that the
 config has changed.
-}
setConfig :: Object -> Session ()
setConfig newConfig = modifyConfig (const newConfig)

{- | Modify a client config section (if already present, otherwise does nothing).
 This will send a notification to the server that the config has changed.
-}
modifyConfigSection :: String -> (Value -> Value) -> Session ()
modifyConfigSection section f = modifyConfig (\o -> o & ix (fromString section) %~ f)

{- | Set a client config section. This will send a notification to the server that the
 config has changed.
-}
setConfigSection :: String -> Value -> Session ()
setConfigSection section settings = modifyConfig (\o -> o & at (fromString section) ?~ settings)

{- | /Creates/ a new text document. This is different from 'openDoc'
 as it sends a workspace/didChangeWatchedFiles notification letting the server
 know that a file was created within the workspace, __provided that the server
 has registered for it__, and the file matches any patterns the server
 registered for.
 It /does not/ actually create a file on disk, but is useful for convincing
 the server that one does exist.

 @since 11.0.0.0
-}
createDoc ::
  -- | The path to the document to open, __relative to the root directory__.
  FilePath ->
  -- | The text document's language identifier, e.g. @"haskell"@.
  L.LanguageKind ->
  -- | The content of the text document to create.
  T.Text ->
  -- | The identifier of the document just created.
  Session L.TextDocumentIdentifier
createDoc file languageId contents = do
  dynCaps <- curDynCaps <$> get
  rootDir <- asks rootDir
  caps <- asks sessionCapabilities
  absFile <- liftIO $ canonicalizePath (rootDir </> file)
  let pred :: L.SomeRegistration -> [L.TRegistration Method_WorkspaceDidChangeWatchedFiles]
      pred (L.SomeRegistration r@(L.TRegistration _ SMethod_WorkspaceDidChangeWatchedFiles _)) = [r]
      pred _ = mempty
      regs = concatMap pred $ Map.elems dynCaps
      watchHits :: L.FileSystemWatcher -> Bool
      watchHits (L.FileSystemWatcher (L.GlobPattern (InL (L.Pattern pattern))) kind) =
        -- If WatchKind is excluded, defaults to all true as per spec
        fileMatches (T.unpack pattern) && L.containsCreate (fromMaybe L.WatchKind_Create kind)
      -- TODO: Relative patterns
      watchHits _ = False

      fileMatches pattern = Glob.match (Glob.compile pattern) relOrAbs
       where
        -- If the pattern is absolute then match against the absolute fp
        relOrAbs
          | isAbsolute pattern = absFile
          | otherwise = file

      regHits :: L.TRegistration Method_WorkspaceDidChangeWatchedFiles -> Bool
      regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. field' @"registerOptions" . _Just . #watchers)

      clientCapsSupports =
        caps ^? #workspace . _Just . #didChangeWatchedFiles . _Just . #dynamicRegistration . _Just
          == Just True
      shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

  when shouldSend $
    sendNotification SMethod_WorkspaceDidChangeWatchedFiles $
      L.DidChangeWatchedFilesParams $
        [L.FileEvent (L.filePathToUri (rootDir </> file)) L.FileChangeType_Created]
  openDoc' file languageId contents

{- | Opens a text document that /exists on disk/, and sends a
 textDocument/didOpen notification to the server.
-}
openDoc :: FilePath -> L.LanguageKind -> Session L.TextDocumentIdentifier
openDoc file languageId = do
  context <- ask
  let fp = rootDir context </> file
  contents <- liftIO $ T.readFile fp
  openDoc' file languageId contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: FilePath -> L.LanguageKind -> T.Text -> Session L.TextDocumentIdentifier
openDoc' file languageId contents = do
  context <- ask
  let fp = rootDir context </> file
      uri = L.filePathToUri fp
      item = L.TextDocumentItem uri languageId 0 contents
  sendNotification SMethod_TextDocumentDidOpen (L.DidOpenTextDocumentParams item)
  pure $ L.TextDocumentIdentifier uri

-- | Closes a text document and sends a textDocument/didOpen notification to the server.
closeDoc :: L.TextDocumentIdentifier -> Session ()
closeDoc docId = do
  let params = L.DidCloseTextDocumentParams (L.TextDocumentIdentifier (docId.uri))
  sendNotification SMethod_TextDocumentDidClose params

-- | Changes a text document and sends a textDocument/didOpen notification to the server.
changeDoc :: L.TextDocumentIdentifier -> [L.TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
  verDoc <- getVersionedDoc docId
  let params = L.DidChangeTextDocumentParams (verDoc & #version +~ 1) changes
  sendNotification SMethod_TextDocumentDidChange params

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session L.Uri
getDocUri file = do
  context <- ask
  let fp = rootDir context </> file
  return $ L.filePathToUri fp

-- | Waits for diagnostics to be published and returns them.
waitForDiagnostics :: Session [L.Diagnostic]
waitForDiagnostics = do
  diagsNot <- skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics)
  let diags = diagsNot.params.diagnostics
  return diags

{- | The same as 'waitForDiagnostics', but will only match a specific
 'Language.LSP.Types._source'.
-}
waitForDiagnosticsSource :: String -> Session [L.Diagnostic]
waitForDiagnosticsSource src = do
  diags <- waitForDiagnostics
  let res = filter matches diags
  if null res
    then waitForDiagnosticsSource src
    else return res
 where
  matches :: L.Diagnostic -> Bool
  matches d = d.source == Just (T.pack src)

{- | Expects a 'PublishDiagnosticsNotification' and throws an
 'UnexpectedDiagnostics' exception if there are any diagnostics
 returned.
-}
noDiagnostics :: Session ()
noDiagnostics = do
  diagsNot <- message SMethod_TextDocumentPublishDiagnostics
  when (diagsNot.params.diagnostics /= []) $ liftIO $ throw UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: L.TextDocumentIdentifier -> Session (Either [L.SymbolInformation] [L.DocumentSymbol])
getDocumentSymbols doc = do
  L.TResponseMessage _ rspLid res <- request SMethod_TextDocumentDocumentSymbol (L.DocumentSymbolParams Nothing Nothing doc)
  case res of
    Right (InL xs) -> return (Left xs)
    Right (InR (InL xs)) -> return (Right xs)
    Right (InR (InR _)) -> return (Right [])
    Left err -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust rspLid) err)

-- | Returns the code actions in the specified range.
getCodeActions :: L.TextDocumentIdentifier -> L.Range -> Session [L.Command |? L.CodeAction]
getCodeActions doc range = do
  ctx <- getCodeActionContextInRange doc range
  rsp <- request SMethod_TextDocumentCodeAction (L.CodeActionParams Nothing Nothing doc range ctx)

  case rsp.result of
    Right (InL xs) -> return xs
    Right (InR _) -> return []
    Left error -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust $ rsp.id) error)

{- | Returns the code actions in the specified range, resolving any with
 a non empty _data_ field.
-}
getAndResolveCodeActions :: L.TextDocumentIdentifier -> L.Range -> Session [L.Command |? L.CodeAction]
getAndResolveCodeActions doc range = do
  items <- getCodeActions doc range
  for items $ \case
    l@(InL _) -> pure l
    (InR r) | isJust (r.data_) -> InR <$> resolveCodeAction r
    r@(InR _) -> pure r

{- | Returns all the code actions in a document by
 querying the code actions at each of the current
 diagnostics' positions.
-}
getAllCodeActions :: L.TextDocumentIdentifier -> Session [L.Command |? L.CodeAction]
getAllCodeActions doc = do
  ctx <- getCodeActionContext doc

  foldM (go ctx) [] =<< getCurrentDiagnostics doc
 where
  go :: L.CodeActionContext -> [L.Command |? L.CodeAction] -> L.Diagnostic -> Session [L.Command |? L.CodeAction]
  go ctx acc diag = do
    L.TResponseMessage _ rspLid res <- request SMethod_TextDocumentCodeAction (L.CodeActionParams Nothing Nothing doc (diag.range) ctx)

    case res of
      Left e -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust rspLid) e)
      Right (InL cmdOrCAs) -> pure (acc ++ cmdOrCAs)
      Right (InR _) -> pure acc

getCodeActionContextInRange :: L.TextDocumentIdentifier -> L.Range -> Session L.CodeActionContext
getCodeActionContextInRange doc caRange = do
  curDiags <- getCurrentDiagnostics doc
  let diags =
        [ d | d@L.Diagnostic{range = range} <- curDiags, overlappingRange caRange range
        ]
  return $ L.CodeActionContext diags Nothing Nothing
 where
  overlappingRange :: L.Range -> L.Range -> Bool
  overlappingRange (L.Range s e) range =
    positionInRange s range
      || positionInRange e range

  positionInRange :: L.Position -> L.Range -> Bool
  positionInRange (L.Position pl po) (L.Range (L.Position sl so) (L.Position el eo)) =
    pl > sl && pl < el
      || pl == sl && pl == el && po >= so && po <= eo
      || pl == sl && po >= so
      || pl == el && po <= eo

getCodeActionContext :: L.TextDocumentIdentifier -> Session L.CodeActionContext
getCodeActionContext doc = do
  curDiags <- getCurrentDiagnostics doc
  return $ L.CodeActionContext curDiags Nothing Nothing

{- | Returns the current diagnostics that have been sent to the client.
 Note that this does not wait for more to come in.
-}
getCurrentDiagnostics :: L.TextDocumentIdentifier -> Session [L.Diagnostic]
getCurrentDiagnostics doc = fromMaybe [] . Map.lookup (L.toNormalizedUri $ doc.uri) . curDiagnostics <$> get

-- | Returns the tokens of all progress sessions that have started but not yet ended.
getIncompleteProgressSessions :: Session (Set.Set L.ProgressToken)
getIncompleteProgressSessions = curProgressSessions <$> get

-- | Executes a command.
executeCommand :: L.Command -> Session ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd.arguments
      execParams = L.ExecuteCommandParams Nothing (cmd.command) args
  void $ sendRequest SMethod_WorkspaceExecuteCommand execParams

{- | Executes a code action.
 Matching with the specification, if a code action
 contains both an edit and a command, the edit will
 be applied first.
-}
executeCodeAction :: L.CodeAction -> Session ()
executeCodeAction action = do
  maybe (return ()) handleEdit $ action.edit
  maybe (return ()) executeCommand $ action.command
 where
  handleEdit :: L.WorkspaceEdit -> Session ()
  handleEdit e =
    -- Its ok to pass in dummy parameters here as they aren't used
    let req = L.TRequestMessage "" (L.IdInt 0) SMethod_WorkspaceApplyEdit (L.ApplyWorkspaceEditParams Nothing e)
     in updateState (L.FromServerMess SMethod_WorkspaceApplyEdit req)

-- | Resolves the provided code action.
resolveCodeAction :: L.CodeAction -> Session L.CodeAction
resolveCodeAction ca = do
  rsp <- request SMethod_CodeActionResolve ca
  case rsp.result of
    Right ca -> return ca
    Left er -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust $ rsp.id) er)

{- | If a code action contains a _data_ field: resolves the code action, then
 executes it. Otherwise, just executes it.
-}
resolveAndExecuteCodeAction :: L.CodeAction -> Session ()
resolveAndExecuteCodeAction ca@L.CodeAction{data_ = Just _} = do
  caRsp <- resolveCodeAction ca
  executeCodeAction caRsp
resolveAndExecuteCodeAction ca = executeCodeAction ca

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: L.TextDocumentIdentifier -> Session L.VersionedTextDocumentIdentifier
getVersionedDoc (L.TextDocumentIdentifier uri) = do
  vfs <- vfs <$> get
  let ver = vfs ^? #vfsMap . ix (L.toNormalizedUri uri) . to virtualFileVersion
  -- TODO: is this correct? Could return an OptionalVersionedTextDocumentIdentifier,
  -- but that complicated callers...
  return (L.VersionedTextDocumentIdentifier uri (fromMaybe 0 ver))

-- | Applys an edit to the document and returns the updated document version.
applyEdit :: L.TextDocumentIdentifier -> L.TextEdit -> Session L.VersionedTextDocumentIdentifier
applyEdit doc edit = do
  verDoc <- getVersionedDoc doc

  caps <- asks sessionCapabilities

  let supportsDocChanges = fromMaybe False $ caps ^? #workspace . _Just . #workspaceEdit . _Just . #documentChanges . _Just

  let wEdit =
        if supportsDocChanges
          then
            let docEdit = L.TextDocumentEdit (review L.versionedTextDocumentIdentifier verDoc) [InL edit]
             in L.WorkspaceEdit Nothing (Just [InL docEdit]) Nothing
          else
            let changes = Map.singleton (doc.uri) [edit]
             in L.WorkspaceEdit (Just changes) Nothing Nothing

  let req = L.TRequestMessage "" (L.IdInt 0) SMethod_WorkspaceApplyEdit (L.ApplyWorkspaceEditParams Nothing wEdit)
  updateState (L.FromServerMess SMethod_WorkspaceApplyEdit req)

  -- version may have changed
  getVersionedDoc doc

-- | Returns the completions for the position in the document.
getCompletions :: L.TextDocumentIdentifier -> L.Position -> Session [L.CompletionItem]
getCompletions doc pos = do
  rsp <- request SMethod_TextDocumentCompletion (L.CompletionParams doc pos Nothing Nothing Nothing)

  case getResponseResult rsp of
    InL items -> return items
    InR (InL c) -> return $ c.items
    InR (InR _) -> return []

{- | Returns the completions for the position in the document, resolving any with
 a non empty _data_ field.
-}
getAndResolveCompletions :: L.TextDocumentIdentifier -> L.Position -> Session [L.CompletionItem]
getAndResolveCompletions doc pos = do
  items <- getCompletions doc pos
  for items $ \item -> if isJust (item.data_) then resolveCompletion item else pure item

-- | Resolves the provided completion item.
resolveCompletion :: L.CompletionItem -> Session L.CompletionItem
resolveCompletion ci = do
  rsp <- request SMethod_CompletionItemResolve ci
  case rsp.result of
    Right ci -> return ci
    Left error -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust $ rsp.id) error)

-- | Returns the references for the position in the document.
getReferences ::
  -- | The document to lookup in.
  L.TextDocumentIdentifier ->
  -- | The position to lookup.
  L.Position ->
  -- | Whether to include declarations as references.
  Bool ->
  -- | The locations of the references.
  Session [L.Location]
getReferences doc pos inclDecl =
  let ctx = L.ReferenceContext inclDecl
      params = L.ReferenceParams doc pos Nothing Nothing ctx
   in L.absorbNull . getResponseResult <$> request SMethod_TextDocumentReferences params

-- | Returns the declarations(s) for the term at the specified position.
getDeclarations ::
  -- | The document the term is in.
  L.TextDocumentIdentifier ->
  -- | The position the term is at.
  L.Position ->
  Session (L.Declaration |? [L.DeclarationLink] |? L.Null)
getDeclarations doc pos = do
  rsp <- request SMethod_TextDocumentDeclaration (L.DeclarationParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the definition(s) for the term at the specified position.
getDefinitions ::
  -- | The document the term is in.
  L.TextDocumentIdentifier ->
  -- | The position the term is at.
  L.Position ->
  Session (L.Definition |? [L.DefinitionLink] |? L.Null)
getDefinitions doc pos = do
  rsp <- request SMethod_TextDocumentDefinition (L.DefinitionParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the type definition(s) for the term at the specified position.
getTypeDefinitions ::
  -- | The document the term is in.
  L.TextDocumentIdentifier ->
  -- | The position the term is at.
  L.Position ->
  Session (L.Definition |? [L.DefinitionLink] |? L.Null)
getTypeDefinitions doc pos = do
  rsp <- request SMethod_TextDocumentTypeDefinition (L.TypeDefinitionParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Returns the type definition(s) for the term at the specified position.
getImplementations ::
  -- | The document the term is in.
  L.TextDocumentIdentifier ->
  -- | The position the term is at.
  L.Position ->
  Session (L.Definition |? [L.DefinitionLink] |? L.Null)
getImplementations doc pos = do
  rsp <- request SMethod_TextDocumentImplementation (L.ImplementationParams doc pos Nothing Nothing)
  pure $ getResponseResult rsp

-- | Renames the term at the specified position.
rename :: L.TextDocumentIdentifier -> L.Position -> String -> Session ()
rename doc pos newName = do
  let params = L.RenameParams Nothing doc pos (T.pack newName)
  rsp <- request SMethod_TextDocumentRename params
  let wEdit = getResponseResult rsp
  case L.nullToMaybe wEdit of
    Just e -> do
      let req = L.TRequestMessage "" (L.IdInt 0) SMethod_WorkspaceApplyEdit (L.ApplyWorkspaceEditParams Nothing e)
      updateState (L.FromServerMess SMethod_WorkspaceApplyEdit req)
    Nothing -> pure ()

-- | Returns the hover information at the specified position.
getHover :: L.TextDocumentIdentifier -> L.Position -> Session (Maybe L.Hover)
getHover doc pos =
  let params = L.HoverParams doc pos Nothing
   in L.nullToMaybe . getResponseResult <$> request SMethod_TextDocumentHover params

-- | Returns the highlighted occurrences of the term at the specified position
getHighlights :: L.TextDocumentIdentifier -> L.Position -> Session [L.DocumentHighlight]
getHighlights doc pos =
  let params = L.DocumentHighlightParams doc pos Nothing Nothing
   in L.absorbNull . getResponseResult <$> request SMethod_TextDocumentDocumentHighlight params

{- | Checks the response for errors and throws an exception if needed.
 Returns the result if successful.
-}
getResponseResult :: (ToJSON (L.ErrorData m)) => L.TResponseMessage m -> L.MessageResult m
getResponseResult rsp =
  case rsp.result of
    Right x -> x
    Left err -> throw $ UnexpectedResponseError (L.SomeLspId $ fromJust $ rsp.id) err

-- | Applies formatting to the specified document.
formatDoc :: L.TextDocumentIdentifier -> L.FormattingOptions -> Session ()
formatDoc doc opts = do
  let params = L.DocumentFormattingParams Nothing doc opts
  edits <- L.absorbNull . getResponseResult <$> request SMethod_TextDocumentFormatting params
  applyTextEdits doc edits

-- | Applies formatting to the specified range in a document.
formatRange :: L.TextDocumentIdentifier -> L.FormattingOptions -> L.Range -> Session ()
formatRange doc opts range = do
  let params = L.DocumentRangeFormattingParams Nothing doc range opts
  edits <- L.absorbNull . getResponseResult <$> request SMethod_TextDocumentRangeFormatting params
  applyTextEdits doc edits

applyTextEdits :: L.TextDocumentIdentifier -> [L.TextEdit] -> Session ()
applyTextEdits doc edits =
  let wEdit = L.WorkspaceEdit (Just (Map.singleton (doc.uri) edits)) Nothing Nothing
      -- Send a dummy message to updateState so it can do bookkeeping
      req = L.TRequestMessage "" (L.IdInt 0) SMethod_WorkspaceApplyEdit (L.ApplyWorkspaceEditParams Nothing wEdit)
   in updateState (L.FromServerMess SMethod_WorkspaceApplyEdit req)

-- | Returns the code lenses for the specified document.
getCodeLenses :: L.TextDocumentIdentifier -> Session [L.CodeLens]
getCodeLenses tId = do
  rsp <- request SMethod_TextDocumentCodeLens (L.CodeLensParams Nothing Nothing tId)
  pure $ L.absorbNull $ getResponseResult rsp

{- | Returns the code lenses for the specified document, resolving any with
 a non empty _data_ field.
-}
getAndResolveCodeLenses :: L.TextDocumentIdentifier -> Session [L.CodeLens]
getAndResolveCodeLenses tId = do
  codeLenses <- getCodeLenses tId
  for codeLenses $ \codeLens -> if isJust (codeLens.data_) then resolveCodeLens codeLens else pure codeLens

-- | Resolves the provided code lens.
resolveCodeLens :: L.CodeLens -> Session L.CodeLens
resolveCodeLens cl = do
  rsp <- request SMethod_CodeLensResolve cl
  case rsp.result of
    Right cl -> return cl
    Left error -> throw (UnexpectedResponseError (L.SomeLspId $ fromJust $ rsp.id) error)

-- | Pass a param and return the response from `prepareCallHierarchy`
prepareCallHierarchy :: L.CallHierarchyPrepareParams -> Session [L.CallHierarchyItem]
prepareCallHierarchy = resolveRequestWithListResp SMethod_TextDocumentPrepareCallHierarchy

incomingCalls :: L.CallHierarchyIncomingCallsParams -> Session [L.CallHierarchyIncomingCall]
incomingCalls = resolveRequestWithListResp SMethod_CallHierarchyIncomingCalls

outgoingCalls :: L.CallHierarchyOutgoingCallsParams -> Session [L.CallHierarchyOutgoingCall]
outgoingCalls = resolveRequestWithListResp SMethod_CallHierarchyOutgoingCalls

-- | Send a request and receive a response with list.
resolveRequestWithListResp ::
  forall (m :: Method ClientToServer Request) a.
  (ToJSON (L.ErrorData m), L.MessageResult m ~ ([a] |? L.Null)) =>
  SMethod m ->
  L.MessageParams m ->
  Session [a]
resolveRequestWithListResp method params = do
  rsp <- request method params
  pure $ L.absorbNull $ getResponseResult rsp

-- | Pass a param and return the response from `semanticTokensFull`
getSemanticTokens :: L.TextDocumentIdentifier -> Session (L.SemanticTokens |? L.Null)
getSemanticTokens doc = do
  let params = L.SemanticTokensParams Nothing Nothing doc
  rsp <- request SMethod_TextDocumentSemanticTokensFull params
  pure $ getResponseResult rsp

{- | Returns a list of capabilities that the server has requested to /dynamically/
 register during the 'Session'.

 @since 0.11.0.0
-}
getRegisteredCapabilities :: Session [L.SomeRegistration]
getRegisteredCapabilities = Map.elems . curDynCaps <$> get
