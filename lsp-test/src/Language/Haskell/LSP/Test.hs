{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module      : Language.Haskell.LSP.Test
Description : A functional testing framework for LSP servers.
Maintainer  : luke_lau@icloud.com
Stability   : experimental
Portability : non-portable

Provides the framework to start functionally testing
<https://github.com/Microsoft/language-server-protocol Language Server Protocol servers>.
You should import "Language.Haskell.LSP.Types" alongside this.
-}
module Language.Haskell.LSP.Test
  (
  -- * Sessions
    Session
  , runSession
  -- ** Config
  , runSessionWithConfig
  , SessionConfig(..)
  , defaultConfig
  , C.fullCaps
  -- ** Exceptions
  , module Language.Haskell.LSP.Test.Exceptions
  , withTimeout
  -- * Sending
  , request
  , request_
  , sendRequest
  , sendNotification
  , sendResponse
  -- * Receving
  , module Language.Haskell.LSP.Test.Parsing
  -- * Utilities
  -- | Quick helper functions for common tasks.

  -- ** Initialization
  , initializeResponse
  -- ** Documents
  , openDoc
  , openDoc'
  , closeDoc
  , changeDoc
  , documentContents
  , getDocumentEdit
  , getDocUri
  , getVersionedDoc
  -- ** Symbols
  , getDocumentSymbols
  -- ** Diagnostics
  , waitForDiagnostics
  , waitForDiagnosticsSource
  , noDiagnostics
  , getCurrentDiagnostics
  -- ** Commands
  , executeCommand
  -- ** Code Actions
  , getCodeActions
  , getAllCodeActions
  , executeCodeAction
  -- ** Completions
  , getCompletions
  -- ** References
  , getReferences
  -- ** Definitions
  , getDefinitions
  , getTypeDefinitions
  -- ** Renaming
  , rename
  -- ** Hover
  , getHover
  -- ** Highlights
  , getHighlights
  -- ** Formatting
  , formatDoc
  , formatRange
  -- ** Edits
  , applyEdit
  -- ** Code lenses
  , getCodeLenses
  ) where

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Lens hiding ((.=), List)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding
  (id, capabilities, message, executeCommand, applyEdit, rename)
import qualified Language.Haskell.LSP.Types.Lens as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import Language.Haskell.LSP.Test.Parsing
import Language.Haskell.LSP.Test.Session
import Language.Haskell.LSP.Test.Server
import System.Environment
import System.IO
import System.Directory
import System.FilePath

-- | Starts a new session.
--
-- > runSession "hie" fullCaps "path/to/root/dir" $ do
-- >   doc <- openDoc "Desktop/simple.hs" "haskell"
-- >   diags <- waitForDiagnostics
-- >   let pos = Position 12 5
-- >       params = TextDocumentPositionParams doc
-- >   hover <- request TextDocumentHover params
runSession :: String -- ^ The command to run the server.
           -> C.ClientCapabilities -- ^ The capabilities that the client should declare.
           -> FilePath -- ^ The filepath to the root directory for the session.
           -> Session a -- ^ The session to run.
           -> IO a
runSession = runSessionWithConfig def

-- | Starts a new sesion with a custom configuration.
runSessionWithConfig :: SessionConfig -- ^ Configuration options for the session.
                     -> String -- ^ The command to run the server.
                     -> C.ClientCapabilities -- ^ The capabilities that the client should declare.
                     -> FilePath -- ^ The filepath to the root directory for the session.
                     -> Session a -- ^ The session to run.
                     -> IO a
runSessionWithConfig config' serverExe caps rootDir session = do
  pid <- getCurrentProcessID
  absRootDir <- canonicalizePath rootDir

  config <- envOverrideConfig config'

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          caps
                                          (Just TraceOff)
                                          Nothing
  withServer serverExe (logStdErr config) $ \serverIn serverOut serverProc ->
    runSessionWithHandles serverIn serverOut serverProc listenServer config caps rootDir exitServer $ do
      -- Wrap the session around initialize and shutdown calls
      initRspMsg <- request Initialize initializeParams :: Session InitializeResponse

      liftIO $ maybe (return ()) (putStrLn . ("Error while initializing: " ++) . show ) (initRspMsg ^. LSP.error)

      initRspVar <- initRsp <$> ask
      liftIO $ putMVar initRspVar initRspMsg
      sendNotification Initialized InitializedParams

      case lspConfig config of
        Just cfg -> sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams cfg)
        Nothing -> return ()

      -- Run the actual test
      session
  where
  -- | Asks the server to shutdown and exit politely
  exitServer :: Session ()
  exitServer = request_ Shutdown (Nothing :: Maybe Value) >> sendNotification Exit ExitParams

  -- | Listens to the server output until the shutdown ack,
  -- makes sure it matches the record and signals any semaphores
  listenServer :: Handle -> SessionContext -> IO ()
  listenServer serverOut context = do
    msgBytes <- getNextMessage serverOut

    reqMap <- readMVar $ requestMap context

    let msg = decodeFromServerMsg reqMap msgBytes
    writeChan (messageChan context) (ServerMessage msg)

    case msg of
      (RspShutdown _) -> return ()
      _               -> listenServer serverOut context

  -- | Check environment variables to override the config
  envOverrideConfig :: SessionConfig -> IO SessionConfig
  envOverrideConfig cfg = do
    logMessages' <- fromMaybe (logMessages cfg) <$> checkEnv "LSP_TEST_LOG_MESSAGES"
    logStdErr' <- fromMaybe (logStdErr cfg) <$> checkEnv "LSP_TEST_LOG_STDERR"
    return $ cfg { logMessages = logMessages', logStdErr = logStdErr' }
    where checkEnv :: String -> IO (Maybe Bool)
          checkEnv s = fmap convertVal <$> lookupEnv s
          convertVal "0" = False
          convertVal _ = True

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session T.Text
documentContents doc = do
  vfs <- vfs <$> get
  let file = vfsMap vfs Map.! toNormalizedUri (doc ^. uri)
  return (virtualFileText file)

-- | Parses an ApplyEditRequest, checks that it is for the passed document
-- and returns the new content
getDocumentEdit :: TextDocumentIdentifier -> Session T.Text
getDocumentEdit doc = do
  req <- message :: Session ApplyWorkspaceEditRequest

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $ throw (IncorrectApplyEditRequest (show req))

  documentContents doc
  where
    checkDocumentChanges :: ApplyWorkspaceEditRequest -> Bool
    checkDocumentChanges req =
      let changes = req ^. params . edit . documentChanges
          maybeDocs = fmap (fmap (^. textDocument . uri)) changes
      in case maybeDocs of
        Just docs -> (doc ^. uri) `elem` docs
        Nothing -> False
    checkChanges :: ApplyWorkspaceEditRequest -> Bool
    checkChanges req =
      let mMap = req ^. params . edit . changes
        in maybe False (HashMap.member (doc ^. uri)) mMap

-- | Sends a request to the server and waits for its response.
-- Will skip any messages in between the request and the response
-- @
-- rsp <- request TextDocumentDocumentSymbol params :: Session DocumentSymbolsResponse
-- @
-- Note: will skip any messages in between the request and the response.
request :: (ToJSON params, FromJSON a) => ClientMethod -> params -> Session (ResponseMessage a)
request m = sendRequest m >=> skipManyTill anyMessage . responseForId

-- | The same as 'sendRequest', but discard the response.
request_ :: ToJSON params => ClientMethod -> params -> Session ()
request_ p = void . (request p :: ToJSON params => params -> Session (ResponseMessage Value))

-- | Sends a request to the server. Unlike 'request', this doesn't wait for the response.
sendRequest
  :: ToJSON params
  => ClientMethod -- ^ The request method.
  -> params -- ^ The request parameters.
  -> Session LspId -- ^ The id of the request that was sent.
sendRequest method params = do
  id <- curReqId <$> get
  modify $ \c -> c { curReqId = nextId id }

  let req = RequestMessage' "2.0" id method params

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r id method

  sendMessage req

  return id

  where nextId (IdInt i) = IdInt (i + 1)
        nextId (IdString s) = IdString $ T.pack $ show $ read (T.unpack s) + 1

-- | A custom type for request message that doesn't
-- need a response type, allows us to infer the request
-- message type without using proxies.
data RequestMessage' a = RequestMessage' T.Text LspId ClientMethod a

instance ToJSON a => ToJSON (RequestMessage' a) where
  toJSON (RequestMessage' rpc id method params) =
    object ["jsonrpc" .= rpc, "id" .= id, "method" .= method, "params" .= params]


-- | Sends a notification to the server.
sendNotification :: ToJSON a
                 => ClientMethod -- ^ The notification method.
                 -> a -- ^ The notification parameters.
                 -> Session ()

-- Open a virtual file if we send a did open text document notification
sendNotification TextDocumentDidOpen params = do
  let params' = fromJust $ decode $ encode params
      n :: DidOpenTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidOpen params'
  oldVFS <- vfs <$> get
  let (newVFS,_) = openVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendMessage n

-- Close a virtual file if we send a close text document notification
sendNotification TextDocumentDidClose params = do
  let params' = fromJust $ decode $ encode params
      n :: DidCloseTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidClose params'
  oldVFS <- vfs <$> get
  let (newVFS,_) = closeVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendMessage n

sendNotification TextDocumentDidChange params = do
    let params' = fromJust $ decode $ encode params
        n :: DidChangeTextDocumentNotification
        n = NotificationMessage "2.0" TextDocumentDidChange params'
    oldVFS <- vfs <$> get
    let (newVFS,_) = changeFromClientVFS oldVFS n
    modify (\s -> s { vfs = newVFS })
    sendMessage n

sendNotification method params = sendMessage (NotificationMessage "2.0" method params)

-- | Sends a response to the server.
sendResponse :: ToJSON a => ResponseMessage a -> Session ()
sendResponse = sendMessage

-- | Returns the initialize response that was received from the server.
-- The initialize requests and responses are not included the session,
-- so if you need to test it use this.
initializeResponse :: Session InitializeResponse
initializeResponse = initRsp <$> ask >>= (liftIO . readMVar)

-- | Opens a text document and sends a notification to the client.
openDoc :: FilePath -> String -> Session TextDocumentIdentifier
openDoc file languageId = do
  context <- ask
  let fp = rootDir context </> file
  contents <- liftIO $ T.readFile fp
  openDoc' file languageId contents

-- | This is a variant of `openDoc` that takes the file content as an argument.
openDoc' :: FilePath -> String -> T.Text -> Session TextDocumentIdentifier
openDoc' file languageId contents = do
  context <- ask
  let fp = rootDir context </> file
      uri = filePathToUri fp
      item = TextDocumentItem uri (T.pack languageId) 0 contents
  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams item)
  pure $ TextDocumentIdentifier uri

-- | Closes a text document and sends a notification to the client.
closeDoc :: TextDocumentIdentifier -> Session ()
closeDoc docId = do
  let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
  sendNotification TextDocumentDidClose params

-- | Changes a text document and sends a notification to the client
changeDoc :: TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
  verDoc <- getVersionedDoc docId
  let params = DidChangeTextDocumentParams (verDoc & version . non 0 +~ 1) (List changes)
  sendNotification TextDocumentDidChange params

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
  context <- ask
  let fp = rootDir context </> file
  return $ filePathToUri fp

-- | Waits for diagnostics to be published and returns them.
waitForDiagnostics :: Session [Diagnostic]
waitForDiagnostics = do
  diagsNot <- skipManyTill anyMessage message :: Session PublishDiagnosticsNotification
  let (List diags) = diagsNot ^. params . LSP.diagnostics
  return diags

-- | The same as 'waitForDiagnostics', but will only match a specific
-- 'Language.Haskell.LSP.Types._source'.
waitForDiagnosticsSource :: String -> Session [Diagnostic]
waitForDiagnosticsSource src = do
  diags <- waitForDiagnostics
  let res = filter matches diags
  if null res
    then waitForDiagnosticsSource src
    else return res
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. source == Just (T.pack src)

-- | Expects a 'PublishDiagnosticsNotification' and throws an
-- 'UnexpectedDiagnostics' exception if there are any diagnostics
-- returned.
noDiagnostics :: Session ()
noDiagnostics = do
  diagsNot <- message :: Session PublishDiagnosticsNotification
  when (diagsNot ^. params . LSP.diagnostics /= List []) $ liftIO $ throw UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: TextDocumentIdentifier -> Session (Either [DocumentSymbol] [SymbolInformation])
getDocumentSymbols doc = do
  ResponseMessage _ rspLid mRes mErr <- request TextDocumentDocumentSymbol (DocumentSymbolParams doc Nothing) :: Session DocumentSymbolsResponse
  maybe (return ()) (throw . UnexpectedResponseError rspLid) mErr
  case mRes of
    Just (DSDocumentSymbols (List xs)) -> return (Left xs)
    Just (DSSymbolInformation (List xs)) -> return (Right xs)
    Nothing -> Prelude.error "No result and no error in DocumentSymbolsResponse"

-- | Returns the code actions in the specified range.
getCodeActions :: TextDocumentIdentifier -> Range -> Session [CAResult]
getCodeActions doc range = do
  ctx <- getCodeActionContext doc
  rsp <- request TextDocumentCodeAction (CodeActionParams doc range ctx Nothing)

  case rsp ^. result of
    Just (List xs) -> return xs
    _ -> throw (UnexpectedResponseError (rsp ^. LSP.id) (fromJust $ rsp ^. LSP.error))

-- | Returns all the code actions in a document by
-- querying the code actions at each of the current
-- diagnostics' positions.
getAllCodeActions :: TextDocumentIdentifier -> Session [CAResult]
getAllCodeActions doc = do
  ctx <- getCodeActionContext doc

  foldM (go ctx) [] =<< getCurrentDiagnostics doc

  where
    go :: CodeActionContext -> [CAResult] -> Diagnostic -> Session [CAResult]
    go ctx acc diag = do
      ResponseMessage _ rspLid mRes mErr <- request TextDocumentCodeAction (CodeActionParams doc (diag ^. range) ctx Nothing)

      case mErr of
        Just e -> throw (UnexpectedResponseError rspLid e)
        Nothing ->
          let Just (List cmdOrCAs) = mRes
            in return (acc ++ cmdOrCAs)

getCodeActionContext :: TextDocumentIdentifier -> Session CodeActionContext
getCodeActionContext doc = do
  curDiags <- getCurrentDiagnostics doc
  return $ CodeActionContext (List curDiags) Nothing

-- | Returns the current diagnostics that have been sent to the client.
-- Note that this does not wait for more to come in.
getCurrentDiagnostics :: TextDocumentIdentifier -> Session [Diagnostic]
getCurrentDiagnostics doc = fromMaybe [] . Map.lookup (toNormalizedUri $ doc ^. uri) . curDiagnostics <$> get

-- | Executes a command.
executeCommand :: Command -> Session ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams (cmd ^. command) args Nothing
  request_ WorkspaceExecuteCommand execParams

-- | Executes a code action.
-- Matching with the specification, if a code action
-- contains both an edit and a command, the edit will
-- be applied first.
executeCodeAction :: CodeAction -> Session ()
executeCodeAction action = do
  maybe (return ()) handleEdit $ action ^. edit
  maybe (return ()) executeCommand $ action ^. command

  where handleEdit :: WorkspaceEdit -> Session ()
        handleEdit e =
          -- Its ok to pass in dummy parameters here as they aren't used
          let req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams e)
            in updateState (ReqApplyWorkspaceEdit req)

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: TextDocumentIdentifier -> Session VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier uri) = do
  fs <- vfsMap . vfs <$> get
  let ver =
        case fs Map.!? toNormalizedUri uri of
          Just vf -> Just (virtualFileVersion vf)
          _ -> Nothing
  return (VersionedTextDocumentIdentifier uri ver)

-- | Applys an edit to the document and returns the updated document version.
applyEdit :: TextDocumentIdentifier -> TextEdit -> Session VersionedTextDocumentIdentifier
applyEdit doc edit = do

  verDoc <- getVersionedDoc doc

  caps <- asks sessionCapabilities

  let supportsDocChanges = fromMaybe False $ do
        let mWorkspace = C._workspace caps
        C.WorkspaceClientCapabilities _ mEdit _ _ _ _ _ _ <- mWorkspace
        C.WorkspaceEditClientCapabilities mDocChanges <- mEdit
        mDocChanges

  let wEdit = if supportsDocChanges
      then
        let docEdit = TextDocumentEdit verDoc (List [edit])
        in WorkspaceEdit Nothing (Just (List [docEdit]))
      else
        let changes = HashMap.singleton (doc ^. uri) (List [edit])
        in WorkspaceEdit (Just changes) Nothing

  let req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  updateState (ReqApplyWorkspaceEdit req)

  -- version may have changed
  getVersionedDoc doc

-- | Returns the completions for the position in the document.
getCompletions :: TextDocumentIdentifier -> Position -> Session [CompletionItem]
getCompletions doc pos = do
  rsp <- request TextDocumentCompletion (TextDocumentPositionParams doc pos Nothing)

  case getResponseResult rsp of
    Completions (List items) -> return items
    CompletionList (CompletionListType _ (List items)) -> return items

-- | Returns the references for the position in the document.
getReferences :: TextDocumentIdentifier -- ^ The document to lookup in.
              -> Position -- ^ The position to lookup.
              -> Bool -- ^ Whether to include declarations as references.
              -> Session [Location] -- ^ The locations of the references.
getReferences doc pos inclDecl =
  let ctx = ReferenceContext inclDecl
      params = ReferenceParams doc pos ctx Nothing
  in getResponseResult <$> request TextDocumentReferences params

-- | Returns the definition(s) for the term at the specified position.
getDefinitions :: TextDocumentIdentifier -- ^ The document the term is in.
               -> Position -- ^ The position the term is at.
               -> Session [Location] -- ^ The location(s) of the definitions
getDefinitions doc pos = do
  let params = TextDocumentPositionParams doc pos Nothing
  rsp <- request TextDocumentDefinition params :: Session DefinitionResponse
  case getResponseResult rsp of
      SingleLoc loc -> pure [loc]
      MultiLoc locs -> pure locs

-- | Returns the type definition(s) for the term at the specified position.
getTypeDefinitions :: TextDocumentIdentifier -- ^ The document the term is in.
               -> Position -- ^ The position the term is at.
               -> Session [Location] -- ^ The location(s) of the definitions
getTypeDefinitions doc pos =
  let params = TextDocumentPositionParams doc pos Nothing
  in getResponseResult <$> request TextDocumentTypeDefinition params

-- | Renames the term at the specified position.
rename :: TextDocumentIdentifier -> Position -> String -> Session ()
rename doc pos newName = do
  let params = RenameParams doc pos (T.pack newName) Nothing
  rsp <- request TextDocumentRename params :: Session RenameResponse
  let wEdit = getResponseResult rsp
      req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  updateState (ReqApplyWorkspaceEdit req)

-- | Returns the hover information at the specified position.
getHover :: TextDocumentIdentifier -> Position -> Session (Maybe Hover)
getHover doc pos =
  let params = TextDocumentPositionParams doc pos Nothing
  in getResponseResult <$> request TextDocumentHover params

-- | Returns the highlighted occurences of the term at the specified position
getHighlights :: TextDocumentIdentifier -> Position -> Session [DocumentHighlight]
getHighlights doc pos =
  let params = TextDocumentPositionParams doc pos Nothing
  in getResponseResult <$> request TextDocumentDocumentHighlight params

-- | Checks the response for errors and throws an exception if needed.
-- Returns the result if successful.
getResponseResult :: ResponseMessage a -> a
getResponseResult rsp = fromMaybe exc (rsp ^. result)
  where exc = throw $ UnexpectedResponseError (rsp ^. LSP.id)
                                              (fromJust $ rsp ^. LSP.error)

-- | Applies formatting to the specified document.
formatDoc :: TextDocumentIdentifier -> FormattingOptions -> Session ()
formatDoc doc opts = do
  let params = DocumentFormattingParams doc opts Nothing
  edits <- getResponseResult <$> request TextDocumentFormatting params
  applyTextEdits doc edits

-- | Applies formatting to the specified range in a document.
formatRange :: TextDocumentIdentifier -> FormattingOptions -> Range -> Session ()
formatRange doc opts range = do
  let params = DocumentRangeFormattingParams doc range opts Nothing
  edits <- getResponseResult <$> request TextDocumentRangeFormatting params
  applyTextEdits doc edits

applyTextEdits :: TextDocumentIdentifier -> List TextEdit -> Session ()
applyTextEdits doc edits =
  let wEdit = WorkspaceEdit (Just (HashMap.singleton (doc ^. uri) edits)) Nothing
      req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  in updateState (ReqApplyWorkspaceEdit req)

-- | Returns the code lenses for the specified document.
getCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getCodeLenses tId = do
    rsp <- request TextDocumentCodeLens (CodeLensParams tId Nothing) :: Session CodeLensResponse
    case getResponseResult rsp of
        List res -> pure res
