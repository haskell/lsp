{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Language.Haskell.LSP.Test
-- Description : A functional testing framework for LSP servers.
-- Maintainer  : luke_lau@icloud.com
-- Stability   : experimental
--
-- A framework for testing <https://github.com/Microsoft/language-server-protocol Language Server Protocol servers> at the JSON level.

module Language.Haskell.LSP.Test
  (
  -- * Sessions
    runSession
  , runSessionWithHandles
  , runSessionWithConfig
  , Session
  , SessionConfig(..)
  , SessionException(..)
  , anySessionException
  , withTimeout
  -- * Sending
  , sendRequest
  , sendRequest_
  , sendRequest'
  , sendNotification
  , sendRequestMessage
  , sendNotification'
  , sendResponse
  -- * Receving
  , message
  , anyRequest
  , anyResponse
  , anyNotification
  , anyMessage
  , loggingNotification
  , publishDiagnosticsNotification
  -- * Combinators
  , satisfy
  -- * Utilities
  , initializeResponse
  -- ** Documents
  , openDoc
  , documentContents
  , getDocumentEdit
  , getDocUri
  , getVersionedDoc
  -- ** Symbols
  , getDocumentSymbols
  -- ** Diagnostics
  , waitForDiagnostics
  , noDiagnostics
  -- ** Commands
  , executeCommand
  -- ** Code Actions
  , getAllCodeActions
  , executeCodeAction
  -- ** Completions
  , getCompletions
  -- ** Edits
  , applyEdit
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
import Language.Haskell.LSP.Types hiding (id, capabilities, message)
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import Language.Haskell.LSP.Test.Parsing
import Language.Haskell.LSP.Test.Session
import Language.Haskell.LSP.Test.Server
import System.IO
import System.Directory
import System.FilePath
import qualified Yi.Rope as Rope

-- | Starts a new session.
runSession :: String -- ^ The command to run the server.
           -> FilePath -- ^ The filepath to the root directory for the session.
           -> Session a -- ^ The session to run.
           -> IO a
runSession = runSessionWithConfig def

-- | Starts a new sesion with a client with the specified capabilities.
runSessionWithConfig :: SessionConfig -- ^ The capabilities the client should have.
                     -> String -- ^ The command to run the server.
                     -> FilePath -- ^ The filepath to the root directory for the session.
                     -> Session a -- ^ The session to run.
                     -> IO a
runSessionWithConfig config serverExe rootDir session = do
  pid <- getCurrentProcessID
  absRootDir <- canonicalizePath rootDir

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          (capabilities config)
                                          (Just TraceOff)
  withServer serverExe (logStdErr config) $ \serverIn serverOut _ ->
    runSessionWithHandles serverIn serverOut listenServer config rootDir $ do

      -- Wrap the session around initialize and shutdown calls
      initRspMsg <- sendRequest Initialize initializeParams :: Session InitializeResponse

      liftIO $ maybe (return ()) (putStrLn . ("Error while initializing: " ++) . show ) (initRspMsg ^. LSP.error)

      initRspVar <- initRsp <$> ask
      liftIO $ putMVar initRspVar initRspMsg

      sendNotification Initialized InitializedParams

      -- Run the actual test
      result <- session

      sendNotification Exit ExitParams

      return result
  where
  -- | Listens to the server output, makes sure it matches the record and
  -- signals any semaphores
  listenServer :: Handle -> SessionContext -> IO ()
  listenServer serverOut context = do
    msgBytes <- getNextMessage serverOut

    reqMap <- readMVar $ requestMap context

    let msg = decodeFromServerMsg reqMap msgBytes
    writeChan (messageChan context) (ServerMessage msg)

    listenServer serverOut context

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session T.Text
documentContents doc = do
  vfs <- vfs <$> get
  let file = vfs Map.! (doc ^. uri)
  return $ Rope.toText $ Language.Haskell.LSP.VFS._text file

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
-- @
-- rsp <- sendRequest TextDocumentDocumentSymbol params :: Session DocumentSymbolsResponse
-- @
-- Note: will skip any messages in between the request and the response.
sendRequest :: (ToJSON params, FromJSON a) => ClientMethod -> params -> Session (ResponseMessage a)
sendRequest m = sendRequest' m >=> skipManyTill anyMessage . responseForId

-- | Send a request to the server and wait for its response,
-- but discard it.
sendRequest_ :: ToJSON params => ClientMethod -> params -> Session ()
sendRequest_ p = void . (sendRequest p :: ToJSON params => params -> Session (ResponseMessage Value))

-- | Sends a request to the server without waiting on the response.
sendRequest'
  :: ToJSON params
  => ClientMethod -- ^ The request method.
  -> params -- ^ The request parameters.
  -> Session LspId -- ^ The id of the request that was sent.
sendRequest' method params = do
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


sendRequestMessage :: (ToJSON a, ToJSON b) => RequestMessage ClientMethod a b -> Session ()
sendRequestMessage req = do
  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r (req ^. LSP.id) (req ^. method)

  sendMessage req

-- | Sends a notification to the server.
sendNotification :: ToJSON a
                 => ClientMethod -- ^ The notification method.
                 -> a -- ^ The notification parameters.
                 -> Session ()

-- | Open a virtual file if we send a did open text document notification
sendNotification TextDocumentDidOpen params = do
  let params' = fromJust $ decode $ encode params
      n :: DidOpenTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidOpen params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ openVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

-- | Close a virtual file if we send a close text document notification
sendNotification TextDocumentDidClose params = do
  let params' = fromJust $ decode $ encode params
      n :: DidCloseTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidClose params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ closeVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

sendNotification method params = sendNotification' (NotificationMessage "2.0" method params)

sendNotification' :: (ToJSON a, ToJSON b) => NotificationMessage a b -> Session ()
sendNotification' = sendMessage

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
  item <- getDocItem file languageId
  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams item)
  TextDocumentIdentifier <$> getDocUri file
  where
  -- | Reads in a text document as the first version.
  getDocItem :: FilePath -- ^ The path to the text document to read in.
            -> String -- ^ The language ID, e.g "haskell" for .hs files.
            -> Session TextDocumentItem
  getDocItem file languageId = do
    context <- ask
    let fp = rootDir context </> file
    contents <- liftIO $ T.readFile fp
    return $ TextDocumentItem (filePathToUri fp) (T.pack languageId) 0 contents

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

-- | Expects a 'PublishDiagnosticsNotification' and throws an
-- 'UnexpectedDiagnosticsException' if there are any diagnostics
-- returned.
noDiagnostics :: Session ()
noDiagnostics = do
  diagsNot <- message :: Session PublishDiagnosticsNotification
  when (diagsNot ^. params . LSP.diagnostics /= List []) $ liftIO $ throw UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: TextDocumentIdentifier -> Session [SymbolInformation]
getDocumentSymbols doc = do
  ResponseMessage _ rspLid mRes mErr <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
  maybe (return ()) (throw . UnexpectedResponseError rspLid) mErr
  let (Just (List symbols)) = mRes
  return symbols

-- | Returns all the code actions in a document by 
-- querying the code actions at each of the current 
-- diagnostics' positions.
getAllCodeActions :: TextDocumentIdentifier -> Session [CommandOrCodeAction]
getAllCodeActions doc = do
  curDiags <- fromMaybe [] . Map.lookup (doc ^. uri) . curDiagnostics <$> get
  let ctx = CodeActionContext (List curDiags) Nothing

  foldM (go ctx) [] curDiags

  where
    go :: CodeActionContext -> [CommandOrCodeAction] -> Diagnostic -> Session [CommandOrCodeAction]
    go ctx acc diag = do
      ResponseMessage _ rspLid mRes mErr <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) ctx)

      case mErr of
        Just e -> throw (UnexpectedResponseError rspLid e)
        Nothing ->
          let Just (List cmdOrCAs) = mRes
            in return (acc ++ cmdOrCAs)

-- | Executes a command.
executeCommand :: Command -> Session ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams (cmd ^. command) args
  sendRequest_ WorkspaceExecuteCommand execParams

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
  fs <- vfs <$> get
  let ver =
        case fs Map.!? uri of
          Just (VirtualFile v _) -> Just v
          _ -> Nothing
  return (VersionedTextDocumentIdentifier uri ver)

-- | Applys an edit to the document and returns the updated document version.
applyEdit :: TextDocumentIdentifier -> TextEdit -> Session VersionedTextDocumentIdentifier
applyEdit doc edit = do

  verDoc <- getVersionedDoc doc

  caps <- asks (capabilities . config)

  let supportsDocChanges = fromMaybe False $ do
        let LSP.ClientCapabilities mWorkspace _ _ = caps
        LSP.WorkspaceClientCapabilities _ mEdit _ _ _ _ <- mWorkspace
        LSP.WorkspaceEditClientCapabilities mDocChanges <- mEdit
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
  rsp <- sendRequest TextDocumentCompletion (TextDocumentPositionParams doc pos)

  let exc = throw $ UnexpectedResponseError (rsp ^. LSP.id)
                                            (fromJust $ rsp ^. LSP.error)
      res = fromMaybe exc (rsp ^. result)
  case res of
    Completions (List items) -> return items
    CompletionList (CompletionListType _ (List items)) -> return items
