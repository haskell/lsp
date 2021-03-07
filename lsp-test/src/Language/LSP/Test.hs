{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Language.LSP.Test
Description : A functional testing framework for LSP servers.
Maintainer  : luke_lau@icloud.com
Stability   : experimental
Portability : non-portable

Provides the framework to start functionally testing
<https://github.com/Microsoft/language-server-protocol Language Server Protocol servers>.
You should import "Language.LSP.Types" alongside this.
-}
module Language.LSP.Test
  (
  -- * Sessions
    Session
  , runSession
  , runSessionWithConfig
  , runSessionWithHandles
  -- ** Config
  , SessionConfig(..)
  , defaultConfig
  , C.fullCaps
  -- ** Exceptions
  , module Language.LSP.Test.Exceptions
  , withTimeout
  -- * Sending
  , request
  , request_
  , sendRequest
  , sendNotification
  , sendResponse
  -- * Receving
  , module Language.LSP.Test.Parsing
  -- * Utilities
  -- | Quick helper functions for common tasks.

  -- ** Initialization
  , initializeResponse
  -- ** Documents
  , createDoc
  , openDoc
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
  , getIncompleteProgressSessions
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
  , getDeclarations
  , getDefinitions
  , getTypeDefinitions
  , getImplementations
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
  -- ** Capabilities
  , getRegisteredCapabilities
  ) where

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Lens hiding ((.=), List, Empty)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Language.LSP.Types
import Language.LSP.Types.Lens hiding
  (id, capabilities, message, executeCommand, applyEdit, rename)
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.Types.Capabilities as C
import Language.LSP.VFS
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Parsing
import Language.LSP.Test.Session
import Language.LSP.Test.Server
import System.Environment
import System.IO
import System.Directory
import System.FilePath
import System.Process (ProcessHandle)
import qualified System.FilePath.Glob as Glob

-- | Starts a new session.
--
-- > runSession "hie" fullCaps "path/to/root/dir" $ do
-- >   doc <- openDoc "Desktop/simple.hs" "haskell"
-- >   diags <- waitForDiagnostics
-- >   let pos = Position 12 5
-- >       params = TextDocumentPositionParams doc
-- >   hover <- request STextdocumentHover params
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
  config <- envOverrideConfig config'
  withServer serverExe (logStdErr config) $ \serverIn serverOut serverProc ->
    runSessionWithHandles' (Just serverProc) serverIn serverOut config caps rootDir session

-- | Starts a new session, using the specified handles to communicate with the
-- server. You can use this to host the server within the same process.
-- An example with lsp might look like:
--
-- > (hinRead, hinWrite) <- createPipe
-- > (houtRead, houtWrite) <- createPipe
-- > 
-- > forkIO $ void $ runServerWithHandles hinRead houtWrite serverDefinition
-- > runSessionWithHandles hinWrite houtRead defaultConfig fullCaps "." $ do
-- >   -- ...
runSessionWithHandles :: Handle -- ^ The input handle
                      -> Handle -- ^ The output handle
                      -> SessionConfig
                      -> C.ClientCapabilities -- ^ The capabilities that the client should declare.
                      -> FilePath -- ^ The filepath to the root directory for the session.
                      -> Session a -- ^ The session to run.
                      -> IO a
runSessionWithHandles = runSessionWithHandles' Nothing


runSessionWithHandles' :: Maybe ProcessHandle
                       -> Handle -- ^ The input handle
                       -> Handle -- ^ The output handle
                       -> SessionConfig
                       -> C.ClientCapabilities -- ^ The capabilities that the client should declare.
                       -> FilePath -- ^ The filepath to the root directory for the session.
                       -> Session a -- ^ The session to run.
                       -> IO a
runSessionWithHandles' serverProc serverIn serverOut config' caps rootDir session = do
  pid <- getCurrentProcessID
  absRootDir <- canonicalizePath rootDir

  config <- envOverrideConfig config'

  let initializeParams = InitializeParams Nothing
                                          (Just pid)
                                          (Just lspTestClientInfo)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          caps
                                          (Just TraceOff)
                                          (List <$> initialWorkspaceFolders config)
  runSession' serverIn serverOut serverProc listenServer config caps rootDir exitServer $ do
    -- Wrap the session around initialize and shutdown calls
    initReqId <- sendRequest SInitialize initializeParams

    -- Because messages can be sent in between the request and response,
    -- collect them and then...
    (inBetween, initRspMsg) <- manyTill_ anyMessage (responseForId SInitialize initReqId)

    case initRspMsg ^. LSP.result of
      Left error -> liftIO $ putStrLn ("Error while initializing: " ++ show error)
      Right _ -> pure ()

    initRspVar <- initRsp <$> ask
    liftIO $ putMVar initRspVar initRspMsg
    sendNotification SInitialized (Just InitializedParams)

    case lspConfig config of
      Just cfg -> sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams cfg)
      Nothing -> return ()

    -- ... relay them back to the user Session so they can match on them!
    -- As long as they are allowed.
    forM_ inBetween checkLegalBetweenMessage
    msgChan <- asks messageChan
    liftIO $ writeList2Chan msgChan (ServerMessage <$> inBetween)

    -- Run the actual test
    session
  where
  -- | Asks the server to shutdown and exit politely
  exitServer :: Session ()
  exitServer = request_ SShutdown Empty >> sendNotification SExit Empty

  -- | Listens to the server output until the shutdown ack,
  -- makes sure it matches the record and signals any semaphores
  listenServer :: Handle -> SessionContext -> IO ()
  listenServer serverOut context = do
    msgBytes <- getNextMessage serverOut

    msg <- modifyMVar (requestMap context) $ \reqMap ->
      pure $ decodeFromServerMsg reqMap msgBytes
    writeChan (messageChan context) (ServerMessage msg)

    case msg of
      (FromServerRsp SShutdown _) -> return ()
      _                           -> listenServer serverOut context

  -- | Is this message allowed to be sent by the server between the intialize
  -- request and response?
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
  checkLegalBetweenMessage :: FromServerMessage -> Session ()
  checkLegalBetweenMessage (FromServerMess SWindowShowMessage _) = pure ()
  checkLegalBetweenMessage (FromServerMess SWindowLogMessage _) = pure ()
  checkLegalBetweenMessage (FromServerMess STelemetryEvent _) = pure ()
  checkLegalBetweenMessage (FromServerMess SWindowShowMessageRequest _) = pure ()
  checkLegalBetweenMessage msg = throw (IllegalInitSequenceMessage msg)

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
  req <- message SWorkspaceApplyEdit

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $ throw (IncorrectApplyEditRequest (show req))

  documentContents doc
  where
    checkDocumentChanges req =
      let changes = req ^. params . edit . documentChanges
          maybeDocs = fmap (fmap documentChangeUri) changes
      in case maybeDocs of
        Just docs -> (doc ^. uri) `elem` docs
        Nothing -> False
    checkChanges req =
      let mMap = req ^. params . edit . changes
        in maybe False (HashMap.member (doc ^. uri)) mMap

-- | Sends a request to the server and waits for its response.
-- Will skip any messages in between the request and the response
-- @
-- rsp <- request STextDocumentDocumentSymbol params
-- @
-- Note: will skip any messages in between the request and the response.
request :: SClientMethod m -> MessageParams m -> Session (ResponseMessage m)
request m = sendRequest m >=> skipManyTill anyMessage . responseForId m

-- | The same as 'sendRequest', but discard the response.
request_ :: SClientMethod (m :: Method FromClient Request) -> MessageParams m -> Session ()
request_ p = void . request p

-- | Sends a request to the server. Unlike 'request', this doesn't wait for the response.
sendRequest
  :: SClientMethod m -- ^ The request method.
  -> MessageParams m -- ^ The request parameters.
  -> Session (LspId m) -- ^ The id of the request that was sent.
sendRequest method params = do
  idn <- curReqId <$> get
  modify $ \c -> c { curReqId = idn+1 }
  let id = IdInt idn

  let mess = RequestMessage "2.0" id method params

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ fromJust $ updateRequestMap r id method

  ~() <- case splitClientMethod method of
    IsClientReq -> sendMessage mess
    IsClientEither -> sendMessage $ ReqMess mess

  return id

-- | Sends a notification to the server.
sendNotification :: SClientMethod (m :: Method FromClient Notification) -- ^ The notification method.
                 -> MessageParams m -- ^ The notification parameters.
                 -> Session ()
-- Open a virtual file if we send a did open text document notification
sendNotification STextDocumentDidOpen params = do
  let n = NotificationMessage "2.0" STextDocumentDidOpen params
  oldVFS <- vfs <$> get
  let (newVFS,_) = openVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendMessage n

-- Close a virtual file if we send a close text document notification
sendNotification STextDocumentDidClose params = do
  let n = NotificationMessage "2.0" STextDocumentDidClose params
  oldVFS <- vfs <$> get
  let (newVFS,_) = closeVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendMessage n

sendNotification STextDocumentDidChange params = do
    let n = NotificationMessage "2.0" STextDocumentDidChange params
    oldVFS <- vfs <$> get
    let (newVFS,_) = changeFromClientVFS oldVFS n
    modify (\s -> s { vfs = newVFS })
    sendMessage n

sendNotification method params =
  case splitClientMethod method of
    IsClientNot -> sendMessage (NotificationMessage "2.0" method params)
    IsClientEither -> sendMessage (NotMess $ NotificationMessage "2.0" method params)

-- | Sends a response to the server.
sendResponse :: ToJSON (ResponseResult m) => ResponseMessage m -> Session ()
sendResponse = sendMessage

-- | Returns the initialize response that was received from the server.
-- The initialize requests and responses are not included the session,
-- so if you need to test it use this.
initializeResponse :: Session (ResponseMessage Initialize)
initializeResponse = ask >>= (liftIO . readMVar) . initRsp

-- | /Creates/ a new text document. This is different from 'openDoc'
-- as it sends a workspace/didChangeWatchedFiles notification letting the server
-- know that a file was created within the workspace, __provided that the server
-- has registered for it__, and the file matches any patterns the server
-- registered for.
-- It /does not/ actually create a file on disk, but is useful for convincing
-- the server that one does exist.
--
-- @since 11.0.0.0
createDoc :: FilePath -- ^ The path to the document to open, __relative to the root directory__.
          -> T.Text -- ^ The text document's language identifier, e.g. @"haskell"@.
          -> T.Text -- ^ The content of the text document to create.
          -> Session TextDocumentIdentifier -- ^ The identifier of the document just created.
createDoc file languageId contents = do
  dynCaps <- curDynCaps <$> get
  rootDir <- asks rootDir
  caps <- asks sessionCapabilities
  absFile <- liftIO $ canonicalizePath (rootDir </> file)
  let pred :: SomeRegistration -> [Registration WorkspaceDidChangeWatchedFiles]
      pred (SomeRegistration r@(Registration _ SWorkspaceDidChangeWatchedFiles _)) = [r]
      pred _ = mempty
      regs = concatMap pred $ Map.elems dynCaps
      watchHits :: FileSystemWatcher -> Bool
      watchHits (FileSystemWatcher pattern kind) =
        -- If WatchKind is exlcuded, defaults to all true as per spec
        fileMatches (T.unpack pattern) && createHits (fromMaybe (WatchKind True True True) kind)

      fileMatches pattern = Glob.match (Glob.compile pattern) relOrAbs
        -- If the pattern is absolute then match against the absolute fp
        where relOrAbs
                | isAbsolute pattern = absFile
                | otherwise = file

      createHits (WatchKind create _ _) = create

      regHits :: Registration WorkspaceDidChangeWatchedFiles -> Bool
      regHits reg = foldl' (\acc w -> acc || watchHits w) False (reg ^. registerOptions . watchers)

      clientCapsSupports =
          caps ^? workspace . _Just . didChangeWatchedFiles . _Just . dynamicRegistration . _Just
            == Just True
      shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

  when shouldSend $
    sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
      List [ FileEvent (filePathToUri (rootDir </> file)) FcCreated ]
  openDoc' file languageId contents

-- | Opens a text document that /exists on disk/, and sends a
-- textDocument/didOpen notification to the server.
openDoc :: FilePath -> T.Text -> Session TextDocumentIdentifier
openDoc file languageId = do
  context <- ask
  let fp = rootDir context </> file
  contents <- liftIO $ T.readFile fp
  openDoc' file languageId contents

-- | This is a variant of `openDoc` that takes the file content as an argument.
-- Use this is the file exists /outside/ of the current workspace.
openDoc' :: FilePath -> T.Text -> T.Text -> Session TextDocumentIdentifier
openDoc' file languageId contents = do
  context <- ask
  let fp = rootDir context </> file
      uri = filePathToUri fp
      item = TextDocumentItem uri languageId 0 contents
  sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams item)
  pure $ TextDocumentIdentifier uri

-- | Closes a text document and sends a textDocument/didOpen notification to the server.
closeDoc :: TextDocumentIdentifier -> Session ()
closeDoc docId = do
  let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
  sendNotification STextDocumentDidClose params

-- | Changes a text document and sends a textDocument/didOpen notification to the server.
changeDoc :: TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
  verDoc <- getVersionedDoc docId
  let params = DidChangeTextDocumentParams (verDoc & version . non 0 +~ 1) (List changes)
  sendNotification STextDocumentDidChange params

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
  context <- ask
  let fp = rootDir context </> file
  return $ filePathToUri fp

-- | Waits for diagnostics to be published and returns them.
waitForDiagnostics :: Session [Diagnostic]
waitForDiagnostics = do
  diagsNot <- skipManyTill anyMessage (message STextDocumentPublishDiagnostics)
  let (List diags) = diagsNot ^. params . LSP.diagnostics
  return diags

-- | The same as 'waitForDiagnostics', but will only match a specific
-- 'Language.LSP.Types._source'.
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
  diagsNot <- message STextDocumentPublishDiagnostics
  when (diagsNot ^. params . LSP.diagnostics /= List []) $ liftIO $ throw UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: TextDocumentIdentifier -> Session (Either [DocumentSymbol] [SymbolInformation])
getDocumentSymbols doc = do
  ResponseMessage _ rspLid res <- request STextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
  case res of
    Right (InL (List xs)) -> return (Left xs)
    Right (InR (List xs)) -> return (Right xs)
    Left err -> throw (UnexpectedResponseError (SomeLspId $ fromJust rspLid) err)

-- | Returns the code actions in the specified range.
getCodeActions :: TextDocumentIdentifier -> Range -> Session [Command |? CodeAction]
getCodeActions doc range = do
  ctx <- getCodeActionContextInRange doc range
  rsp <- request STextDocumentCodeAction (CodeActionParams Nothing Nothing doc range ctx)

  case rsp ^. result of
    Right (List xs) -> return xs
    Left error -> throw (UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. LSP.id) error)

-- | Returns all the code actions in a document by
-- querying the code actions at each of the current
-- diagnostics' positions.
getAllCodeActions :: TextDocumentIdentifier -> Session [Command |? CodeAction]
getAllCodeActions doc = do
  ctx <- getCodeActionContext doc

  foldM (go ctx) [] =<< getCurrentDiagnostics doc

  where
    go :: CodeActionContext -> [Command |? CodeAction] -> Diagnostic -> Session [Command |? CodeAction]
    go ctx acc diag = do
      ResponseMessage _ rspLid res <- request STextDocumentCodeAction (CodeActionParams Nothing Nothing doc (diag ^. range) ctx)

      case res of
        Left e -> throw (UnexpectedResponseError (SomeLspId $ fromJust rspLid) e)
        Right (List cmdOrCAs) -> pure (acc ++ cmdOrCAs)

getCodeActionContextInRange :: TextDocumentIdentifier -> Range -> Session CodeActionContext
getCodeActionContextInRange doc caRange = do
  curDiags <- getCurrentDiagnostics doc
  let diags = [ d | d@Diagnostic{_range=range} <- curDiags
                  , overlappingRange caRange range
              ]
  return $ CodeActionContext (List diags) Nothing
  where
    overlappingRange :: Range -> Range -> Bool
    overlappingRange (Range s e) range =
         positionInRange s range
      || positionInRange e range

    positionInRange :: Position -> Range -> Bool
    positionInRange (Position pl po) (Range (Position sl so) (Position el eo)) =
         pl >  sl && pl <  el
      || pl == sl && pl == el && po >= so && po <= eo
      || pl == sl && po >= so
      || pl == el && po <= eo

getCodeActionContext :: TextDocumentIdentifier -> Session CodeActionContext
getCodeActionContext doc = do
  curDiags <- getCurrentDiagnostics doc
  return $ CodeActionContext (List curDiags) Nothing

-- | Returns the current diagnostics that have been sent to the client.
-- Note that this does not wait for more to come in.
getCurrentDiagnostics :: TextDocumentIdentifier -> Session [Diagnostic]
getCurrentDiagnostics doc = fromMaybe [] . Map.lookup (toNormalizedUri $ doc ^. uri) . curDiagnostics <$> get

-- | Returns the tokens of all progress sessions that have started but not yet ended.
getIncompleteProgressSessions :: Session (Set.Set ProgressToken)
getIncompleteProgressSessions = curProgressSessions <$> get

-- | Executes a command.
executeCommand :: Command -> Session ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams Nothing (cmd ^. command) args
  void $ sendRequest SWorkspaceExecuteCommand execParams

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
          let req = RequestMessage "" (IdInt 0) SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing e)
            in updateState (FromServerMess SWorkspaceApplyEdit req)

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
        let mWorkspace = caps ^. LSP.workspace
        C.WorkspaceClientCapabilities _ mEdit _ _ _ _ _ _ <- mWorkspace
        C.WorkspaceEditClientCapabilities mDocChanges _ _ _ _ <- mEdit
        mDocChanges

  let wEdit = if supportsDocChanges
      then
        let docEdit = TextDocumentEdit verDoc (List [InL edit])
        in WorkspaceEdit Nothing (Just (List [InL docEdit])) Nothing
      else
        let changes = HashMap.singleton (doc ^. uri) (List [edit])
        in WorkspaceEdit (Just changes) Nothing Nothing

  let req = RequestMessage "" (IdInt 0) SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit)
  updateState (FromServerMess SWorkspaceApplyEdit req)

  -- version may have changed
  getVersionedDoc doc

-- | Returns the completions for the position in the document.
getCompletions :: TextDocumentIdentifier -> Position -> Session [CompletionItem]
getCompletions doc pos = do
  rsp <- request STextDocumentCompletion (CompletionParams doc pos Nothing Nothing Nothing)

  case getResponseResult rsp of
    InL (List items) -> return items
    InR (CompletionList _ (List items)) -> return items

-- | Returns the references for the position in the document.
getReferences :: TextDocumentIdentifier -- ^ The document to lookup in.
              -> Position -- ^ The position to lookup.
              -> Bool -- ^ Whether to include declarations as references.
              -> Session (List Location) -- ^ The locations of the references.
getReferences doc pos inclDecl =
  let ctx = ReferenceContext inclDecl
      params = ReferenceParams doc pos Nothing Nothing ctx
  in getResponseResult <$> request STextDocumentReferences params

-- | Returns the declarations(s) for the term at the specified position.
getDeclarations :: TextDocumentIdentifier -- ^ The document the term is in.
                -> Position -- ^ The position the term is at.
                -> Session ([Location] |? [LocationLink])
getDeclarations = getDeclarationyRequest STextDocumentDeclaration DeclarationParams

-- | Returns the definition(s) for the term at the specified position.
getDefinitions :: TextDocumentIdentifier -- ^ The document the term is in.
               -> Position -- ^ The position the term is at.
               -> Session ([Location] |? [LocationLink])
getDefinitions = getDeclarationyRequest STextDocumentDefinition DefinitionParams

-- | Returns the type definition(s) for the term at the specified position.
getTypeDefinitions :: TextDocumentIdentifier -- ^ The document the term is in.
                   -> Position -- ^ The position the term is at.
                   -> Session ([Location] |? [LocationLink])
getTypeDefinitions = getDeclarationyRequest STextDocumentTypeDefinition TypeDefinitionParams 

-- | Returns the type definition(s) for the term at the specified position.
getImplementations :: TextDocumentIdentifier -- ^ The document the term is in.
                   -> Position -- ^ The position the term is at.
                   -> Session ([Location] |? [LocationLink])
getImplementations = getDeclarationyRequest STextDocumentImplementation ImplementationParams


getDeclarationyRequest :: (ResponseResult m ~ (Location |? (List Location |? List LocationLink)))
                       => SClientMethod m
                       -> (TextDocumentIdentifier
                            -> Position
                            -> Maybe ProgressToken
                            -> Maybe ProgressToken
                            -> MessageParams m)
                       -> TextDocumentIdentifier
                       -> Position
                       -> Session ([Location] |? [LocationLink])
getDeclarationyRequest method paramCons doc pos = do
  let params = paramCons doc pos Nothing Nothing
  rsp <- request method params
  case getResponseResult rsp of
      InL loc -> pure (InL [loc])
      InR (InL (List locs)) -> pure (InL locs)
      InR (InR (List locLinks)) -> pure (InR locLinks)

-- | Renames the term at the specified position.
rename :: TextDocumentIdentifier -> Position -> String -> Session ()
rename doc pos newName = do
  let params = RenameParams doc pos Nothing (T.pack newName)
  rsp <- request STextDocumentRename params
  let wEdit = getResponseResult rsp
      req = RequestMessage "" (IdInt 0) SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit)
  updateState (FromServerMess SWorkspaceApplyEdit req)

-- | Returns the hover information at the specified position.
getHover :: TextDocumentIdentifier -> Position -> Session (Maybe Hover)
getHover doc pos =
  let params = HoverParams doc pos Nothing
  in getResponseResult <$> request STextDocumentHover params

-- | Returns the highlighted occurences of the term at the specified position
getHighlights :: TextDocumentIdentifier -> Position -> Session (List DocumentHighlight)
getHighlights doc pos =
  let params = DocumentHighlightParams doc pos Nothing Nothing
  in getResponseResult <$> request STextDocumentDocumentHighlight params

-- | Checks the response for errors and throws an exception if needed.
-- Returns the result if successful.
getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult rsp =
  case rsp ^. result of
    Right x -> x
    Left err -> throw $ UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. LSP.id) err

-- | Applies formatting to the specified document.
formatDoc :: TextDocumentIdentifier -> FormattingOptions -> Session ()
formatDoc doc opts = do
  let params = DocumentFormattingParams Nothing doc opts
  edits <- getResponseResult <$> request STextDocumentFormatting params
  applyTextEdits doc edits

-- | Applies formatting to the specified range in a document.
formatRange :: TextDocumentIdentifier -> FormattingOptions -> Range -> Session ()
formatRange doc opts range = do
  let params = DocumentRangeFormattingParams Nothing doc range opts
  edits <- getResponseResult <$> request STextDocumentRangeFormatting params
  applyTextEdits doc edits

applyTextEdits :: TextDocumentIdentifier -> List TextEdit -> Session ()
applyTextEdits doc edits =
  let wEdit = WorkspaceEdit (Just (HashMap.singleton (doc ^. uri) edits)) Nothing Nothing
      -- Send a dummy message to updateState so it can do bookkeeping
      req = RequestMessage "" (IdInt 0) SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit)
  in updateState (FromServerMess SWorkspaceApplyEdit req)

-- | Returns the code lenses for the specified document.
getCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getCodeLenses tId = do
    rsp <- request STextDocumentCodeLens (CodeLensParams Nothing Nothing tId)
    case getResponseResult rsp of
        List res -> pure res

-- | Returns a list of capabilities that the server has requested to /dynamically/
-- register during the 'Session'.
--
-- @since 0.11.0.0
getRegisteredCapabilities :: Session [SomeRegistration]
getRegisteredCapabilities = Map.elems . curDynCaps <$> get
