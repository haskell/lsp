{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
-- there's just so much!
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.LSP.Server.Processing where

import Colog.Core (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  cmap,
  (<&),
 )

import Control.Concurrent.Extra as C
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Except ()
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Writer.Strict
import Data.Aeson hiding (
  Error,
  Null,
  Options,
 )
import Data.Aeson.Lens ()
import Data.Aeson.Types hiding (
  Error,
  Null,
  Options,
 )
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Functor.Product qualified as P
import Data.IxMap
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.SMethodMap (SMethodMap)
import Language.LSP.Protocol.Utils.SMethodMap qualified as SMethodMap
import Language.LSP.Server.Core
import Language.LSP.VFS as VFS
import Prettyprinter
import System.Exit

data LspProcessingLog
  = VfsLog VfsLog
  | LspCore LspCoreLog
  | MessageProcessingError BSL.ByteString String
  | forall m. MissingHandler Bool (SClientMethod m)
  | ProgressCancel ProgressToken
  | forall m. MessageDuringShutdown (SClientMethod m)
  | ShuttingDown
  | Exiting

deriving instance Show LspProcessingLog

instance Pretty LspProcessingLog where
  pretty (VfsLog l) = pretty l
  pretty (LspCore l) = pretty l
  pretty (MessageProcessingError bs err) =
    vsep
      [ "LSP: incoming message parse error:"
      , pretty err
      , "when processing"
      , pretty (TL.decodeUtf8 bs)
      ]
  pretty (MissingHandler _ m) = "LSP: no handler for:" <+> pretty m
  pretty (ProgressCancel tid) = "LSP: cancelling action for token:" <+> pretty tid
  pretty (MessageDuringShutdown m) = "LSP: received message during shutdown:" <+> pretty m
  pretty ShuttingDown = "LSP: received shutdown"
  pretty Exiting = "LSP: received exit"

processMessage :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> BSL.ByteString -> m ()
processMessage logger jsonStr = do
  pendingResponsesVar <- LspT $ asks $ resPendingResponses . resState
  shutdown <- isShuttingDown
  join $ liftIO $ atomically $ fmap handleErrors $ runExceptT $ do
    val <- except $ eitherDecode jsonStr
    pending <- lift $ readTVar pendingResponsesVar
    msg <- except $ parseEither (parser pending) val
    lift $ case msg of
      FromClientMess m mess ->
        pure $ handle logger m mess
      FromClientRsp (P.Pair (ServerResponseCallback f) (Const !newMap)) res -> do
        -- see Note [Shutdown]
        writeTVar pendingResponsesVar newMap
        unless shutdown <$> do
          pure $ liftIO $ f (res ^. L.result)
 where
  parser :: ResponseMap -> Value -> Parser (FromClientMessage' (P.Product ServerResponseCallback (Const ResponseMap)))
  parser rm = parseClientMessage $ \i ->
    let (mhandler, newMap) = pickFromIxMap i rm
     in (\(P.Pair m handler) -> (m, P.Pair handler (Const newMap))) <$> mhandler

  handleErrors = either (\e -> logger <& MessageProcessingError jsonStr e `WithSeverity` Error) id

-- | Call this to initialize the session
initializeRequestHandler ::
  LogAction IO (WithSeverity LspProcessingLog) ->
  ServerDefinition config ->
  VFS ->
  (FromServerMessage -> IO ()) ->
  TMessage Method_Initialize ->
  IO (Maybe (LanguageContextEnv config))
initializeRequestHandler logger ServerDefinition{..} vfs sendFunc req = do
  let sendResp = sendFunc . FromServerRsp SMethod_Initialize
      handleErr (Left err) = do
        sendResp $ makeResponseError (req ^. L.id) err
        pure Nothing
      handleErr (Right a) = pure $ Just a
  E.handle (initializeErrorHandler $ sendResp . makeResponseError (req ^. L.id)) $ handleErr <=< runExceptT $ mdo
    let p = req ^. L.params
        rootDir =
          getFirst $
            foldMap
              First
              [ p ^? L.rootUri . _L >>= uriToFilePath
              , p ^? L.rootPath . _Just . _L <&> T.unpack
              ]
        clientCaps = p ^. L.capabilities

    let initialWfs = case p ^. L.workspaceFolders of
          Just (InL xs) -> xs
          _ -> []

        -- See Note [LSP configuration]
        configObject = lookForConfigSection configSection <$> (p ^. L.initializationOptions)

    initialConfig <- case configObject of
      Just o -> case parseConfig defaultConfig o of
        Right newConfig -> do
          liftIO $ logger <& LspCore (NewConfig o) `WithSeverity` Debug
          pure newConfig
        Left err -> do
          -- Warn not error here, since initializationOptions is pretty unspecified
          liftIO $ logger <& LspCore (ConfigurationParseError o err) `WithSeverity` Warning
          pure defaultConfig
      Nothing -> pure defaultConfig

    stateVars <- liftIO $ do
      resVFS <- newTVarIO (VFSData vfs mempty)
      resDiagnostics <- newTVarIO mempty
      resConfig <- newTVarIO initialConfig
      resWorkspaceFolders <- newTVarIO initialWfs
      resProgressData <- do
        progressNextId <- newTVarIO 0
        progressCancel <- newTVarIO mempty
        pure ProgressData{..}
      resPendingResponses <- newTVarIO emptyIxMap
      resRegistrationsNot <- newTVarIO mempty
      resRegistrationsReq <- newTVarIO mempty
      resLspId <- newTVarIO 0
      resShutdown <- C.newBarrier
      pure LanguageContextState{..}

    -- Call the 'duringInitialization' callback to let the server kick stuff up
    let env =
          LanguageContextEnv
            handlers
            configSection
            parseConfig
            configChanger
            sendFunc
            stateVars
            (p ^. L.capabilities)
            rootDir
            (optProgressStartDelay options)
            (optProgressUpdateDelay options)
        configChanger config = forward interpreter (onConfigChange config)
        handlers = transmuteHandlers interpreter (staticHandlers clientCaps)
        interpreter = interpretHandler initializationResult
    initializationResult <- ExceptT $ doInitialize env req

    let serverCaps = inferServerCapabilities clientCaps options handlers
    liftIO $ sendResp $ makeResponseMessage (req ^. L.id) (InitializeResult serverCaps (optServerInfo options))
    pure env
 where
  makeResponseMessage rid result = TResponseMessage "2.0" (Just rid) (Right result)
  makeResponseError origId err = TResponseMessage "2.0" (Just origId) (Left err)

  initializeErrorHandler :: (TResponseError Method_Initialize -> IO ()) -> E.SomeException -> IO (Maybe a)
  initializeErrorHandler sendResp e = do
    sendResp $ TResponseError (InR ErrorCodes_InternalError) msg Nothing
    pure Nothing
   where
    msg = T.pack $ unwords ["Error on initialize:", show e]

{- | Infers the capabilities based on registered handlers, and sets the appropriate options.
 A provider should be set to Nothing if the server does not support it, unless it is a
 static option.
-}
inferServerCapabilities :: ClientCapabilities -> Options -> Handlers m -> ServerCapabilities
inferServerCapabilities _clientCaps o h =
  ServerCapabilities
    { _textDocumentSync = sync
    , _hoverProvider =
        supported' SMethod_TextDocumentHover $
          InR $
            HoverOptions clientInitiatedProgress
    , _completionProvider = completionProvider
    , _inlayHintProvider = inlayProvider
    , _declarationProvider =
        supported' SMethod_TextDocumentDeclaration $
          InR $
            InL $
              DeclarationOptions clientInitiatedProgress
    , _signatureHelpProvider = signatureHelpProvider
    , _definitionProvider =
        supported' SMethod_TextDocumentDefinition $
          InR $
            DefinitionOptions clientInitiatedProgress
    , _typeDefinitionProvider =
        supported' SMethod_TextDocumentTypeDefinition $
          InR $
            InL $
              TypeDefinitionOptions clientInitiatedProgress
    , _implementationProvider =
        supported' SMethod_TextDocumentImplementation $
          InR $
            InL $
              ImplementationOptions clientInitiatedProgress
    , _referencesProvider =
        supported' SMethod_TextDocumentReferences $
          InR $
            ReferenceOptions clientInitiatedProgress
    , _documentHighlightProvider =
        supported' SMethod_TextDocumentDocumentHighlight $
          InR $
            DocumentHighlightOptions clientInitiatedProgress
    , _documentSymbolProvider =
        supported' SMethod_TextDocumentDocumentSymbol $
          InR $
            DocumentSymbolOptions clientInitiatedProgress Nothing
    , _codeActionProvider = codeActionProvider
    , _codeLensProvider =
        supported' SMethod_TextDocumentCodeLens $
          CodeLensOptions clientInitiatedProgress (supported SMethod_CodeLensResolve)
    , _documentFormattingProvider =
        supported' SMethod_TextDocumentFormatting $
          InR $
            DocumentFormattingOptions clientInitiatedProgress
    , _documentRangeFormattingProvider =
        supported' SMethod_TextDocumentRangeFormatting $
          InR $
            DocumentRangeFormattingOptions clientInitiatedProgress
    , _documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , _renameProvider =
        supported' SMethod_TextDocumentRename $
          InR $
            RenameOptions clientInitiatedProgress (supported SMethod_TextDocumentPrepareRename)
    , _documentLinkProvider =
        supported' SMethod_TextDocumentDocumentLink $
          DocumentLinkOptions clientInitiatedProgress (supported SMethod_DocumentLinkResolve)
    , _colorProvider =
        supported' SMethod_TextDocumentDocumentColor $
          InR $
            InL $
              DocumentColorOptions clientInitiatedProgress
    , _foldingRangeProvider =
        supported' SMethod_TextDocumentFoldingRange $
          InR $
            InL $
              FoldingRangeOptions clientInitiatedProgress
    , _executeCommandProvider = executeCommandProvider
    , _selectionRangeProvider =
        supported' SMethod_TextDocumentSelectionRange $
          InR $
            InL $
              SelectionRangeOptions clientInitiatedProgress
    , _callHierarchyProvider =
        supported' SMethod_TextDocumentPrepareCallHierarchy $
          InR $
            InL $
              CallHierarchyOptions clientInitiatedProgress
    , _semanticTokensProvider = semanticTokensProvider
    , _workspaceSymbolProvider =
        supported' SMethod_WorkspaceSymbol $
          InR $
            WorkspaceSymbolOptions clientInitiatedProgress (supported SMethod_WorkspaceSymbolResolve)
    , _workspace = Just workspace
    , _experimental = Nothing :: Maybe Value
    , -- The only encoding the VFS supports is the legacy UTF16 option at the moment
      _positionEncoding = Just PositionEncodingKind_UTF16
    , _linkedEditingRangeProvider =
        supported' SMethod_TextDocumentLinkedEditingRange $
          InR $
            InL $
              LinkedEditingRangeOptions clientInitiatedProgress
    , _monikerProvider =
        supported' SMethod_TextDocumentMoniker $
          InR $
            InL $
              MonikerOptions clientInitiatedProgress
    , _typeHierarchyProvider =
        supported' SMethod_TextDocumentPrepareTypeHierarchy $
          InR $
            InL $
              TypeHierarchyOptions clientInitiatedProgress
    , _inlineValueProvider =
        supported' SMethod_TextDocumentInlineValue $
          InR $
            InL $
              InlineValueOptions clientInitiatedProgress
    , _diagnosticProvider = diagnosticProvider
    , -- TODO: super unclear what to do about notebooks in general
      _notebookDocumentSync = Nothing
    }
 where
  clientInitiatedProgress = Just (optSupportClientInitiatedProgress o)

  supported' m b
    | supported_b m = Just b
    | otherwise = Nothing

  supported :: forall m. SClientMethod m -> Maybe Bool
  supported = Just . supported_b

  supported_b :: forall m. SClientMethod m -> Bool
  supported_b m = case splitClientMethod m of
    IsClientNot -> SMethodMap.member m $ notHandlers h
    IsClientReq -> SMethodMap.member m $ reqHandlers h
    IsClientEither -> error "capabilities depend on custom method"

  singleton :: a -> [a]
  singleton x = [x]

  completionProvider =
    supported' SMethod_TextDocumentCompletion $
      CompletionOptions
        { _triggerCharacters = map T.singleton <$> optCompletionTriggerCharacters o
        , _allCommitCharacters = map T.singleton <$> optCompletionAllCommitCharacters o
        , _resolveProvider = supported SMethod_CompletionItemResolve
        , _completionItem = Nothing
        , _workDoneProgress = clientInitiatedProgress
        }

  inlayProvider =
    supported' SMethod_TextDocumentInlayHint $
      InR $
        InL
          InlayHintOptions
            { _workDoneProgress = clientInitiatedProgress
            , _resolveProvider = supported SMethod_InlayHintResolve
            }

  codeActionProvider =
    supported' SMethod_TextDocumentCodeAction $
      InR $
        CodeActionOptions
          { _workDoneProgress = clientInitiatedProgress
          , _codeActionKinds = optCodeActionKinds o
          , _resolveProvider = supported SMethod_CodeActionResolve
          }

  signatureHelpProvider =
    supported' SMethod_TextDocumentSignatureHelp $
      SignatureHelpOptions
        clientInitiatedProgress
        (map T.singleton <$> optSignatureHelpTriggerCharacters o)
        (map T.singleton <$> optSignatureHelpRetriggerCharacters o)

  documentOnTypeFormattingProvider
    | supported_b SMethod_TextDocumentOnTypeFormatting
    , Just (first :| rest) <- optDocumentOnTypeFormattingTriggerCharacters o =
        Just $
          DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singleton) rest))
    | supported_b SMethod_TextDocumentOnTypeFormatting
    , Nothing <- optDocumentOnTypeFormattingTriggerCharacters o =
        error "documentOnTypeFormattingTriggerCharacters needs to be set if a documentOnTypeFormattingHandler is set"
    | otherwise = Nothing

  executeCommandProvider =
    supported' SMethod_WorkspaceExecuteCommand $
      case optExecuteCommandCommands o of
        Just cmds -> ExecuteCommandOptions clientInitiatedProgress cmds
        Nothing -> error "executeCommandCommands needs to be set if a executeCommandHandler is set"

  -- Always provide the default legend
  -- TODO: allow user-provided legend via 'Options', or at least user-provided types
  semanticTokensProvider =
    Just $
      InL $
        SemanticTokensOptions clientInitiatedProgress defaultSemanticTokensLegend semanticTokenRangeProvider semanticTokenFullProvider
  semanticTokenRangeProvider
    | supported_b SMethod_TextDocumentSemanticTokensRange = Just $ InL True
    | otherwise = Nothing
  semanticTokenFullProvider
    | supported_b SMethod_TextDocumentSemanticTokensFull = Just $ InR $ SemanticTokensFullDelta{_delta = supported SMethod_TextDocumentSemanticTokensFullDelta}
    | otherwise = Nothing

  sync = case optTextDocumentSync o of
    Just x -> Just (InL x)
    Nothing -> Nothing

  workspace = WorkspaceOptions{_workspaceFolders = workspaceFolder, _fileOperations = Nothing}
  workspaceFolder =
    supported' SMethod_WorkspaceDidChangeWorkspaceFolders $
      -- sign up to receive notifications
      WorkspaceFoldersServerCapabilities (Just True) (Just (InR True))

  diagnosticProvider =
    supported' SMethod_TextDocumentDiagnostic $
      InL $
        DiagnosticOptions
          { _workDoneProgress = clientInitiatedProgress
          , _identifier = Nothing
          , -- TODO: this is a conservative but maybe inaccurate, unclear how much it matters
            _interFileDependencies = True
          , _workspaceDiagnostics = supported_b SMethod_WorkspaceDiagnostic
          }

{- | Invokes the registered dynamic or static handlers for the given message and
 method, as well as doing some bookkeeping.
-}
handle :: forall m config meth. (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> SClientMethod meth -> TClientMessage meth -> m ()
handle logger m msg =
  case m of
    SMethod_WorkspaceDidChangeWorkspaceFolders -> handle' logger (Just updateWorkspaceFolders) m msg
    SMethod_WorkspaceDidChangeConfiguration -> handle' logger (Just $ handleDidChangeConfiguration logger) m msg
    -- See Note [LSP configuration]
    SMethod_Initialized -> handle' logger (Just $ \_ -> initialDynamicRegistrations logger >> requestConfigUpdate (cmap (fmap LspCore) logger)) m msg
    SMethod_Shutdown -> handle' logger (Just $ \_ -> signalShutdown) m msg
     where
      -- See Note [Shutdown]
      signalShutdown :: LspM config ()
      signalShutdown = do
        logger <& ShuttingDown `WithSeverity` Info
        b <- resShutdown . resState <$> getLspEnv
        liftIO $ signalBarrier b ()
    SMethod_TextDocumentDidOpen -> handle' logger (Just $ vfsFunc logger openVFS) m msg
    SMethod_TextDocumentDidChange -> handle' logger (Just $ vfsFunc logger changeFromClientVFS) m msg
    SMethod_TextDocumentDidClose -> handle' logger (Just $ vfsFunc logger closeVFS) m msg
    SMethod_WindowWorkDoneProgressCancel -> handle' logger (Just $ progressCancelHandler logger) m msg
    _ -> handle' logger Nothing m msg

handle' ::
  forall m t (meth :: Method ClientToServer t) config.
  (m ~ LspM config) =>
  LogAction m (WithSeverity LspProcessingLog) ->
  -- | An action to be run before invoking the handler, used for
  -- bookkeeping stuff like the vfs etc.
  Maybe (TClientMessage meth -> m ()) ->
  SClientMethod meth ->
  TClientMessage meth ->
  m ()
handle' logger mAction m msg = do
  shutdown <- isShuttingDown
  -- These are the methods that we are allowed to process during shutdown.
  -- The reason that we do not include 'shutdown' itself here is because
  -- by the time we get the first 'shutdown' message, isShuttingDown will
  -- still be false, so we would still be able to process it.
  -- This ensures we won't process the second 'shutdown' message and only
  -- process 'exit' during shutdown.
  let allowedMethod m = case (splitClientMethod m, m) of
        (IsClientNot, SMethod_Exit) -> True
        _ -> False

  case mAction of
    Just f | not shutdown || allowedMethod m -> f msg
    _ -> pure ()

  dynReqHandlers <- getsState resRegistrationsReq
  dynNotHandlers <- getsState resRegistrationsNot

  env <- getLspEnv
  let Handlers{reqHandlers, notHandlers} = resHandlers env

  case splitClientMethod m of
    -- See Note [Shutdown]
    IsClientNot | shutdown, not (allowedMethod m) -> notificationDuringShutdown
    IsClientNot -> case pickHandler dynNotHandlers notHandlers of
      Just h -> liftIO $ h msg
      Nothing
        | SMethod_Exit <- m -> exitNotificationHandler logger msg
        | otherwise -> missingNotificationHandler
    -- See Note [Shutdown]
    IsClientReq | shutdown, not (allowedMethod m) -> requestDuringShutdown msg
    IsClientReq -> case pickHandler dynReqHandlers reqHandlers of
      Just h -> liftIO $ h msg (runLspT env . sendResponse msg)
      Nothing
        | SMethod_Shutdown <- m -> liftIO $ shutdownRequestHandler msg (runLspT env . sendResponse msg)
        | otherwise -> missingRequestHandler msg
    IsClientEither -> case msg of
      -- See Note [Shutdown]
      NotMess _ | shutdown -> notificationDuringShutdown
      NotMess noti -> case pickHandler dynNotHandlers notHandlers of
        Just h -> liftIO $ h noti
        Nothing -> missingNotificationHandler
      -- See Note [Shutdown]
      ReqMess req | shutdown -> requestDuringShutdown req
      ReqMess req -> case pickHandler dynReqHandlers reqHandlers of
        Just h -> liftIO $ h req (runLspT env . sendResponse req)
        Nothing -> missingRequestHandler req
 where
  -- \| Checks to see if there's a dynamic handler, and uses it in favour of the
  -- static handler, if it exists.
  pickHandler :: RegistrationMap t -> SMethodMap (ClientMessageHandler IO t) -> Maybe (Handler IO meth)
  pickHandler dynHandlerMap staticHandler = case (SMethodMap.lookup m dynHandlerMap, SMethodMap.lookup m staticHandler) of
    (Just (P.Pair _ (ClientMessageHandler h)), _) -> Just h
    (Nothing, Just (ClientMessageHandler h)) -> Just h
    (Nothing, Nothing) -> Nothing

  sendResponse :: forall m1. TRequestMessage (m1 :: Method ClientToServer Request) -> Either (TResponseError m1) (MessageResult m1) -> m ()
  sendResponse req res = sendToClient $ FromServerRsp (req ^. L.method) $ TResponseMessage "2.0" (Just (req ^. L.id)) res

  requestDuringShutdown :: forall m1. TRequestMessage (m1 :: Method ClientToServer Request) -> m ()
  requestDuringShutdown req = do
    logger <& MessageDuringShutdown m `WithSeverity` Warning
    sendResponse req (Left (TResponseError (InR ErrorCodes_InvalidRequest) "Server is shutdown" Nothing))

  notificationDuringShutdown :: m ()
  notificationDuringShutdown = logger <& MessageDuringShutdown m `WithSeverity` Warning

  -- '$/' notifications should/could be ignored by server.
  -- Don't log errors in that case.
  -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
  missingNotificationHandler :: m ()
  missingNotificationHandler =
    let optional = isOptionalMethod (SomeMethod m)
     in logger <& MissingHandler optional m `WithSeverity` if optional then Warning else Error

  missingRequestHandler :: TRequestMessage (m1 :: Method ClientToServer Request) -> m ()
  missingRequestHandler req = do
    logger <& MissingHandler False m `WithSeverity` Error
    let errorMsg = T.pack $ unwords ["No handler for: ", show m]
        err = TResponseError (InR ErrorCodes_MethodNotFound) errorMsg Nothing
    sendResponse req (Left err)

progressCancelHandler :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> TMessage Method_WindowWorkDoneProgressCancel -> m ()
progressCancelHandler logger (TNotificationMessage _ _ (WorkDoneProgressCancelParams tid)) = do
  pdata <- getsState (progressCancel . resProgressData)
  case Map.lookup tid pdata of
    Nothing -> return ()
    Just cancelAction -> do
      logger <& ProgressCancel tid `WithSeverity` Debug
      liftIO cancelAction

exitNotificationHandler :: (MonadIO m) => LogAction m (WithSeverity LspProcessingLog) -> Handler m Method_Exit
exitNotificationHandler logger _ = do
  logger <& Exiting `WithSeverity` Info
  liftIO exitSuccess

-- | Default Shutdown handler
shutdownRequestHandler :: Handler IO Method_Shutdown
shutdownRequestHandler _req k = do
  k $ Right Null

initialDynamicRegistrations :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> m ()
initialDynamicRegistrations logger = do
  section <- LspT $ asks resConfigSection
  -- We need to register for `workspace/didChangeConfiguration` dynamically in order to
  -- ensure we receive notifications. See
  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_configuration
  -- https://github.com/microsoft/language-server-protocol/issues/1888
  void $
    trySendRegistration
      (cmap (fmap LspCore) logger)
      SMethod_WorkspaceDidChangeConfiguration
      (DidChangeConfigurationRegistrationOptions (Just $ InL section))

{- | Try to find the configuration section in an object that might represent "all" the settings.
 The heuristic we use is to look for a property with the right name, and use that if we find
 it. Otherwise we fall back to the whole object.
 See Note [LSP configuration]
-}
lookForConfigSection :: T.Text -> Value -> Value
lookForConfigSection section (Object o) | Just s' <- o ^. at (fromString $ T.unpack section) = s'
lookForConfigSection _ o = o

-- | Handle a workspace/didChangeConfiguration request.
handleDidChangeConfiguration :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> TMessage Method_WorkspaceDidChangeConfiguration -> m ()
handleDidChangeConfiguration logger req = do
  section <- LspT $ asks resConfigSection
  -- See Note [LSP configuration]

  -- There are a few cases:
  -- 1. Client supports `workspace/configuration` and sends nothing in `workspace/didChangeConfiguration`
  --    Then we will fail the first attempt and succeed the second one.
  -- 2. Client does not support `workspace/configuration` and sends updated config in `workspace/didChangeConfiguration`.
  --    Then we will succeed the first attempt and fail (or in fact do nothing in) the second one.
  -- 3. Client supports `workspace/configuration` and sends updated config in `workspace/didChangeConfiguration`.
  --    Then both will succeed, which is a bit redundant but not a big deal.
  tryChangeConfig (cmap (fmap LspCore) logger) (lookForConfigSection section $ req ^. L.params . L.settings)
  requestConfigUpdate (cmap (fmap LspCore) logger)

vfsFunc ::
  forall m n a config.
  (m ~ LspM config, n ~ WriterT [WithSeverity VfsLog] (State VFS)) =>
  LogAction m (WithSeverity LspProcessingLog) ->
  (LogAction n (WithSeverity VfsLog) -> a -> n ()) ->
  a ->
  m ()
vfsFunc logger modifyVfs req = do
  -- This is an intricate dance. We want to run the VFS functions essentially in STM, that's
  -- what 'stateState' does. But we also want them to log. We accomplish this by exfiltrating
  -- the logs through the return value of 'stateState' and then re-logging them.
  -- We therefore have to use the stupid approach of accumulating the logs in Writer inside
  -- the VFS functions. They don't log much so for now we just use [Log], but we could use
  -- DList here if we're worried about performance.
  logs <- stateState resVFS $ \(VFSData vfs rm) ->
    let (ls, vfs') = flip runState vfs $ execWriterT $ modifyVfs innerLogger req
     in (ls, VFSData vfs' rm)
  traverse_ (\l -> logger <& fmap VfsLog l) logs
 where
  innerLogger :: LogAction n (WithSeverity VfsLog)
  innerLogger = LogAction $ \m -> tell [m]

-- | Updates the list of workspace folders
updateWorkspaceFolders :: TMessage Method_WorkspaceDidChangeWorkspaceFolders -> LspM config ()
updateWorkspaceFolders (TNotificationMessage _ _ params) = do
  let toRemove = params ^. L.event . L.removed
      toAdd = params ^. L.event . L.added
      newWfs oldWfs = foldr delete oldWfs toRemove <> toAdd
  modifyState resWorkspaceFolders newWfs

-- ---------------------------------------------------------------------
