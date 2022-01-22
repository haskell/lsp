{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LSP.Server.Processing where

import           Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))

import Control.Lens hiding (List, Empty)
import Data.Aeson hiding (Options, Error)
import Data.Aeson.Types hiding (Options, Error)
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens as LSP
import           Language.LSP.Types.SMethodMap (SMethodMap)
import qualified Language.LSP.Types.SMethodMap as SMethodMap
import Language.LSP.Server.Core

import Language.LSP.VFS as VFS
import Data.Functor.Product
import qualified Control.Exception as E
import Data.Monoid hiding (Product)
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.IxMap
import Data.Maybe
import qualified Data.Map.Strict as Map
import System.Exit
import Data.Default (def)
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Product)
import Data.Foldable (traverse_)

data LspProcessingLog =
  VfsLog VfsLog
  | MessageProcessingError BSL.ByteString String
  | forall m . MissingHandler Bool (SClientMethod m)
  | ConfigurationParseError Value T.Text
  | ProgressCancel ProgressToken
  | Exiting

instance Show LspProcessingLog where
  show (VfsLog l) = show l
  show (MessageProcessingError bs l) = "LSP: incoming message parse error: " ++ l ++ " from " ++ TL.unpack (TL.decodeUtf8 bs)
  show (MissingHandler _ m) = "LSP: no handler for: " ++ show m
  show (ConfigurationParseError settings err) = "LSP: configuration parse error: " ++ show err ++ ", when parsing " ++ show settings
  show (ProgressCancel tid) = "LSP: cancelling action for token: " ++ show tid
  show Exiting = "LSP: Got exit, exiting"

processMessage :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> BSL.ByteString -> m ()
processMessage logger jsonStr = do
  pendingResponsesVar <- LspT $ asks $ resPendingResponses . resState
  join $ liftIO $ atomically $ fmap handleErrors $ runExceptT $ do
      val <- except $ eitherDecode jsonStr
      pending <- lift $ readTVar pendingResponsesVar
      msg <- except $ parseEither (parser pending) val
      lift $ case msg of
        FromClientMess m mess ->
          pure $ handle logger m mess
        FromClientRsp (Pair (ServerResponseCallback f) (Const !newMap)) res -> do
          writeTVar pendingResponsesVar newMap
          pure $ liftIO $ f (res ^. LSP.result)
  where
    parser :: ResponseMap -> Value -> Parser (FromClientMessage' (Product ServerResponseCallback (Const ResponseMap)))
    parser rm = parseClientMessage $ \i ->
      let (mhandler, newMap) = pickFromIxMap i rm
        in (\(Pair m handler) -> (m,Pair handler (Const newMap))) <$> mhandler

    handleErrors = either (\e -> logger <& MessageProcessingError jsonStr e `WithSeverity` Error) id

-- | Call this to initialize the session
initializeRequestHandler
  :: ServerDefinition config
  -> VFS
  -> (FromServerMessage -> IO ())
  -> Message Initialize
  -> IO (Maybe (LanguageContextEnv config))
initializeRequestHandler ServerDefinition{..} vfs sendFunc req = do
  let sendResp = sendFunc . FromServerRsp SInitialize
      handleErr (Left err) = do
        sendResp $ makeResponseError (req ^. LSP.id) err
        pure Nothing
      handleErr (Right a) = pure $ Just a
  flip E.catch (initializeErrorHandler $ sendResp . makeResponseError (req ^. LSP.id)) $ handleErr <=< runExceptT $ mdo

    let params = req ^. LSP.params
        rootDir = getFirst $ foldMap First [ params ^. LSP.rootUri  >>= uriToFilePath
                                           , params ^. LSP.rootPath <&> T.unpack ]

    let initialWfs = case params ^. LSP.workspaceFolders of
          Just (List xs) -> xs
          Nothing -> []

        initialConfig = case onConfigurationChange defaultConfig <$> (req ^. LSP.params . LSP.initializationOptions) of
          Just (Right newConfig) -> newConfig
          _ -> defaultConfig

    stateVars <- liftIO $ do
      resVFS              <- newTVarIO (VFSData vfs mempty)
      resDiagnostics      <- newTVarIO mempty
      resConfig           <- newTVarIO initialConfig
      resWorkspaceFolders <- newTVarIO initialWfs
      resProgressData     <- do
        progressNextId <- newTVarIO 0
        progressCancel <- newTVarIO mempty
        pure ProgressData{..}
      resPendingResponses <- newTVarIO emptyIxMap
      resRegistrationsNot <- newTVarIO mempty
      resRegistrationsReq <- newTVarIO mempty
      resLspId            <- newTVarIO 0
      pure LanguageContextState{..}

    -- Call the 'duringInitialization' callback to let the server kick stuff up
    let env = LanguageContextEnv handlers onConfigurationChange sendFunc stateVars (params ^. LSP.capabilities) rootDir
        handlers = transmuteHandlers interpreter staticHandlers
        interpreter = interpretHandler initializationResult
    initializationResult <- ExceptT $ doInitialize env req

    let serverCaps = inferServerCapabilities (params ^. LSP.capabilities) options handlers
    liftIO $ sendResp $ makeResponseMessage (req ^. LSP.id) (InitializeResult serverCaps (serverInfo options))
    pure env
  where
    makeResponseMessage rid result = ResponseMessage "2.0" (Just rid) (Right result)
    makeResponseError origId err = ResponseMessage "2.0" (Just origId) (Left err)

    initializeErrorHandler :: (ResponseError -> IO ()) -> E.SomeException -> IO (Maybe a)
    initializeErrorHandler sendResp e = do
        sendResp $ ResponseError InternalError msg Nothing
        pure Nothing
      where
        msg = T.pack $ unwords ["Error on initialize:", show e]


-- | Infers the capabilities based on registered handlers, and sets the appropriate options.
-- A provider should be set to Nothing if the server does not support it, unless it is a
-- static option.
inferServerCapabilities :: ClientCapabilities -> Options -> Handlers m -> ServerCapabilities
inferServerCapabilities clientCaps o h =
  ServerCapabilities
    { _textDocumentSync                 = sync
    , _hoverProvider                    = supportedBool STextDocumentHover
    , _completionProvider               = completionProvider
    , _declarationProvider              = supportedBool STextDocumentDeclaration
    , _signatureHelpProvider            = signatureHelpProvider
    , _definitionProvider               = supportedBool STextDocumentDefinition
    , _typeDefinitionProvider           = supportedBool STextDocumentTypeDefinition
    , _implementationProvider           = supportedBool STextDocumentImplementation
    , _referencesProvider               = supportedBool STextDocumentReferences
    , _documentHighlightProvider        = supportedBool STextDocumentDocumentHighlight
    , _documentSymbolProvider           = supportedBool STextDocumentDocumentSymbol
    , _codeActionProvider               = codeActionProvider
    , _codeLensProvider                 = supported' STextDocumentCodeLens $ CodeLensOptions
                                              (Just False)
                                              (supported SCodeLensResolve)
    , _documentFormattingProvider       = supportedBool STextDocumentFormatting
    , _documentRangeFormattingProvider  = supportedBool STextDocumentRangeFormatting
    , _documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , _renameProvider                   = renameProvider
    , _documentLinkProvider             = supported' STextDocumentDocumentLink $ DocumentLinkOptions
                                              (Just False)
                                              (supported SDocumentLinkResolve)
    , _colorProvider                    = supportedBool STextDocumentDocumentColor
    , _foldingRangeProvider             = supportedBool STextDocumentFoldingRange
    , _executeCommandProvider           = executeCommandProvider
    , _selectionRangeProvider           = supportedBool STextDocumentSelectionRange
    , _callHierarchyProvider            = supportedBool STextDocumentPrepareCallHierarchy
    , _semanticTokensProvider           = semanticTokensProvider
    , _workspaceSymbolProvider          = supported SWorkspaceSymbol
    , _workspace                        = Just workspace
    -- TODO: Add something for experimental
    , _experimental                     = Nothing :: Maybe Value
    }
  where

    -- | For when we just return a simple @true@/@false@ to indicate if we
    -- support the capability
    supportedBool = Just . InL . supported_b

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

    completionProvider
      | supported_b STextDocumentCompletion = Just $
          CompletionOptions
            Nothing
            (map T.singleton <$> completionTriggerCharacters o)
            (map T.singleton <$> completionAllCommitCharacters o)
            (supported SCompletionItemResolve)
      | otherwise = Nothing

    clientSupportsCodeActionKinds = isJust $
      clientCaps ^? LSP.textDocument . _Just . LSP.codeAction . _Just . LSP.codeActionLiteralSupport

    codeActionProvider
      | clientSupportsCodeActionKinds
      , supported_b STextDocumentCodeAction = Just $ case codeActionKinds o of
          Just ks -> InR $ CodeActionOptions Nothing (Just (List ks)) (supported SCodeLensResolve)
          Nothing -> InL True
      | supported_b STextDocumentCodeAction = Just (InL True)
      | otherwise = Just (InL False)

    signatureHelpProvider
      | supported_b STextDocumentSignatureHelp = Just $
          SignatureHelpOptions
            Nothing
            (List . map T.singleton <$> signatureHelpTriggerCharacters o)
            (List . map T.singleton <$> signatureHelpRetriggerCharacters o)
      | otherwise = Nothing

    documentOnTypeFormattingProvider
      | supported_b STextDocumentOnTypeFormatting
      , Just (first :| rest) <- documentOnTypeFormattingTriggerCharacters o = Just $
          DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singleton) rest))
      | supported_b STextDocumentOnTypeFormatting
      , Nothing <- documentOnTypeFormattingTriggerCharacters o =
          error "documentOnTypeFormattingTriggerCharacters needs to be set if a documentOnTypeFormattingHandler is set"
      | otherwise = Nothing

    executeCommandProvider
      | supported_b SWorkspaceExecuteCommand
      , Just cmds <- executeCommandCommands o = Just (ExecuteCommandOptions Nothing (List cmds))
      | supported_b SWorkspaceExecuteCommand
      , Nothing <- executeCommandCommands o =
          error "executeCommandCommands needs to be set if a executeCommandHandler is set"
      | otherwise = Nothing

    clientSupportsPrepareRename = fromMaybe False $
      clientCaps ^? LSP.textDocument . _Just . LSP.rename . _Just . LSP.prepareSupport . _Just

    renameProvider
      | clientSupportsPrepareRename
      , supported_b STextDocumentRename
      , supported_b STextDocumentPrepareRename = Just $
          InR . RenameOptions Nothing . Just $ True
      | supported_b STextDocumentRename = Just (InL True)
      | otherwise = Just (InL False)

    -- Always provide the default legend
    -- TODO: allow user-provided legend via 'Options', or at least user-provided types
    semanticTokensProvider = Just $ InL $ SemanticTokensOptions Nothing def semanticTokenRangeProvider semanticTokenFullProvider
    semanticTokenRangeProvider
      | supported_b STextDocumentSemanticTokensRange = Just $ SemanticTokensRangeBool True
      | otherwise = Nothing
    semanticTokenFullProvider
      | supported_b STextDocumentSemanticTokensFull = Just $ SemanticTokensFullDelta $ SemanticTokensDeltaClientCapabilities $ supported STextDocumentSemanticTokensFullDelta
      | otherwise = Nothing

    sync = case textDocumentSync o of
            Just x -> Just (InL x)
            Nothing -> Nothing

    workspace = WorkspaceServerCapabilities workspaceFolder
    workspaceFolder = supported' SWorkspaceDidChangeWorkspaceFolders $
        -- sign up to receive notifications
        WorkspaceFoldersServerCapabilities (Just True) (Just (InR True))

-- | Invokes the registered dynamic or static handlers for the given message and
-- method, as well as doing some bookkeeping.
handle :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> SClientMethod meth -> ClientMessage meth -> m ()
handle logger m msg =
  case m of
    SWorkspaceDidChangeWorkspaceFolders -> handle' logger (Just updateWorkspaceFolders) m msg
    SWorkspaceDidChangeConfiguration    -> handle' logger (Just $ handleConfigChange logger) m msg
    STextDocumentDidOpen                -> handle' logger (Just $ vfsFunc logger openVFS) m msg
    STextDocumentDidChange              -> handle' logger (Just $ vfsFunc logger changeFromClientVFS) m msg
    STextDocumentDidClose               -> handle' logger (Just $ vfsFunc logger closeVFS) m msg
    SWindowWorkDoneProgressCancel       -> handle' logger (Just $ progressCancelHandler logger) m msg
    _ -> handle' logger Nothing m msg


handle' :: forall m t (meth :: Method FromClient t) config
        . (m ~ LspM config)
        => LogAction m (WithSeverity LspProcessingLog)
        -> Maybe (ClientMessage meth -> m ())
           -- ^ An action to be run before invoking the handler, used for
           -- bookkeeping stuff like the vfs etc.
        -> SClientMethod meth
        -> ClientMessage meth
        -> m ()
handle' logger mAction m msg = do
  maybe (return ()) (\f -> f msg) mAction

  dynReqHandlers <- getsState resRegistrationsReq
  dynNotHandlers <- getsState resRegistrationsNot

  env <- getLspEnv
  let Handlers{reqHandlers, notHandlers} = resHandlers env

  let mkRspCb :: RequestMessage (m1 :: Method FromClient Request) -> Either ResponseError (ResponseResult m1) -> IO ()
      mkRspCb req (Left  err) = runLspT env $ sendToClient $
        FromServerRsp (req ^. LSP.method) $ ResponseMessage "2.0" (Just (req ^. LSP.id)) (Left err)
      mkRspCb req (Right rsp) = runLspT env $ sendToClient $
        FromServerRsp (req ^. LSP.method) $ ResponseMessage "2.0" (Just (req ^. LSP.id)) (Right rsp)

  case splitClientMethod m of
    IsClientNot -> case pickHandler dynNotHandlers notHandlers of
      Just h -> liftIO $ h msg
      Nothing
        | SExit <- m -> exitNotificationHandler logger msg
        | otherwise -> do
            reportMissingHandler

    IsClientReq -> case pickHandler dynReqHandlers reqHandlers of
      Just h -> liftIO $ h msg (mkRspCb msg)
      Nothing
        | SShutdown <- m -> liftIO $ shutdownRequestHandler msg (mkRspCb msg)
        | otherwise -> do
            let errorMsg = T.pack $ unwords ["lsp:no handler for: ", show m]
                err = ResponseError MethodNotFound errorMsg Nothing
            sendToClient $
              FromServerRsp (msg ^. LSP.method) $ ResponseMessage "2.0" (Just (msg ^. LSP.id)) (Left err)

    IsClientEither -> case msg of
      NotMess noti -> case pickHandler dynNotHandlers notHandlers of
        Just h -> liftIO $ h noti
        Nothing -> reportMissingHandler
      ReqMess req -> case pickHandler dynReqHandlers reqHandlers of
        Just h -> liftIO $ h req (mkRspCb req)
        Nothing -> do
          let errorMsg = T.pack $ unwords ["lsp:no handler for: ", show m]
              err = ResponseError MethodNotFound errorMsg Nothing
          sendToClient $
            FromServerRsp (req ^. LSP.method) $ ResponseMessage "2.0" (Just (req ^. LSP.id)) (Left err)
  where
    -- | Checks to see if there's a dynamic handler, and uses it in favour of the
    -- static handler, if it exists.
    pickHandler :: RegistrationMap t -> SMethodMap (ClientMessageHandler IO t) -> Maybe (Handler IO meth)
    pickHandler dynHandlerMap staticHandler = case (SMethodMap.lookup m dynHandlerMap, SMethodMap.lookup m staticHandler) of
      (Just (Pair _ (ClientMessageHandler h)), _) -> Just h
      (Nothing, Just (ClientMessageHandler h)) -> Just h
      (Nothing, Nothing) -> Nothing

    -- '$/' notifications should/could be ignored by server.
    -- Don't log errors in that case.
    -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
    reportMissingHandler :: m ()
    reportMissingHandler =
      let optional = isOptionalNotification m
      in logger <& MissingHandler optional m `WithSeverity` if optional then Warning else Error
    isOptionalNotification (SCustomMethod method)
      | "$/" `T.isPrefixOf` method = True
    isOptionalNotification _  = False

progressCancelHandler :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> Message WindowWorkDoneProgressCancel -> m ()
progressCancelHandler logger (NotificationMessage _ _ (WorkDoneProgressCancelParams tid)) = do
  pdata <- getsState (progressCancel . resProgressData)
  case Map.lookup tid pdata of
    Nothing -> return ()
    Just cancelAction -> do
      logger <& ProgressCancel tid `WithSeverity` Debug
      liftIO cancelAction

exitNotificationHandler :: (MonadIO m) => LogAction m (WithSeverity LspProcessingLog) -> Handler m Exit
exitNotificationHandler logger _ = do
  logger <& Exiting `WithSeverity` Info
  liftIO exitSuccess

-- | Default Shutdown handler
shutdownRequestHandler :: Handler IO Shutdown
shutdownRequestHandler _req k = do
  k $ Right Empty

handleConfigChange :: (m ~ LspM config) => LogAction m (WithSeverity LspProcessingLog) -> Message WorkspaceDidChangeConfiguration -> m ()
handleConfigChange logger req = do
  parseConfig <- LspT $ asks resParseConfig
  let settings = req ^. LSP.params . LSP.settings
  res <- stateState resConfig $ \oldConfig -> case parseConfig oldConfig settings of
    Left err -> (Left err, oldConfig)
    Right !newConfig -> (Right (), newConfig)
  case res of
    Left err -> do
      logger <& ConfigurationParseError settings err `WithSeverity` Error
    Right () -> pure ()

vfsFunc :: forall m n a config
        . (m ~ LspM config, n ~ WriterT [WithSeverity VfsLog] (State VFS))
        => LogAction m (WithSeverity LspProcessingLog)
        -> (LogAction n (WithSeverity VfsLog) -> a -> n ())
        -> a
        -> m ()
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
updateWorkspaceFolders :: Message WorkspaceDidChangeWorkspaceFolders -> LspM config ()
updateWorkspaceFolders (NotificationMessage _ _ params) = do
  let List toRemove = params ^. LSP.event . LSP.removed
      List toAdd = params ^. LSP.event . LSP.added
      newWfs oldWfs = foldr delete oldWfs toRemove <> toAdd
  modifyState resWorkspaceFolders newWfs

-- ---------------------------------------------------------------------
