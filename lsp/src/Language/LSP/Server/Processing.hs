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
module Language.LSP.Server.Processing where

import Control.Lens hiding (List, Empty)
import Data.Aeson hiding (Options)
import Data.Aeson.Types hiding (Options)
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.Server.Core
import Language.LSP.VFS
import Data.Functor.Product
import qualified Control.Exception as E
import Data.Monoid hiding (Product)
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.IxMap
import System.Directory
import System.Log.Logger
import qualified Data.Dependent.Map as DMap
import Data.Maybe
import Data.Dependent.Map (DMap)
import qualified Data.Map.Strict as Map
import System.Exit

processMessage :: BSL.ByteString -> LspM config ()
processMessage jsonStr = do
  pendingResponsesVar <- LspT $ asks $ resPendingResponses . resState
  join $ liftIO $ atomically $ fmap handleErrors $ runExceptT $ do
      val <- except $ eitherDecode jsonStr
      pending <- lift $ readTVar pendingResponsesVar
      msg <- except $ parseEither (parser pending) val
      lift $ case msg of
        FromClientMess m mess ->
          pure $ handle m mess
        FromClientRsp (Pair (ServerResponseCallback f) (Const !newMap)) res -> do
          writeTVar pendingResponsesVar newMap
          pure $ liftIO $ f (res ^. LSP.result)
  where
    parser :: ResponseMap -> Value -> Parser (FromClientMessage' (Product ServerResponseCallback (Const ResponseMap)))
    parser rm = parseClientMessage $ \i ->
      let (mhandler, newMap) = pickFromIxMap i rm
        in (\(Pair m handler) -> (m,Pair handler (Const newMap))) <$> mhandler

    handleErrors = either (sendErrorLog . errMsg) id

    errMsg err = TL.toStrict $ TL.unwords
      [ "lsp:incoming message parse error."
      , TL.decodeUtf8 jsonStr
      , TL.pack err
      ] <> "\n"

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

    liftIO $ case rootDir of
      Nothing -> return ()
      Just dir -> do
        debugM "lsp.initializeRequestHandler" $ "Setting current dir to project root:" ++ dir
        unless (null dir) $ setCurrentDirectory dir

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
      IsClientNot -> DMap.member m $ notHandlers h
      IsClientReq -> DMap.member m $ reqHandlers h
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

    sync = case textDocumentSync o of
            Just x -> Just (InL x)
            Nothing -> Nothing

    workspace = WorkspaceServerCapabilities workspaceFolder
    workspaceFolder = supported' SWorkspaceDidChangeWorkspaceFolders $
        -- sign up to receive notifications
        WorkspaceFoldersServerCapabilities (Just True) (Just (InR True))

-- | Invokes the registered dynamic or static handlers for the given message and
-- method, as well as doing some bookkeeping.
handle :: SClientMethod m -> ClientMessage m -> LspM config ()
handle m msg =
  case m of
    SWorkspaceDidChangeWorkspaceFolders -> handle' (Just updateWorkspaceFolders) m msg
    SWorkspaceDidChangeConfiguration    -> handle' (Just handleConfigChange) m msg
    STextDocumentDidOpen                -> handle' (Just $ vfsFunc openVFS) m msg
    STextDocumentDidChange              -> handle' (Just $ vfsFunc changeFromClientVFS) m msg
    STextDocumentDidClose               -> handle' (Just $ vfsFunc closeVFS) m msg
    SWindowWorkDoneProgressCancel       -> handle' (Just progressCancelHandler) m msg
    _ -> handle' Nothing m msg


handle' :: forall t (m :: Method FromClient t) config.
           Maybe (ClientMessage m -> LspM config ())
           -- ^ An action to be run before invoking the handler, used for
           -- bookkeeping stuff like the vfs etc.
        -> SClientMethod m
        -> ClientMessage m
        -> LspM config ()
handle' mAction m msg = do
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
        | SExit <- m -> liftIO $ exitNotificationHandler msg
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
    pickHandler :: RegistrationMap t -> DMap SMethod (ClientMessageHandler IO t) -> Maybe (Handler IO m)
    pickHandler dynHandlerMap staticHandler = case (DMap.lookup m dynHandlerMap, DMap.lookup m staticHandler) of
      (Just (Pair _ (ClientMessageHandler h)), _) -> Just h
      (Nothing, Just (ClientMessageHandler h)) -> Just h
      (Nothing, Nothing) -> Nothing

    -- '$/' notifications should/could be ignored by server.
    -- Don't log errors in that case.
    -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
    reportMissingHandler :: LspM config ()
    reportMissingHandler
      | isOptionalNotification m = return ()
      | otherwise = do
          let errorMsg = T.pack $ unwords ["lsp:no handler for: ", show m]
          sendErrorLog errorMsg
    isOptionalNotification (SCustomMethod method)
      | "$/" `T.isPrefixOf` method = True
    isOptionalNotification _  = False

progressCancelHandler :: Message WindowWorkDoneProgressCancel -> LspM config ()
progressCancelHandler (NotificationMessage _ _ (WorkDoneProgressCancelParams tid)) = do
  mact <- Map.lookup tid <$> getsState (progressCancel . resProgressData)
  case mact of
    Nothing -> return ()
    Just cancelAction -> liftIO $ cancelAction

exitNotificationHandler :: Handler IO Exit
exitNotificationHandler =  \_ -> do
  noticeM "lsp.exitNotificationHandler" "Got exit, exiting"
  exitSuccess

-- | Default Shutdown handler
shutdownRequestHandler :: Handler IO Shutdown
shutdownRequestHandler = \_req k -> do
  k $ Right Empty

handleConfigChange :: Message WorkspaceDidChangeConfiguration -> LspM config ()
handleConfigChange req = do
  parseConfig <- LspT $ asks resParseConfig
  res <- stateState resConfig $ \oldConfig -> case parseConfig oldConfig (req ^. LSP.params . LSP.settings) of
    Left err -> (Left err, oldConfig)
    Right !newConfig -> (Right (), newConfig)
  case res of
    Left err -> do
      let msg = T.pack $ unwords
            ["lsp:configuration parse error.", show req, show err]
      sendErrorLog msg
    Right () -> pure ()

vfsFunc :: (VFS -> b -> (VFS, [String])) -> b -> LspM config ()
vfsFunc modifyVfs req = do
  join $ stateState resVFS $ \(VFSData vfs rm) ->
    let (!vfs', ls) = modifyVfs vfs req
    in (liftIO $ mapM_ (debugM "lsp.vfsFunc") ls,VFSData vfs' rm)

-- | Updates the list of workspace folders
updateWorkspaceFolders :: Message WorkspaceDidChangeWorkspaceFolders -> LspM config ()
updateWorkspaceFolders (NotificationMessage _ _ params) = do
  let List toRemove = params ^. LSP.event . LSP.removed
      List toAdd = params ^. LSP.event . LSP.added
      newWfs oldWfs = foldr delete oldWfs toRemove <> toAdd
  modifyState resWorkspaceFolders newWfs

-- ---------------------------------------------------------------------

