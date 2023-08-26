{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server (
  LspLog(..)
  , runLspServer
  , runLspServerWithHandles
  , module Core
  ) where

import Colog.Core
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson (Value)
import Data.List.NonEmpty hiding (map)
import Data.Map qualified as Map
import Data.Singletons
import Data.Text qualified as T
import JSONRPC.RPC qualified as Untyped
import JSONRPC.Server qualified as Untyped
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method as Method
import JSONRPC.Typed.Server as Server
import Language.LSP.Diagnostics as Diagnostics
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Server.Config as Config
import Language.LSP.Server.Core as Core
import Language.LSP.Server.DynamicRegistration as Registration
import Language.LSP.Server.Progress as Progress
import Language.LSP.Server.Shutdown as Shutdown
import Language.LSP.Server.WorkspaceFolders as WorkspaceFolders
import Language.LSP.VFS as VFS
import Prettyprinter
import System.IO

data LspLog
  = ServerLog Untyped.ServerLog
  | VfsLog VfsLog
  | ConfigLog LspConfigLog
  | ProgressLog LspProgressLog
  | ShutdownLog LspShutdownLog
  | RegistrationLog LspRegistrationLog
  deriving stock (Show)

instance Pretty LspLog where
  pretty (ServerLog l) = pretty l
  pretty (VfsLog l) = pretty l
  pretty (ConfigLog l) = pretty l
  pretty (ProgressLog l) = pretty l
  pretty (ShutdownLog l) = pretty l
  pretty (RegistrationLog l) = pretty l

-- | Call this to initialize the session
initializeRequestHandler ::
  LogAction IO (WithSeverity LspLog) ->
  ServerHandle Server LSP.Method ->
  Core.ServerDefinition config ->
  MethodParams Method_Initialize ->
  IO (Either (ResponseError Method_Initialize) (MethodResult Method_Initialize))
initializeRequestHandler logger serverHandle Core.ServerDefinition{..} p = do
  -- TODO: exceptions
  runExceptT $ do
    let clientCapabilities = p ^. L.capabilities

    vfsHandle <- liftIO $ do
      let vfsLogger = cmap (fmap VfsLog) logger
      VFS.new vfsLogger
    let vh = vfsServerHandlers vfsHandle

    registrationHandle <- liftIO $ do
      let registrationLogger = cmap (fmap RegistrationLog) logger
      Registration.new registrationLogger serverHandle clientCapabilities

    configHandle <- liftIO $ do
      let configLogger = cmap (fmap ConfigLog) logger
      Config.new
        configLogger
        serverHandle
        registrationHandle
        clientCapabilities
        configSection
        defaultConfig
        parseConfig
        onConfigChange
    liftIO $ Config.updateFromInitializeParams configHandle p
    let ch = configHandlers configHandle

    progressHandle <- liftIO $ do
      let progressLogger = cmap (fmap ProgressLog) logger
      Progress.new
        progressLogger
        (optProgressStartDelay options)
        (optProgressUpdateDelay options)
        serverHandle
        clientCapabilities
    let ph = progressHandlers progressHandle

    shutdownHandle <- liftIO $ do
      let shutdownLogger = cmap (fmap ShutdownLog) logger
      Shutdown.new shutdownLogger
    let sh = shutdownHandlers shutdownHandle

    workspaceFoldersHandle <- liftIO $ do
      WorkspaceFolders.new (WorkspaceFolders.fromInitializeParams p)
    let wh = workspaceFoldersHandlers workspaceFoldersHandle

    diagnosticsHandle <- liftIO $ do
      Diagnostics.new serverHandle

    env <- liftIO $ do
      let rootPath =
            getFirst $
              foldMap
                First
                [ p ^? L.rootUri . _L >>= uriToFilePath
                , p ^? L.rootPath . _Just . _L <&> T.unpack
                ]

      pure $
        LanguageServerHandle
          { serverHandle
          , configHandle
          , vfsHandle
          , clientCapabilities
          , rootPath
          , diagnosticsHandle
          , workspaceFoldersHandle
          , progressHandle
          , shutdownHandle
          , registrationHandle
          }

    userHandlers <- ExceptT $ doInitialize env p
    let
      fullHandlers = userHandlers <> vh <> ch <> ph <> sh <> wh
    liftIO $ atomically $ setHandlers serverHandle fullHandlers

    let serverCaps = inferServerCapabilities clientCapabilities options fullHandlers
    let serverInfo = optServerInfo options
    pure (InitializeResult serverCaps serverInfo)

mkServerFun :: LogAction IO (WithSeverity LspLog) -> Core.ServerDefinition config -> ServerFunction Server LSP.Method
mkServerFun logger defn env = do
  let ih = requestHandler SMethod_Initialize $ mkRequestHandler (initializeRequestHandler logger env defn)
  pure Server.ServerDefinition{Server.initialHandlers = ih}

runLspServer :: Core.ServerDefinition config -> IO ()
runLspServer = runLspServerWithHandles logger stdin stdout
 where
  prettyMsg l = "[" <> viaShow (getSeverity l) <> "] " <> pretty (getMsg l)
  -- log to stderr
  ioLogger :: LogAction IO (WithSeverity LspLog)
  ioLogger = cmap (show . prettyMsg) logStringStderr
  -- TODO: figure out how to set this up nicely, needs the server env
  -- log to the client, at default verbosity (log at info and above, show errors)
  -- clientLogger :: LogAction IO (WithSeverity LspLog)
  -- clientLogger = cmap (fmap (T.pack . show . pretty)) defaultClientLogger
  logger :: LogAction IO (WithSeverity LspLog)
  logger = ioLogger -- <> clinetLogger

runLspServerWithHandles :: LogAction IO (WithSeverity LspLog) -> Handle -> Handle -> Core.ServerDefinition config -> IO ()
runLspServerWithHandles logger hin hout defn = do
  rpc <- Untyped.initRpcWithHandles (cmap (fmap Untyped.IoLog) serverLogger) hin hout
  (_env, act) <- Untyped.runServerIn serverLogger rpc $ toUntypedServerFunction (mkServerFun logger defn)
  act
 where
  serverLogger = cmap (fmap ServerLog) logger

-------------------

{- | Infers the capabilities based on registered handlers, and sets the appropriate options.
 A provider should be set to Nothing if the server does not support it, unless it is a
 static option.
-}
inferServerCapabilities :: ClientCapabilities -> Options -> Handlers r k -> ServerCapabilities
inferServerCapabilities _clientCaps o (Handlers h) =
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

  supported :: forall m. SMethod m -> Maybe Bool
  supported = Just . supported_b

  supported_b :: forall m. SMethod m -> Bool
  supported_b m =
    let meth = toUntypedMethod (fromSing m)
     in case sMethodType m of
          Method.SNotification -> Map.member meth $ Untyped.notHandlers h
          Method.SRequest -> Map.member meth $ Untyped.reqHandlers h

  singletonList :: a -> [a]
  singletonList x = [x]

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
          DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singletonList) rest))
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
