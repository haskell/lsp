{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.Message.Registration where

import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.Misc

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Prettyprinter

--- STATIC REGISTRATION

type family ServerCapability (m :: Method f t) where
  ServerCapability Method_TextDocumentDeclaration = Bool |? (DeclarationOptions |? DeclarationRegistrationOptions)
  ServerCapability Method_TextDocumentImplementation = Bool |? (ImplementationOptions |? ImplementationRegistrationOptions)
  ServerCapability Method_TextDocumentTypeDefinition = Bool |? (TypeDefinitionOptions |? TypeDefinitionRegistrationOptions)
  ServerCapability Method_TextDocumentCompletion = CompletionOptions
  ServerCapability Method_TextDocumentHover = Bool |? HoverOptions
  ServerCapability Method_TextDocumentSignatureHelp = SignatureHelpOptions
  ServerCapability Method_TextDocumentDefinition = Bool |? DefinitionOptions
  ServerCapability Method_TextDocumentReferences = Bool |? ReferenceOptions
  ServerCapability Method_TextDocumentDocumentHighlight = Bool |? DocumentHighlightOptions
  ServerCapability Method_TextDocumentDocumentSymbol = Bool |? DocumentSymbolOptions
  ServerCapability Method_TextDocumentCodeAction = Bool |? CodeActionOptions
  ServerCapability Method_TextDocumentCodeLens = CodeLensOptions
  ServerCapability Method_TextDocumentDocumentLink = DocumentLinkOptions
  ServerCapability Method_TextDocumentDocumentColor = Bool |? (DocumentColorOptions |? DocumentColorRegistrationOptions)
  ServerCapability Method_WorkspaceSymbol = Bool |? WorkspaceSymbolOptions
  ServerCapability Method_TextDocumentFormatting = Bool |? DocumentFormattingOptions
  ServerCapability Method_TextDocumentRangeFormatting = Bool |? DocumentRangeFormattingOptions
  ServerCapability Method_TextDocumentOnTypeFormatting = DocumentOnTypeFormattingOptions
  ServerCapability Method_TextDocumentRename = Bool |? RenameOptions
  ServerCapability Method_TextDocumentFoldingRange = Bool |? (FoldingRangeOptions |? FoldingRangeRegistrationOptions)
  ServerCapability Method_TextDocumentSelectionRange = Bool |? (SelectionRangeOptions |? SelectionRangeRegistrationOptions)
  ServerCapability Method_WorkspaceExecuteCommand = ExecuteCommandOptions
  ServerCapability Method_TextDocumentPrepareCallHierarchy = Bool |? (CallHierarchyOptions |? CallHierarchyRegistrationOptions)
  ServerCapability Method_TextDocumentLinkedEditingRange = Bool |? (LinkedEditingRangeOptions |? LinkedEditingRangeRegistrationOptions)
  ServerCapability Method_TextDocumentSemanticTokensFull = SemanticTokensOptions |? SemanticTokensRegistrationOptions
  ServerCapability Method_TextDocumentMoniker = Bool |? (MonikerOptions |? MonikerRegistrationOptions)
  ServerCapability Method_TextDocumentPrepareTypeHierarchy = Bool |? (TypeHierarchyOptions |? TypeHierarchyRegistrationOptions)
  ServerCapability Method_TextDocumentInlineValue = Bool |? (InlineValueOptions |? InlineValueRegistrationOptions)
  ServerCapability Method_TextDocumentInlayHint = Bool |? (InlayHintOptions |? InlayHintRegistrationOptions)
  ServerCapability Method_TextDocumentDiagnostic = DiagnosticOptions |? DiagnosticRegistrationOptions

  ServerCapability Method_CompletionItemResolve = Data.Void.Void -- TODO?
  ServerCapability Method_CodeActionResolve = Data.Void.Void -- TODO?
  ServerCapability Method_CodeLensResolve = Data.Void.Void -- TODO?
  ServerCapability Method_DocumentLinkResolve = Data.Void.Void -- TODO?
  ServerCapability Method_CallHierarchyIncomingCalls = Data.Void.Void -- TODO?
  ServerCapability Method_CallHierarchyOutgoingCalls = Data.Void.Void -- TODO?
  ServerCapability Method_TextDocumentSemanticTokensFullDelta = Data.Void.Void -- TODO?
  ServerCapability Method_TextDocumentSemanticTokensRange = Data.Void.Void -- TODO?
  ServerCapability Method_WorkspaceSemanticTokensRefresh = Data.Void.Void -- TODO?
  ServerCapability Method_TypeHierarchySupertypes = Data.Void.Void
  ServerCapability Method_TypeHierarchySubtypes = Data.Void.Void
  ServerCapability Method_WorkspaceInlineValueRefresh = Data.Void.Void
  ServerCapability Method_InlayHintResolve = Data.Void.Void
  ServerCapability Method_WorkspaceInlayHintRefresh = Data.Void.Void
  ServerCapability Method_WorkspaceDiagnostic = Data.Void.Void
  ServerCapability Method_WorkspaceDiagnosticRefresh = Data.Void.Void

  ServerCapability Method_WorkspaceWorkspaceFolders = Maybe Data.Void.Void -- TODO
  ServerCapability Method_WorkspaceConfiguration = Maybe Data.Void.Void -- TODO
  ServerCapability Method_TextDocumentColorPresentation = (Row.Rec ("workDoneProgress" Row..== (Maybe Bool) Row..+ ("documentSelector" Row..== (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null) Row..+ Row.Empty)))
  ServerCapability Method_WindowWorkDoneProgressCreate = Maybe Data.Void.Void
  ServerCapability Method_WindowShowDocument = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceWillCreateFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_WorkspaceWillRenameFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_WorkspaceWillDeleteFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_ClientRegisterCapability = Maybe Data.Void.Void
  ServerCapability Method_ClientUnregisterCapability = Maybe Data.Void.Void
  ServerCapability Method_Initialize = Maybe Data.Void.Void
  ServerCapability Method_Shutdown = Maybe Data.Void.Void
  ServerCapability Method_WindowShowMessageRequest = Maybe Data.Void.Void
  ServerCapability Method_TextDocumentWillSaveWaitUntil = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  ServerCapability Method_WorkspaceSymbolResolve = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceCodeLensRefresh = Maybe Data.Void.Void
  ServerCapability Method_TextDocumentPrepareRename = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceApplyEdit = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceDidChangeWorkspaceFolders = Maybe Data.Void.Void
  ServerCapability Method_WindowWorkDoneProgressCancel = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceDidCreateFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_WorkspaceDidRenameFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_WorkspaceDidDeleteFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  ServerCapability Method_NotebookDocumentDidOpen = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  ServerCapability Method_NotebookDocumentDidChange = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  ServerCapability Method_NotebookDocumentDidSave = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  ServerCapability Method_NotebookDocumentDidClose = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  ServerCapability Method_Initialized = Maybe Data.Void.Void
  ServerCapability Method_Exit = Maybe Data.Void.Void
  ServerCapability Method_WorkspaceDidChangeConfiguration = Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions.DidChangeConfigurationRegistrationOptions
  ServerCapability Method_WindowShowMessage = Maybe Data.Void.Void
  ServerCapability Method_WindowLogMessage = Maybe Data.Void.Void
  ServerCapability Method_TelemetryEvent = Maybe Data.Void.Void
  ServerCapability Method_TextDocumentDidOpen = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  ServerCapability Method_TextDocumentDidChange = Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions.TextDocumentChangeRegistrationOptions
  ServerCapability Method_TextDocumentDidClose = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  ServerCapability Method_TextDocumentDidSave = Language.LSP.Protocol.Internal.Types.TextDocumentSaveRegistrationOptions.TextDocumentSaveRegistrationOptions
  ServerCapability Method_TextDocumentWillSave = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  ServerCapability Method_WorkspaceDidChangeWatchedFiles = Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions.DidChangeWatchedFilesRegistrationOptions
  ServerCapability Method_TextDocumentPublishDiagnostics = Maybe Data.Void.Void
  ServerCapability Method_SetTrace = Maybe Data.Void.Void
  ServerCapability Method_LogTrace = Maybe Data.Void.Void
  ServerCapability Method_CancelRequest = Maybe Data.Void.Void
  ServerCapability Method_Progress = Maybe Data.Void.Void
  ServerCapability (Method_CustomMethod s) = Data.Void.Void

--- DYNAMIC REGISTRATION

-- | Typed dynamic registration type, with correct options.
data TRegistration (m :: Method ClientToServer t) = TRegistration
  { _id :: Text
  -- ^ The id used to register the request. The id can be used to deregister
  -- the request again.
  , _method :: SClientMethod m
  -- ^ The method / capability to register for.
  , _registerOptions :: !(Maybe (ServerCapability m))
  -- ^ Options necessary for the registration.
  -- Make this strict to aid the pattern matching exhaustiveness checker
  }
  deriving stock (Generic)

deriving stock instance Eq (ServerCapability m) => Eq (TRegistration m)
deriving stock instance Show (ServerCapability m) => Show (TRegistration m)

-- TODO: can we do this generically somehow?
-- This generates the function
-- regHelper :: SMethod m
--           -> (( Show (ServerCapability m)
--               , ToJSON (ServerCapability m)
--               , FromJSON ($regOptTcon m)
--              => x)
--           -> x
makeRegHelper ''RegistrationOptions

instance ToJSON (TRegistration m) where
  toJSON x@(TRegistration _ m _) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TRegistration m) instance Pretty (TRegistration m)

data SomeRegistration = forall t (m :: Method ClientToServer t). SomeRegistration (TRegistration m)

instance ToJSON SomeRegistration where
  toJSON (SomeRegistration r) = toJSON r

instance FromJSON SomeRegistration where
  parseJSON = withObject "Registration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TRegistration <$> o .: "id" <*> pure m <*> regHelper m (o .: "registerOptions")
    pure (SomeRegistration r)

instance Show SomeRegistration where
  show (SomeRegistration r@(TRegistration _ m _)) = regHelper m (show r)

deriving via ViaJSON SomeRegistration instance Pretty SomeRegistration

toUntypedRegistration :: TRegistration m -> Registration
toUntypedRegistration (TRegistration i meth opts) = Registration i (T.pack $ someMethodToMethodString $ SomeMethod meth) (Just $ regHelper meth (toJSON opts))

toSomeRegistration :: Registration -> Maybe SomeRegistration
toSomeRegistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing

-- ---------------------------------------------------------------------

-- | Typed dynamic unregistration type.
data TUnregistration (m :: Method ClientToServer t) = TUnregistration
  { _id :: Text
  -- ^ The id used to unregister the request or notification. Usually an id
  -- provided during the register request.
  , _method :: SMethod m
  -- ^ The method / capability to unregister for.
  }
  deriving stock (Generic)

deriving stock instance Eq (TUnregistration m)
deriving stock instance Show (TUnregistration m)

instance ToJSON (TUnregistration m) where
  toJSON x@(TUnregistration _ m) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TUnregistration m) instance Pretty (TUnregistration m)

data SomeUnregistration = forall t (m :: Method ClientToServer t). SomeUnregistration (TUnregistration m)

instance ToJSON SomeUnregistration where
  toJSON (SomeUnregistration r) = toJSON r

instance FromJSON SomeUnregistration where
  parseJSON = withObject "Unregistration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TUnregistration <$> o .: "id" <*> pure m
    pure (SomeUnregistration r)

deriving via ViaJSON SomeUnregistration instance Pretty SomeUnregistration

toUntypedUnregistration :: TUnregistration m -> Unregistration
toUntypedUnregistration (TUnregistration i meth) = Unregistration i (T.pack $ someMethodToMethodString $ SomeMethod meth)

toSomeUnregistration :: Unregistration -> Maybe SomeUnregistration
toSomeUnregistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing
