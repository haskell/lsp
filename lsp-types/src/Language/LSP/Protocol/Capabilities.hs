{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Protocol.Capabilities (
  LSPVersion (..),

  -- * Client capabilities
  ClientCapability,
  clientCapability,
  fullLatestClientCaps,
  fullClientCapsForVersion,
  fullClientCapsForVersionAndMethod,
  dynamicRegistrationSupported,

  -- * Server capabilities
  ServerCapability,
  serverCapability,
) where

import Control.Lens
import Data.Kind (Type)
import Data.Maybe
import Data.Set qualified as Set
import Data.Void
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Prelude hiding (min)

-- | A specific version of the LSP specification.
data LSPVersion = LSPVersion Int Int

---- CLIENT CAPABILITIES

type ClientCapability :: forall f t. Method f t -> Type

-- See Note [Capability mappings]

{- | The client capability associated with a given method.

Where several methods are provided together (e.g. the three `callHierarchy` methods), we associate all of them
with the capaiblity, even if there is one "primary" method.
-}
type family ClientCapability (m :: Method f t) where
  ClientCapability Method_TextDocumentDeclaration = DeclarationClientCapabilities
  ClientCapability Method_TextDocumentImplementation = ImplementationClientCapabilities
  ClientCapability Method_TextDocumentTypeDefinition = TypeDefinitionClientCapabilities
  ClientCapability Method_TextDocumentHover = HoverClientCapabilities
  ClientCapability Method_TextDocumentSignatureHelp = SignatureHelpClientCapabilities
  ClientCapability Method_TextDocumentDefinition = DefinitionClientCapabilities
  ClientCapability Method_TextDocumentReferences = ReferenceClientCapabilities
  ClientCapability Method_TextDocumentDocumentHighlight = DocumentHighlightClientCapabilities
  ClientCapability Method_TextDocumentDocumentSymbol = DocumentSymbolClientCapabilities
  ClientCapability Method_TextDocumentFoldingRange = FoldingRangeClientCapabilities
  ClientCapability Method_TextDocumentSelectionRange = SelectionRangeClientCapabilities
  ClientCapability Method_WorkspaceExecuteCommand = ExecuteCommandClientCapabilities
  ClientCapability Method_TextDocumentLinkedEditingRange = LinkedEditingRangeClientCapabilities
  ClientCapability Method_TextDocumentMoniker = MonikerClientCapabilities
  ClientCapability Method_WorkspaceWorkspaceFolders = Bool
  ClientCapability Method_TextDocumentCompletion = CompletionClientCapabilities
  ClientCapability Method_CompletionItemResolve = CompletionClientCapabilities
  ClientCapability Method_TextDocumentCodeAction = CodeActionClientCapabilities
  ClientCapability Method_CodeActionResolve = CodeActionClientCapabilities
  ClientCapability Method_TextDocumentCodeLens = CodeLensClientCapabilities
  ClientCapability Method_CodeLensResolve = CodeLensClientCapabilities
  ClientCapability Method_WorkspaceCodeLensRefresh = CodeLensWorkspaceClientCapabilities
  ClientCapability Method_TextDocumentDocumentLink = DocumentLinkClientCapabilities
  ClientCapability Method_DocumentLinkResolve = DocumentLinkClientCapabilities
  ClientCapability Method_WorkspaceSymbol = WorkspaceSymbolClientCapabilities
  ClientCapability Method_WorkspaceSymbolResolve = WorkspaceSymbolClientCapabilities
  ClientCapability Method_TextDocumentRename = RenameClientCapabilities
  ClientCapability Method_TextDocumentPrepareRename = RenameClientCapabilities
  ClientCapability Method_TextDocumentDocumentColor = DocumentColorClientCapabilities
  ClientCapability Method_TextDocumentColorPresentation = DocumentColorClientCapabilities
  ClientCapability Method_TextDocumentFormatting = DocumentFormattingClientCapabilities
  ClientCapability Method_TextDocumentRangeFormatting = DocumentRangeFormattingClientCapabilities
  ClientCapability Method_TextDocumentOnTypeFormatting = DocumentOnTypeFormattingClientCapabilities
  ClientCapability Method_TextDocumentPrepareCallHierarchy = CallHierarchyClientCapabilities
  ClientCapability Method_CallHierarchyIncomingCalls = CallHierarchyClientCapabilities
  ClientCapability Method_CallHierarchyOutgoingCalls = CallHierarchyClientCapabilities
  ClientCapability Method_TextDocumentSemanticTokensFull = SemanticTokensClientCapabilities
  ClientCapability Method_TextDocumentSemanticTokensFullDelta = SemanticTokensClientCapabilities
  ClientCapability Method_TextDocumentSemanticTokensRange = SemanticTokensClientCapabilities
  ClientCapability Method_WorkspaceSemanticTokensRefresh = SemanticTokensWorkspaceClientCapabilities
  ClientCapability Method_TextDocumentPrepareTypeHierarchy = TypeHierarchyClientCapabilities
  ClientCapability Method_TypeHierarchySupertypes = TypeHierarchyClientCapabilities
  ClientCapability Method_TypeHierarchySubtypes = TypeHierarchyClientCapabilities
  ClientCapability Method_TextDocumentInlineValue = InlineValueClientCapabilities
  ClientCapability Method_WorkspaceInlineValueRefresh = InlineValueWorkspaceClientCapabilities
  ClientCapability Method_TextDocumentInlayHint = InlayHintClientCapabilities
  ClientCapability Method_InlayHintResolve = InlayHintClientCapabilities
  ClientCapability Method_WorkspaceInlayHintRefresh = InlayHintWorkspaceClientCapabilities
  ClientCapability Method_TextDocumentDiagnostic = DiagnosticClientCapabilities
  ClientCapability Method_WorkspaceDiagnostic = DiagnosticWorkspaceClientCapabilities
  ClientCapability Method_WorkspaceDiagnosticRefresh = DiagnosticWorkspaceClientCapabilities
  ClientCapability Method_WorkspaceWillCreateFiles = FileOperationClientCapabilities
  ClientCapability Method_WorkspaceWillRenameFiles = FileOperationClientCapabilities
  ClientCapability Method_WorkspaceWillDeleteFiles = FileOperationClientCapabilities
  ClientCapability Method_WorkspaceDidCreateFiles = FileOperationClientCapabilities
  ClientCapability Method_WorkspaceDidRenameFiles = FileOperationClientCapabilities
  ClientCapability Method_WorkspaceDidDeleteFiles = FileOperationClientCapabilities
  ClientCapability Method_TextDocumentDidOpen = TextDocumentSyncClientCapabilities
  ClientCapability Method_TextDocumentDidChange = TextDocumentSyncClientCapabilities
  ClientCapability Method_TextDocumentDidClose = TextDocumentSyncClientCapabilities
  ClientCapability Method_TextDocumentDidSave = TextDocumentSyncClientCapabilities
  ClientCapability Method_TextDocumentWillSave = TextDocumentSyncClientCapabilities
  ClientCapability Method_TextDocumentWillSaveWaitUntil = TextDocumentSyncClientCapabilities
  ClientCapability Method_NotebookDocumentDidOpen = NotebookDocumentSyncClientCapabilities
  ClientCapability Method_NotebookDocumentDidChange = NotebookDocumentSyncClientCapabilities
  ClientCapability Method_NotebookDocumentDidSave = NotebookDocumentSyncClientCapabilities
  ClientCapability Method_NotebookDocumentDidClose = NotebookDocumentSyncClientCapabilities
  ClientCapability Method_WorkspaceDidChangeConfiguration = DidChangeConfigurationClientCapabilities
  ClientCapability Method_WorkspaceDidChangeWatchedFiles = DidChangeWatchedFilesClientCapabilities
  ClientCapability Method_WorkspaceDidChangeWorkspaceFolders = Void
  ClientCapability Method_TextDocumentPublishDiagnostics = PublishDiagnosticsClientCapabilities
  ClientCapability Method_WorkspaceConfiguration = Bool
  ClientCapability Method_WorkspaceApplyEdit = Bool
  ClientCapability Method_WindowWorkDoneProgressCreate = Bool
  ClientCapability Method_WindowWorkDoneProgressCancel = Bool
  ClientCapability Method_WindowShowMessage = ShowMessageRequestClientCapabilities
  ClientCapability Method_WindowShowMessageRequest = ShowMessageRequestClientCapabilities
  ClientCapability Method_WindowShowDocument = ShowDocumentClientCapabilities
  -- All required by default, no capabilities
  ClientCapability Method_Progress = Void
  ClientCapability Method_WindowLogMessage = Void
  ClientCapability Method_ClientRegisterCapability = Void
  ClientCapability Method_ClientUnregisterCapability = Void
  ClientCapability Method_Initialize = Void
  ClientCapability Method_Initialized = Void
  ClientCapability Method_Shutdown = Void
  ClientCapability Method_Exit = Void
  ClientCapability Method_TelemetryEvent = Void
  ClientCapability Method_SetTrace = Void
  ClientCapability Method_LogTrace = Void
  ClientCapability Method_CancelRequest = Void
  ClientCapability (Method_CustomMethod s) = Void

-- See Note [Capability mappings]

-- | A lens which focusses on the (possibly absent) client capability associated with a method.
clientCapability :: forall m. SMethod m -> Lens' ClientCapabilities (Maybe (ClientCapability m))
clientCapability = \case
  SMethod_TextDocumentDeclaration -> td . L.declaration
  SMethod_TextDocumentImplementation -> td . L.implementation
  SMethod_TextDocumentTypeDefinition -> td . L.typeDefinition
  SMethod_TextDocumentHover -> td . L.hover
  SMethod_TextDocumentSignatureHelp -> td . L.signatureHelp
  SMethod_TextDocumentDefinition -> td . L.definition
  SMethod_TextDocumentReferences -> td . L.references
  SMethod_TextDocumentDocumentHighlight -> td . L.documentHighlight
  SMethod_TextDocumentDocumentSymbol -> td . L.documentSymbol
  SMethod_TextDocumentFoldingRange -> td . L.foldingRange
  SMethod_TextDocumentSelectionRange -> td . L.selectionRange
  SMethod_WorkspaceExecuteCommand -> ws . L.executeCommand
  SMethod_TextDocumentMoniker -> td . L.moniker
  SMethod_TextDocumentCompletion -> td . L.completion
  SMethod_CompletionItemResolve -> td . L.completion
  SMethod_TextDocumentCodeAction -> td . L.codeAction
  SMethod_CodeActionResolve -> td . L.codeAction
  SMethod_TextDocumentCodeLens -> td . L.codeLens
  SMethod_CodeLensResolve -> td . L.codeLens
  SMethod_WorkspaceCodeLensRefresh -> ws . L.codeLens
  SMethod_TextDocumentDocumentLink -> td . L.documentLink
  SMethod_DocumentLinkResolve -> td . L.documentLink
  SMethod_TextDocumentDocumentColor -> td . L.colorProvider
  SMethod_TextDocumentColorPresentation -> td . L.colorProvider
  SMethod_WorkspaceSymbol -> ws . L.symbol
  SMethod_WorkspaceSymbolResolve -> ws . L.symbol
  SMethod_TextDocumentFormatting -> td . L.formatting
  SMethod_TextDocumentRangeFormatting -> td . L.rangeFormatting
  SMethod_TextDocumentOnTypeFormatting -> td . L.onTypeFormatting
  SMethod_TextDocumentRename -> td . L.rename
  SMethod_TextDocumentPrepareRename -> td . L.rename
  SMethod_TextDocumentPrepareCallHierarchy -> td . L.callHierarchy
  SMethod_CallHierarchyIncomingCalls -> td . L.callHierarchy
  SMethod_CallHierarchyOutgoingCalls -> td . L.callHierarchy
  SMethod_TextDocumentLinkedEditingRange -> td . L.linkedEditingRange
  SMethod_TextDocumentSemanticTokensFull -> td . L.semanticTokens
  SMethod_TextDocumentSemanticTokensFullDelta -> td . L.semanticTokens
  SMethod_TextDocumentSemanticTokensRange -> td . L.semanticTokens
  SMethod_WorkspaceSemanticTokensRefresh -> ws . L.semanticTokens
  SMethod_TextDocumentPrepareTypeHierarchy -> td . L.typeHierarchy
  SMethod_TypeHierarchySubtypes -> td . L.typeHierarchy
  SMethod_TypeHierarchySupertypes -> td . L.typeHierarchy
  SMethod_TextDocumentInlineValue -> td . L.inlineValue
  SMethod_WorkspaceInlineValueRefresh -> ws . L.inlineValue
  SMethod_TextDocumentInlayHint -> td . L.inlayHint
  SMethod_InlayHintResolve -> td . L.inlayHint
  SMethod_WorkspaceInlayHintRefresh -> ws . L.inlayHint
  SMethod_TextDocumentDiagnostic -> td . L.diagnostic
  SMethod_WorkspaceDiagnostic -> ws . L.diagnostics
  SMethod_WorkspaceDiagnosticRefresh -> ws . L.diagnostics
  SMethod_WorkspaceWorkspaceFolders -> ws . L.workspaceFolders
  SMethod_WorkspaceWillCreateFiles -> ws . L.fileOperations
  SMethod_WorkspaceWillRenameFiles -> ws . L.fileOperations
  SMethod_WorkspaceWillDeleteFiles -> ws . L.fileOperations
  SMethod_WorkspaceDidCreateFiles -> ws . L.fileOperations
  SMethod_WorkspaceDidRenameFiles -> ws . L.fileOperations
  SMethod_WorkspaceDidDeleteFiles -> ws . L.fileOperations
  SMethod_TextDocumentDidOpen -> td . L.synchronization
  SMethod_TextDocumentDidChange -> td . L.synchronization
  SMethod_TextDocumentDidClose -> td . L.synchronization
  SMethod_TextDocumentDidSave -> td . L.synchronization
  SMethod_TextDocumentWillSave -> td . L.synchronization
  SMethod_TextDocumentWillSaveWaitUntil -> td . L.synchronization
  SMethod_NotebookDocumentDidOpen -> nbs
  SMethod_NotebookDocumentDidChange -> nbs
  SMethod_NotebookDocumentDidSave -> nbs
  SMethod_NotebookDocumentDidClose -> nbs
  SMethod_WorkspaceDidChangeConfiguration -> ws . L.didChangeConfiguration
  SMethod_WorkspaceDidChangeWatchedFiles -> ws . L.didChangeWatchedFiles
  SMethod_TextDocumentPublishDiagnostics -> td . L.publishDiagnostics
  SMethod_WorkspaceConfiguration -> ws . L.configuration
  SMethod_WindowWorkDoneProgressCreate -> wd . L.workDoneProgress
  SMethod_WindowWorkDoneProgressCancel -> wd . L.workDoneProgress
  SMethod_WorkspaceApplyEdit -> ws . L.applyEdit
  SMethod_WindowShowDocument -> wd . L.showDocument
  SMethod_WindowShowMessageRequest -> wd . L.showMessage
  SMethod_WindowShowMessage -> wd . L.showMessage
  SMethod_WorkspaceDidChangeWorkspaceFolders -> noCap
  SMethod_Progress -> noCap
  SMethod_WindowLogMessage -> noCap
  SMethod_ClientRegisterCapability -> noCap
  SMethod_ClientUnregisterCapability -> noCap
  SMethod_Initialize -> noCap
  SMethod_Initialized -> noCap
  SMethod_Shutdown -> noCap
  SMethod_Exit -> noCap
  SMethod_TelemetryEvent -> noCap
  SMethod_SetTrace -> noCap
  SMethod_LogTrace -> noCap
  SMethod_CancelRequest -> noCap
  (SMethod_CustomMethod _s) -> noCap
 where
  ws :: Lens' ClientCapabilities WorkspaceClientCapabilities
  ws = L.workspace . non emptyWorkspaceClientCaps
  wd :: Lens' ClientCapabilities WindowClientCapabilities
  wd = L.window . non emptyWindowClientCaps
  td :: Lens' ClientCapabilities TextDocumentClientCapabilities
  td = L.textDocument . non emptyTextDocumentClientCaps
  -- This is messed up because, unlike literally everything else, `NotebookDocumentClientCapabilities.synchronization` is
  -- a mandatory field, so if we don't have it we need to unset the parent `notebookDocument` field. Maybe.
  nbs :: Lens' ClientCapabilities (Maybe NotebookDocumentSyncClientCapabilities)
  nbs = lens g s
   where
    g c = c ^. L.notebookDocument . non emptyNotebookDocumentClientCaps . L.synchronization . to Just
    s c Nothing = c & L.notebookDocument .~ Nothing
    s c (Just v) = c & L.notebookDocument . non emptyNotebookDocumentClientCaps . L.synchronization .~ v

-- TODO: can we do this generically somehow?

{- | Whether the client supports dynamic registration for the given method.

Note that here we only consider the "main" method against which you dynamically register, so
even though e.g. we associate the client capabilities for code lenses with `codeLens/resolve`,
we don't ever say that you can dynamically register `codeLens/resolve`, because you in fact
need to register `textDocument/codeLens`.
-}
dynamicRegistrationSupported :: SMethod m -> ClientCapabilities -> Bool
dynamicRegistrationSupported m caps = fromMaybe False $ case m of
  SMethod_WorkspaceDidChangeConfiguration -> caps ^? dyn m
  SMethod_WorkspaceDidChangeWatchedFiles -> caps ^? dyn m
  SMethod_WorkspaceSymbol -> caps ^? dyn m
  SMethod_WorkspaceExecuteCommand -> caps ^? dyn m
  SMethod_WorkspaceWillCreateFiles -> caps ^? dyn m
  SMethod_WorkspaceDidCreateFiles -> caps ^? dyn m
  SMethod_WorkspaceWillDeleteFiles -> caps ^? dyn m
  SMethod_WorkspaceDidDeleteFiles -> caps ^? dyn m
  SMethod_TextDocumentDidOpen -> caps ^? dyn m
  SMethod_TextDocumentDidChange -> caps ^? dyn m
  SMethod_TextDocumentDidClose -> caps ^? dyn m
  SMethod_TextDocumentCompletion -> caps ^? dyn m
  SMethod_TextDocumentHover -> caps ^? dyn m
  SMethod_TextDocumentSignatureHelp -> caps ^? dyn m
  SMethod_TextDocumentDeclaration -> caps ^? dyn m
  SMethod_TextDocumentDefinition -> caps ^? dyn m
  SMethod_TextDocumentTypeDefinition -> caps ^? dyn m
  SMethod_TextDocumentImplementation -> caps ^? dyn m
  SMethod_TextDocumentReferences -> caps ^? dyn m
  SMethod_TextDocumentDocumentHighlight -> caps ^? dyn m
  SMethod_TextDocumentDocumentSymbol -> caps ^? dyn m
  SMethod_TextDocumentCodeAction -> caps ^? dyn m
  SMethod_TextDocumentCodeLens -> caps ^? dyn m
  SMethod_TextDocumentDocumentLink -> caps ^? dyn m
  SMethod_TextDocumentDocumentColor -> caps ^? dyn m
  SMethod_TextDocumentColorPresentation -> caps ^? dyn m
  SMethod_TextDocumentFormatting -> caps ^? dyn m
  SMethod_TextDocumentRangeFormatting -> caps ^? dyn m
  SMethod_TextDocumentOnTypeFormatting -> caps ^? dyn m
  SMethod_TextDocumentRename -> caps ^? dyn m
  SMethod_TextDocumentFoldingRange -> caps ^? dyn m
  SMethod_TextDocumentSelectionRange -> caps ^? dyn m
  SMethod_TextDocumentLinkedEditingRange -> caps ^? dyn m
  SMethod_TextDocumentPrepareCallHierarchy -> caps ^? dyn m
  SMethod_TextDocumentInlayHint -> caps ^? dyn m
  SMethod_TextDocumentInlineValue -> caps ^? dyn m
  SMethod_TextDocumentMoniker -> caps ^? dyn m
  SMethod_TextDocumentPrepareTypeHierarchy -> caps ^? dyn m
  SMethod_TextDocumentDiagnostic -> caps ^? dyn m
  -- semantic tokens is messed up due to it having you register with an otherwise non-existent method
  -- SMethod_TextDocumentSemanticTokens       -> capDyn $ clientCaps ^? L.textDocument . _Just . L.semanticTokens . _Just
  -- Notebook document methods alway support dynamic registration, it seems?
  _ -> Just False
 where
  dyn :: L.HasDynamicRegistration (ClientCapability m) (Maybe Bool) => SMethod m -> Traversal' ClientCapabilities Bool
  dyn m1 = clientCapability m1 . _Just . L.dynamicRegistration . _Just

-- | Client capabilities for full support of the current LSP specification.
fullLatestClientCaps :: ClientCapabilities
fullLatestClientCaps = fullClientCapsForVersion (LSPVersion maxBound maxBound)

{-
TODO: the "since" conditions are out-of-date/needs an audit
TODO: can we generate this? process the 'since' annotations in the metamodel?
-}

-- | Client capabilities for full support of the LSP specification up until a version.
fullClientCapsForVersion :: LSPVersion -> ClientCapabilities
fullClientCapsForVersion v@(LSPVersion maj min) = caps
 where
  methCaps :: SMethod m -> Maybe (ClientCapability m)
  methCaps = fullClientCapsForVersionAndMethod v

  caps =
    ClientCapabilities
      { _workspace = Just workspace
      , _textDocument = Just td
      , _window = Just window
      , _general = since 3 16 general
      , _notebookDocument = NotebookDocumentClientCapabilities <$> methCaps SMethod_NotebookDocumentDidOpen
      , _experimental = Nothing
      }

  window =
    WindowClientCapabilities
      { _workDoneProgress = methCaps SMethod_WindowWorkDoneProgressCreate
      , _showMessage = methCaps SMethod_WindowShowMessageRequest
      , _showDocument = methCaps SMethod_WindowShowDocument
      }

  general =
    GeneralClientCapabilities
      { _staleRequestSupport = since 3 16 (StaleRequestSupportOptions{_cancel = True, _retryOnContentModified = []})
      , _regularExpressions = since 3 16 $ RegularExpressionsClientCapabilities (RegularExpressionEngineKind "") Nothing
      , _markdown = since 3 16 $ MarkdownClientCapabilities "" Nothing (Just [])
      , _positionEncodings = since 3 17 [PositionEncodingKind_UTF16]
      }

  workspace =
    WorkspaceClientCapabilities
      { _applyEdit = methCaps SMethod_WorkspaceApplyEdit
      , _workspaceEdit =
          Just
            ( WorkspaceEditClientCapabilities
                (Just True)
                (since 3 13 [ResourceOperationKind_Create, ResourceOperationKind_Delete, ResourceOperationKind_Rename])
                Nothing
                (since 3 16 True)
                (since 3 16 (ChangeAnnotationsSupportOptions{_groupsOnLabel = Just True}))
            )
      , _didChangeConfiguration = methCaps SMethod_WorkspaceDidChangeConfiguration
      , _didChangeWatchedFiles = methCaps SMethod_WorkspaceDidChangeWatchedFiles
      , _symbol = methCaps SMethod_WorkspaceSymbol
      , _executeCommand = methCaps SMethod_WorkspaceExecuteCommand
      , _codeLens = methCaps SMethod_WorkspaceCodeLensRefresh
      , _workspaceFolders = methCaps SMethod_WorkspaceWorkspaceFolders
      , _configuration = methCaps SMethod_WorkspaceConfiguration
      , _semanticTokens = methCaps SMethod_WorkspaceSemanticTokensRefresh
      , _inlayHint = methCaps SMethod_WorkspaceInlayHintRefresh
      , _fileOperations = methCaps SMethod_WorkspaceDidCreateFiles
      , _inlineValue = methCaps SMethod_WorkspaceInlineValueRefresh
      , _diagnostics = methCaps SMethod_WorkspaceDiagnostic
      }

  td =
    TextDocumentClientCapabilities
      { _synchronization = methCaps SMethod_TextDocumentDidOpen
      , _completion = methCaps SMethod_TextDocumentCompletion
      , _hover = methCaps SMethod_TextDocumentHover
      , _signatureHelp = methCaps SMethod_TextDocumentSignatureHelp
      , _references = methCaps SMethod_TextDocumentReferences
      , _documentHighlight = methCaps SMethod_TextDocumentDocumentHighlight
      , _documentSymbol = methCaps SMethod_TextDocumentDocumentSymbol
      , _formatting = methCaps SMethod_TextDocumentFormatting
      , _rangeFormatting = methCaps SMethod_TextDocumentRangeFormatting
      , _onTypeFormatting = methCaps SMethod_TextDocumentOnTypeFormatting
      , _declaration = methCaps SMethod_TextDocumentDeclaration
      , _definition = methCaps SMethod_TextDocumentDefinition
      , _typeDefinition = methCaps SMethod_TextDocumentTypeDefinition
      , _implementation = methCaps SMethod_TextDocumentImplementation
      , _codeAction = methCaps SMethod_TextDocumentCodeAction
      , _codeLens = methCaps SMethod_TextDocumentCodeLens
      , _documentLink = methCaps SMethod_TextDocumentDocumentLink
      , _colorProvider = methCaps SMethod_TextDocumentDocumentColor
      , _rename = methCaps SMethod_TextDocumentRename
      , _publishDiagnostics = methCaps SMethod_TextDocumentPublishDiagnostics
      , _foldingRange = methCaps SMethod_TextDocumentFoldingRange
      , _selectionRange = methCaps SMethod_TextDocumentSelectionRange
      , _callHierarchy = methCaps SMethod_TextDocumentPrepareCallHierarchy
      , _semanticTokens = methCaps SMethod_TextDocumentSemanticTokensFull
      , _linkedEditingRange = methCaps SMethod_TextDocumentLinkedEditingRange
      , _moniker = methCaps SMethod_TextDocumentMoniker
      , _inlayHint = methCaps SMethod_TextDocumentInlayHint
      , _typeHierarchy = methCaps SMethod_TextDocumentPrepareTypeHierarchy
      , _inlineValue = methCaps SMethod_TextDocumentInlineValue
      , _diagnostic = methCaps SMethod_TextDocumentDiagnostic
      }

  since :: Int -> Int -> a -> Maybe a
  since x y a
    | maj >= x && min >= y = Just a
    | otherwise = Nothing

-- TODO: make this only include the caps that are necessary for that specific method. That will require
-- some code to merge capabilities...

-- | Client capabilities for full support of a specific method in the LSP specification up until a version.
fullClientCapsForVersionAndMethod :: LSPVersion -> SMethod m -> Maybe (ClientCapability m)
fullClientCapsForVersionAndMethod (LSPVersion maj min) = \case
  SMethod_TextDocumentDeclaration -> declaration
  SMethod_TextDocumentImplementation -> implementation
  SMethod_TextDocumentTypeDefinition -> typeDefinition
  SMethod_TextDocumentHover -> hover
  SMethod_TextDocumentSignatureHelp -> signatureHelp
  SMethod_TextDocumentDefinition -> definition
  SMethod_TextDocumentReferences -> references
  SMethod_TextDocumentDocumentHighlight -> documentHighlight
  SMethod_TextDocumentDocumentSymbol -> documentSymbol
  SMethod_TextDocumentFoldingRange -> foldingRange
  SMethod_TextDocumentSelectionRange -> selectionRange
  SMethod_WorkspaceExecuteCommand -> executeCommand
  SMethod_TextDocumentMoniker -> moniker
  SMethod_TextDocumentCompletion -> completion
  SMethod_CompletionItemResolve -> completion
  SMethod_TextDocumentCodeAction -> codeAction
  SMethod_CodeActionResolve -> codeAction
  SMethod_TextDocumentCodeLens -> codeLens
  SMethod_CodeLensResolve -> codeLens
  SMethod_WorkspaceCodeLensRefresh -> wsCodeLens
  SMethod_TextDocumentDocumentLink -> documentLink
  SMethod_DocumentLinkResolve -> documentLink
  SMethod_TextDocumentDocumentColor -> colorProvider
  SMethod_TextDocumentColorPresentation -> colorProvider
  SMethod_WorkspaceSymbol -> wsSymbol
  SMethod_WorkspaceSymbolResolve -> wsSymbol
  SMethod_TextDocumentFormatting -> formatting
  SMethod_TextDocumentRangeFormatting -> rangeFormatting
  SMethod_TextDocumentOnTypeFormatting -> onTypeFormatting
  SMethod_TextDocumentRename -> rename
  SMethod_TextDocumentPrepareRename -> rename
  SMethod_TextDocumentPrepareCallHierarchy -> callHierarchy
  SMethod_CallHierarchyIncomingCalls -> callHierarchy
  SMethod_CallHierarchyOutgoingCalls -> callHierarchy
  SMethod_TextDocumentLinkedEditingRange -> linkedEditingRange
  SMethod_TextDocumentSemanticTokensFull -> semanticTokens
  SMethod_TextDocumentSemanticTokensFullDelta -> semanticTokens
  SMethod_TextDocumentSemanticTokensRange -> semanticTokens
  SMethod_WorkspaceSemanticTokensRefresh -> wsSemanticTokens
  SMethod_TextDocumentPrepareTypeHierarchy -> typeHierarchy
  SMethod_TypeHierarchySubtypes -> typeHierarchy
  SMethod_TypeHierarchySupertypes -> typeHierarchy
  SMethod_TextDocumentInlineValue -> inlineValue
  SMethod_WorkspaceInlineValueRefresh -> wsInlineValue
  SMethod_TextDocumentInlayHint -> inlayHint
  SMethod_InlayHintResolve -> inlayHint
  SMethod_WorkspaceInlayHintRefresh -> wsInlayHint
  SMethod_TextDocumentDiagnostic -> diagnostic
  SMethod_WorkspaceDiagnostic -> diagnostics
  SMethod_WorkspaceDiagnosticRefresh -> diagnostics
  SMethod_WorkspaceWorkspaceFolders -> workspaceFolders
  SMethod_WorkspaceWillCreateFiles -> fileOperations
  SMethod_WorkspaceWillRenameFiles -> fileOperations
  SMethod_WorkspaceWillDeleteFiles -> fileOperations
  SMethod_WorkspaceDidCreateFiles -> fileOperations
  SMethod_WorkspaceDidRenameFiles -> fileOperations
  SMethod_WorkspaceDidDeleteFiles -> fileOperations
  SMethod_TextDocumentDidOpen -> synchronization
  SMethod_TextDocumentDidChange -> synchronization
  SMethod_TextDocumentDidClose -> synchronization
  SMethod_TextDocumentDidSave -> synchronization
  SMethod_TextDocumentWillSave -> synchronization
  SMethod_TextDocumentWillSaveWaitUntil -> synchronization
  SMethod_NotebookDocumentDidOpen -> notebookDocumentSync
  SMethod_NotebookDocumentDidChange -> notebookDocumentSync
  SMethod_NotebookDocumentDidSave -> notebookDocumentSync
  SMethod_NotebookDocumentDidClose -> notebookDocumentSync
  SMethod_WorkspaceDidChangeConfiguration -> didChangeConfiguration
  SMethod_WorkspaceDidChangeWatchedFiles -> didChangeWatchedFiles
  SMethod_TextDocumentPublishDiagnostics -> publishDiagnostics
  SMethod_WorkspaceConfiguration -> configuration
  SMethod_WindowWorkDoneProgressCreate -> workDoneProgress
  SMethod_WindowWorkDoneProgressCancel -> workDoneProgress
  SMethod_WorkspaceApplyEdit -> applyEdit
  SMethod_WindowShowDocument -> showDocument
  SMethod_WindowShowMessageRequest -> showMessage
  SMethod_WindowShowMessage -> showMessage
  SMethod_WorkspaceDidChangeWorkspaceFolders -> Nothing
  SMethod_Progress -> Nothing
  SMethod_WindowLogMessage -> Nothing
  SMethod_ClientRegisterCapability -> Nothing
  SMethod_ClientUnregisterCapability -> Nothing
  SMethod_Initialize -> Nothing
  SMethod_Initialized -> Nothing
  SMethod_Shutdown -> Nothing
  SMethod_Exit -> Nothing
  SMethod_TelemetryEvent -> Nothing
  SMethod_SetTrace -> Nothing
  SMethod_LogTrace -> Nothing
  SMethod_CancelRequest -> Nothing
  (SMethod_CustomMethod _s) -> Nothing
 where
  workDoneProgress = since 3 15 True
  showMessage = since 3 16 $ ShowMessageRequestClientCapabilities Nothing
  showDocument = since 3 16 $ ShowDocumentClientCapabilities True
  applyEdit = Just True
  didChangeConfiguration = Just (DidChangeConfigurationClientCapabilities dynamicReg)
  didChangeWatchedFiles = Just (DidChangeWatchedFilesClientCapabilities dynamicReg (Just True))
  wsSymbol =
    Just $
      WorkspaceSymbolClientCapabilities
        dynamicReg
        (since 3 4 (ClientSymbolKindOptions{_valueSet = Just sKs}))
        (since 3 16 (ClientSymbolTagOptions{_valueSet = [SymbolTag_Deprecated]}))
        (since 3 17 (ClientSymbolResolveOptions{_properties = []}))
  executeCommand = Just (ExecuteCommandClientCapabilities dynamicReg)
  wsCodeLens = Just (CodeLensWorkspaceClientCapabilities $ Just True)
  workspaceFolders = since 3 6 True
  configuration = since 3 6 True
  wsSemanticTokens = since 3 16 (SemanticTokensWorkspaceClientCapabilities $ Just True)
  wsInlayHint = since 3 17 (InlayHintWorkspaceClientCapabilities $ Just True)
  fileOperations =
    since 3 16 $
      FileOperationClientCapabilities
        dynamicReg
        (Just True)
        (Just True)
        (Just True)
        (Just True)
        (Just True)
        (Just True)
  wsInlineValue = since 3 17 (InlineValueWorkspaceClientCapabilities $ Just True)
  diagnostics = since 3 17 (DiagnosticWorkspaceClientCapabilities $ Just True)

  notebookDocumentSync = since 3 17 $ NotebookDocumentSyncClientCapabilities dynamicReg (Just True)

  synchronization =
    Just $
      TextDocumentSyncClientCapabilities{_dynamicRegistration = dynamicReg, _willSave = Just True, _willSaveWaitUntil = Just True, _didSave = Just True}
  completion =
    Just $
      CompletionClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _completionItem = Just completionItemCapabilities
        , _completionItemKind = since 3 4 (ClientCompletionItemOptionsKind{_valueSet = Just ciKs})
        , _insertTextMode = since 3 17 InsertTextMode_AsIs
        , _contextSupport = since 3 3 True
        , _completionList = since 3 17 (CompletionListCapabilities{_itemDefaults = Just []})
        }
  hover =
    Just $
      HoverClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _contentFormat = since 3 3 allMarkups
        }
  signatureHelp =
    Just $
      SignatureHelpClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _signatureInformation =
            Just $
              ClientSignatureInformationOptions
                { _documentationFormat = Just allMarkups
                , _parameterInformation = Just (ClientSignatureParameterInformationOptions{_labelOffsetSupport = Just True})
                , _activeParameterSupport = Just True
                }
        , _contextSupport = since 3 16 True
        }
  references = Just $ ReferenceClientCapabilities dynamicReg
  documentHighlight = Just $ DocumentHighlightClientCapabilities dynamicReg
  documentSymbol =
    Just $
      DocumentSymbolClientCapabilities
        { _dynamicRegistration = dynamicReg
        , -- same as workspace symbol kinds
          _symbolKind = Just (ClientSymbolKindOptions{_valueSet = Just sKs})
        , _hierarchicalDocumentSymbolSupport = since 3 10 True
        , _tagSupport = since 3 16 (ClientSymbolTagOptions{_valueSet = [SymbolTag_Deprecated]})
        , _labelSupport = since 3 16 True
        }
  formatting = Just $ DocumentFormattingClientCapabilities dynamicReg
  rangeFormatting = Just $ DocumentRangeFormattingClientCapabilities dynamicReg
  onTypeFormatting = Just $ DocumentOnTypeFormattingClientCapabilities dynamicReg
  declaration = since 3 14 (DeclarationClientCapabilities dynamicReg (Just True))
  definition = Just (DefinitionClientCapabilities dynamicReg (since 3 14 True))
  typeDefinition = since 3 6 (TypeDefinitionClientCapabilities dynamicReg (since 3 14 True))
  implementation = since 3 6 (ImplementationClientCapabilities dynamicReg (since 3 14 True))
  codeAction =
    Just $
      CodeActionClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _codeActionLiteralSupport = since 3 8 (ClientCodeActionLiteralOptions{_codeActionKind = ClientCodeActionKindOptions{_valueSet = Set.toList knownValues}})
        , _isPreferredSupport = since 3 15 True
        , _disabledSupport = since 3 16 True
        , _dataSupport = since 3 16 True
        , _resolveSupport = since 3 16 (ClientCodeActionResolveOptions{_properties = []})
        , _honorsChangeAnnotations = since 3 16 True
        }
  codeLens = Just (CodeLensClientCapabilities dynamicReg)
  documentLink = Just (DocumentLinkClientCapabilities dynamicReg (since 3 15 True))
  colorProvider = since 3 6 (DocumentColorClientCapabilities dynamicReg)
  rename = Just (RenameClientCapabilities dynamicReg (since 3 12 True) (since 3 16 PrepareSupportDefaultBehavior_Identifier) (since 3 16 True))
  publishDiagnostics =
    Just
      PublishDiagnosticsClientCapabilities
        { _relatedInformation = since 3 7 True
        , _tagSupport = since 3 15 (ClientDiagnosticsTagOptions{_valueSet = [DiagnosticTag_Unnecessary, DiagnosticTag_Deprecated]})
        , _versionSupport = since 3 15 True
        , _codeDescriptionSupport = since 3 16 True
        , _dataSupport = since 3 16 True
        }
  foldingRange =
    since 3 10 $
      FoldingRangeClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _rangeLimit = Nothing
        , _lineFoldingOnly = Nothing
        , _foldingRangeKind = since 3 17 (ClientFoldingRangeKindOptions{_valueSet = Just []})
        , _foldingRange = since 3 16 (ClientFoldingRangeOptions{_collapsedText = Just True})
        }
  selectionRange = since 3 5 (SelectionRangeClientCapabilities dynamicReg)
  callHierarchy = since 3 16 (CallHierarchyClientCapabilities dynamicReg)
  semanticTokens =
    since 3 16 $
      SemanticTokensClientCapabilities
        { _dynamicRegistration = Just True
        , _requests = ClientSemanticTokensRequestOptions{_range = Just (InL True), _full = Just (InR (ClientSemanticTokensRequestFullDelta{_delta = Just True}))}
        , _tokenTypes = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenTypes)
        , _tokenModifiers = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenModifiers)
        , _formats = tfs
        , _overlappingTokenSupport = Just True
        , _multilineTokenSupport = Just True
        , _serverCancelSupport = Just True
        , _augmentsSyntaxTokens = Just True
        }
  linkedEditingRange = since 3 16 (LinkedEditingRangeClientCapabilities dynamicReg)
  moniker = since 3 16 (MonikerClientCapabilities dynamicReg)
  inlayHint =
    since 3 17 $
      InlayHintClientCapabilities
        { _dynamicRegistration = dynamicReg
        , _resolveSupport = Just (ClientInlayHintResolveOptions{_properties = []})
        }
  typeHierarchy = since 3 17 (TypeHierarchyClientCapabilities dynamicReg)
  inlineValue = since 3 17 (InlineValueClientCapabilities dynamicReg)
  diagnostic = since 3 17 (DiagnosticClientCapabilities dynamicReg (Just True))

  completionItemCapabilities =
    ClientCompletionItemOptions
      { _snippetSupport = Just True
      , _commitCharactersSupport = Just True
      , _documentationFormat = since 3 3 allMarkups
      , _deprecatedSupport = Just True
      , _preselectSupport = since 3 9 True
      , _tagSupport = since 3 15 (CompletionItemTagOptions{_valueSet = []})
      , _insertReplaceSupport = since 3 16 True
      , _resolveSupport = since 3 16 (ClientCompletionItemResolveOptions{_properties = ["documentation", "details"]})
      , _insertTextModeSupport = since 3 16 (ClientCompletionItemInsertTextModeOptions{_valueSet = []})
      , _labelDetailsSupport = since 3 17 True
      }

  sKs
    | maj >= 3 && min >= 4 = oldSKs ++ newSKs
    | otherwise = oldSKs

  oldSKs =
    [ SymbolKind_File
    , SymbolKind_Module
    , SymbolKind_Namespace
    , SymbolKind_Package
    , SymbolKind_Class
    , SymbolKind_Method
    , SymbolKind_Property
    , SymbolKind_Field
    , SymbolKind_Constructor
    , SymbolKind_Enum
    , SymbolKind_Interface
    , SymbolKind_Function
    , SymbolKind_Variable
    , SymbolKind_Constant
    , SymbolKind_String
    , SymbolKind_Number
    , SymbolKind_Boolean
    , SymbolKind_Array
    ]

  newSKs =
    [ SymbolKind_Object
    , SymbolKind_Key
    , SymbolKind_Null
    , SymbolKind_EnumMember
    , SymbolKind_Struct
    , SymbolKind_Event
    , SymbolKind_Operator
    , SymbolKind_TypeParameter
    ]

  ciKs
    | maj >= 3 && min >= 4 = oldCiKs ++ newCiKs
    | otherwise = oldCiKs

  oldCiKs =
    [ CompletionItemKind_Text
    , CompletionItemKind_Method
    , CompletionItemKind_Function
    , CompletionItemKind_Constructor
    , CompletionItemKind_Field
    , CompletionItemKind_Variable
    , CompletionItemKind_Class
    , CompletionItemKind_Interface
    , CompletionItemKind_Module
    , CompletionItemKind_Property
    , CompletionItemKind_Unit
    , CompletionItemKind_Value
    , CompletionItemKind_Enum
    , CompletionItemKind_Keyword
    , CompletionItemKind_Snippet
    , CompletionItemKind_Color
    , CompletionItemKind_File
    , CompletionItemKind_Reference
    ]

  newCiKs =
    [ CompletionItemKind_Folder
    , CompletionItemKind_EnumMember
    , CompletionItemKind_Constant
    , CompletionItemKind_Struct
    , CompletionItemKind_Event
    , CompletionItemKind_Operator
    , CompletionItemKind_TypeParameter
    ]

  allMarkups = [MarkupKind_PlainText, MarkupKind_Markdown]

  tfs = [TokenFormat_Relative]

  dynamicReg
    | maj >= 3 = Just True
    | otherwise = Nothing
  since :: Int -> Int -> a -> Maybe a
  since x y a
    | maj >= x && min >= y = Just a
    | otherwise = Nothing

---- SERVER CAPABILITIES

type DocumentSyncCaps = TextDocumentSyncOptions |? TextDocumentSyncKind
type NotebookDocumentSyncCaps = NotebookDocumentSyncOptions |? NotebookDocumentSyncRegistrationOptions

type ServerCapability :: forall f t. Method f t -> Type

-- See Note [Capability mappings]

{- | The server capability associated with a given method.

Where several methods are provided together (e.g. the three `callHierarchy` methods), we associate all of them
with the capaiblity, even if there is one "primary" method.

For methods which strictly only need a client capability but which are closely related to a server capability
(e.g. `codeLens/refresh`), we also associate them with that server capability.
-}
type family ServerCapability (m :: Method f t) where
  ServerCapability Method_TextDocumentDeclaration = Bool |? (DeclarationOptions |? DeclarationRegistrationOptions)
  ServerCapability Method_TextDocumentImplementation = Bool |? (ImplementationOptions |? ImplementationRegistrationOptions)
  ServerCapability Method_TextDocumentTypeDefinition = Bool |? (TypeDefinitionOptions |? TypeDefinitionRegistrationOptions)
  ServerCapability Method_TextDocumentHover = Bool |? HoverOptions
  ServerCapability Method_TextDocumentSignatureHelp = SignatureHelpOptions
  ServerCapability Method_TextDocumentDefinition = Bool |? DefinitionOptions
  ServerCapability Method_TextDocumentReferences = Bool |? ReferenceOptions
  ServerCapability Method_TextDocumentDocumentHighlight = Bool |? DocumentHighlightOptions
  ServerCapability Method_TextDocumentDocumentSymbol = Bool |? DocumentSymbolOptions
  ServerCapability Method_TextDocumentFoldingRange = Bool |? (FoldingRangeOptions |? FoldingRangeRegistrationOptions)
  ServerCapability Method_TextDocumentSelectionRange = Bool |? (SelectionRangeOptions |? SelectionRangeRegistrationOptions)
  ServerCapability Method_WorkspaceExecuteCommand = ExecuteCommandOptions
  ServerCapability Method_TextDocumentLinkedEditingRange = Bool |? (LinkedEditingRangeOptions |? LinkedEditingRangeRegistrationOptions)
  ServerCapability Method_TextDocumentMoniker = Bool |? (MonikerOptions |? MonikerRegistrationOptions)
  ServerCapability Method_WorkspaceWorkspaceFolders = WorkspaceFoldersServerCapabilities
  ServerCapability Method_TextDocumentCompletion = CompletionOptions
  ServerCapability Method_CompletionItemResolve = CompletionOptions
  ServerCapability Method_TextDocumentCodeAction = Bool |? CodeActionOptions
  ServerCapability Method_CodeActionResolve = Bool |? CodeActionOptions
  ServerCapability Method_TextDocumentCodeLens = CodeLensOptions
  ServerCapability Method_CodeLensResolve = CodeLensOptions
  ServerCapability Method_WorkspaceCodeLensRefresh = CodeLensOptions
  ServerCapability Method_TextDocumentDocumentLink = DocumentLinkOptions
  ServerCapability Method_DocumentLinkResolve = DocumentLinkOptions
  ServerCapability Method_WorkspaceSymbol = Bool |? WorkspaceSymbolOptions
  ServerCapability Method_WorkspaceSymbolResolve = Bool |? WorkspaceSymbolOptions
  ServerCapability Method_TextDocumentRename = Bool |? RenameOptions
  ServerCapability Method_TextDocumentPrepareRename = Bool |? RenameOptions
  ServerCapability Method_TextDocumentDocumentColor = Bool |? (DocumentColorOptions |? DocumentColorRegistrationOptions)
  ServerCapability Method_TextDocumentColorPresentation = Bool |? (DocumentColorOptions |? DocumentColorRegistrationOptions)
  ServerCapability Method_TextDocumentFormatting = Bool |? DocumentFormattingOptions
  ServerCapability Method_TextDocumentRangeFormatting = Bool |? DocumentRangeFormattingOptions
  ServerCapability Method_TextDocumentOnTypeFormatting = DocumentOnTypeFormattingOptions
  ServerCapability Method_TextDocumentPrepareCallHierarchy = Bool |? (CallHierarchyOptions |? CallHierarchyRegistrationOptions)
  ServerCapability Method_CallHierarchyIncomingCalls = Bool |? (CallHierarchyOptions |? CallHierarchyRegistrationOptions)
  ServerCapability Method_CallHierarchyOutgoingCalls = Bool |? (CallHierarchyOptions |? CallHierarchyRegistrationOptions)
  ServerCapability Method_TextDocumentSemanticTokensFull = SemanticTokensOptions |? SemanticTokensRegistrationOptions
  ServerCapability Method_TextDocumentSemanticTokensFullDelta = SemanticTokensOptions |? SemanticTokensRegistrationOptions
  ServerCapability Method_TextDocumentSemanticTokensRange = SemanticTokensOptions |? SemanticTokensRegistrationOptions
  ServerCapability Method_WorkspaceSemanticTokensRefresh = SemanticTokensOptions |? SemanticTokensRegistrationOptions
  ServerCapability Method_TextDocumentPrepareTypeHierarchy = Bool |? (TypeHierarchyOptions |? TypeHierarchyRegistrationOptions)
  ServerCapability Method_TypeHierarchySupertypes = Bool |? (TypeHierarchyOptions |? TypeHierarchyRegistrationOptions)
  ServerCapability Method_TypeHierarchySubtypes = Bool |? (TypeHierarchyOptions |? TypeHierarchyRegistrationOptions)
  ServerCapability Method_TextDocumentInlineValue = Bool |? (InlineValueOptions |? InlineValueRegistrationOptions)
  ServerCapability Method_WorkspaceInlineValueRefresh = Bool |? (InlineValueOptions |? InlineValueRegistrationOptions)
  ServerCapability Method_TextDocumentInlayHint = Bool |? (InlayHintOptions |? InlayHintRegistrationOptions)
  ServerCapability Method_InlayHintResolve = Bool |? (InlayHintOptions |? InlayHintRegistrationOptions)
  ServerCapability Method_WorkspaceInlayHintRefresh = Bool |? (InlayHintOptions |? InlayHintRegistrationOptions)
  ServerCapability Method_TextDocumentDiagnostic = DiagnosticOptions |? DiagnosticRegistrationOptions
  ServerCapability Method_WorkspaceDiagnostic = DiagnosticOptions |? DiagnosticRegistrationOptions
  ServerCapability Method_WorkspaceDiagnosticRefresh = DiagnosticOptions |? DiagnosticRegistrationOptions
  ServerCapability Method_WorkspaceWillCreateFiles = FileOperationOptions
  ServerCapability Method_WorkspaceWillRenameFiles = FileOperationOptions
  ServerCapability Method_WorkspaceWillDeleteFiles = FileOperationOptions
  ServerCapability Method_WorkspaceDidCreateFiles = FileOperationOptions
  ServerCapability Method_WorkspaceDidRenameFiles = FileOperationOptions
  ServerCapability Method_WorkspaceDidDeleteFiles = FileOperationOptions
  ServerCapability Method_TextDocumentDidOpen = DocumentSyncCaps
  ServerCapability Method_TextDocumentDidChange = DocumentSyncCaps
  ServerCapability Method_TextDocumentDidClose = DocumentSyncCaps
  ServerCapability Method_TextDocumentDidSave = DocumentSyncCaps
  ServerCapability Method_TextDocumentWillSave = DocumentSyncCaps
  ServerCapability Method_TextDocumentWillSaveWaitUntil = DocumentSyncCaps
  ServerCapability Method_NotebookDocumentDidOpen = NotebookDocumentSyncCaps
  ServerCapability Method_NotebookDocumentDidChange = NotebookDocumentSyncCaps
  ServerCapability Method_NotebookDocumentDidSave = NotebookDocumentSyncCaps
  ServerCapability Method_NotebookDocumentDidClose = NotebookDocumentSyncCaps
  -- Dynamic registration only
  ServerCapability Method_WorkspaceDidChangeConfiguration = Void
  ServerCapability Method_WorkspaceDidChangeWatchedFiles = Void
  ServerCapability Method_WorkspaceDidChangeWorkspaceFolders = Void
  -- Client capability only
  ServerCapability Method_TextDocumentPublishDiagnostics = Void
  ServerCapability Method_WorkspaceConfiguration = Void
  -- All required by default, no capabilities
  ServerCapability Method_WindowWorkDoneProgressCreate = Void
  ServerCapability Method_WindowWorkDoneProgressCancel = Void
  ServerCapability Method_Progress = Void
  ServerCapability Method_WindowShowDocument = Void
  ServerCapability Method_WindowShowMessageRequest = Void
  ServerCapability Method_WindowShowMessage = Void
  ServerCapability Method_WindowLogMessage = Void
  ServerCapability Method_ClientRegisterCapability = Void
  ServerCapability Method_ClientUnregisterCapability = Void
  ServerCapability Method_Initialize = Void
  ServerCapability Method_Initialized = Void
  ServerCapability Method_Shutdown = Void
  ServerCapability Method_Exit = Void
  ServerCapability Method_WorkspaceApplyEdit = Void
  ServerCapability Method_TelemetryEvent = Void
  ServerCapability Method_SetTrace = Void
  ServerCapability Method_LogTrace = Void
  ServerCapability Method_CancelRequest = Void
  ServerCapability (Method_CustomMethod s) = Void

-- See Note [Capability mappings]

-- | A lens which focusses on the (possibly absent) server capability associated with a method.
serverCapability :: forall m. SMethod m -> Lens' ServerCapabilities (Maybe (ServerCapability m))
serverCapability = \case
  SMethod_TextDocumentDeclaration -> L.declarationProvider
  SMethod_TextDocumentImplementation -> L.implementationProvider
  SMethod_TextDocumentTypeDefinition -> L.typeDefinitionProvider
  SMethod_TextDocumentHover -> L.hoverProvider
  SMethod_TextDocumentSignatureHelp -> L.signatureHelpProvider
  SMethod_TextDocumentDefinition -> L.definitionProvider
  SMethod_TextDocumentReferences -> L.referencesProvider
  SMethod_TextDocumentDocumentHighlight -> L.documentHighlightProvider
  SMethod_TextDocumentDocumentSymbol -> L.documentSymbolProvider
  SMethod_TextDocumentFoldingRange -> L.foldingRangeProvider
  SMethod_TextDocumentSelectionRange -> L.selectionRangeProvider
  SMethod_WorkspaceExecuteCommand -> L.executeCommandProvider
  SMethod_TextDocumentMoniker -> L.monikerProvider
  SMethod_TextDocumentCompletion -> L.completionProvider
  SMethod_CompletionItemResolve -> L.completionProvider
  SMethod_TextDocumentCodeAction -> L.codeActionProvider
  SMethod_CodeActionResolve -> L.codeActionProvider
  SMethod_TextDocumentCodeLens -> L.codeLensProvider
  SMethod_CodeLensResolve -> L.codeLensProvider
  SMethod_WorkspaceCodeLensRefresh -> L.codeLensProvider
  SMethod_TextDocumentDocumentLink -> L.documentLinkProvider
  SMethod_DocumentLinkResolve -> L.documentLinkProvider
  SMethod_TextDocumentDocumentColor -> L.colorProvider
  SMethod_TextDocumentColorPresentation -> L.colorProvider
  SMethod_WorkspaceSymbol -> L.workspaceSymbolProvider
  SMethod_WorkspaceSymbolResolve -> L.workspaceSymbolProvider
  SMethod_TextDocumentFormatting -> L.documentFormattingProvider
  SMethod_TextDocumentRangeFormatting -> L.documentRangeFormattingProvider
  SMethod_TextDocumentOnTypeFormatting -> L.documentOnTypeFormattingProvider
  SMethod_TextDocumentRename -> L.renameProvider
  SMethod_TextDocumentPrepareRename -> L.renameProvider
  SMethod_TextDocumentPrepareCallHierarchy -> L.callHierarchyProvider
  SMethod_CallHierarchyIncomingCalls -> L.callHierarchyProvider
  SMethod_CallHierarchyOutgoingCalls -> L.callHierarchyProvider
  SMethod_TextDocumentLinkedEditingRange -> L.linkedEditingRangeProvider
  SMethod_TextDocumentSemanticTokensFull -> L.semanticTokensProvider
  SMethod_TextDocumentSemanticTokensFullDelta -> L.semanticTokensProvider
  SMethod_TextDocumentSemanticTokensRange -> L.semanticTokensProvider
  SMethod_WorkspaceSemanticTokensRefresh -> L.semanticTokensProvider
  SMethod_TextDocumentPrepareTypeHierarchy -> L.typeHierarchyProvider
  SMethod_TypeHierarchySubtypes -> L.typeHierarchyProvider
  SMethod_TypeHierarchySupertypes -> L.typeHierarchyProvider
  SMethod_TextDocumentInlineValue -> L.inlineValueProvider
  SMethod_WorkspaceInlineValueRefresh -> L.inlineValueProvider
  SMethod_TextDocumentInlayHint -> L.inlayHintProvider
  SMethod_InlayHintResolve -> L.inlayHintProvider
  SMethod_WorkspaceInlayHintRefresh -> L.inlayHintProvider
  SMethod_TextDocumentDiagnostic -> L.diagnosticProvider
  SMethod_WorkspaceDiagnostic -> L.diagnosticProvider
  SMethod_WorkspaceDiagnosticRefresh -> L.diagnosticProvider
  SMethod_WorkspaceWorkspaceFolders -> L.workspace . non emptyWorkspaceServerCaps . L.workspaceFolders
  SMethod_WorkspaceWillCreateFiles -> fileOps
  SMethod_WorkspaceWillRenameFiles -> fileOps
  SMethod_WorkspaceWillDeleteFiles -> fileOps
  SMethod_WorkspaceDidCreateFiles -> fileOps
  SMethod_WorkspaceDidRenameFiles -> fileOps
  SMethod_WorkspaceDidDeleteFiles -> fileOps
  SMethod_TextDocumentDidOpen -> documentSync
  SMethod_TextDocumentDidChange -> documentSync
  SMethod_TextDocumentDidClose -> documentSync
  SMethod_TextDocumentDidSave -> documentSync
  SMethod_TextDocumentWillSave -> documentSync
  SMethod_TextDocumentWillSaveWaitUntil -> documentSync
  SMethod_NotebookDocumentDidOpen -> notebookDocumentSync
  SMethod_NotebookDocumentDidChange -> notebookDocumentSync
  SMethod_NotebookDocumentDidSave -> notebookDocumentSync
  SMethod_NotebookDocumentDidClose -> notebookDocumentSync
  SMethod_WorkspaceDidChangeConfiguration -> noCap
  SMethod_WorkspaceDidChangeWatchedFiles -> noCap
  SMethod_WorkspaceDidChangeWorkspaceFolders -> noCap
  SMethod_TextDocumentPublishDiagnostics -> noCap
  SMethod_WorkspaceConfiguration -> noCap
  SMethod_WindowWorkDoneProgressCreate -> noCap
  SMethod_WindowWorkDoneProgressCancel -> noCap
  SMethod_Progress -> noCap
  SMethod_WindowShowDocument -> noCap
  SMethod_WindowShowMessageRequest -> noCap
  SMethod_WindowShowMessage -> noCap
  SMethod_WindowLogMessage -> noCap
  SMethod_ClientRegisterCapability -> noCap
  SMethod_ClientUnregisterCapability -> noCap
  SMethod_Initialize -> noCap
  SMethod_Initialized -> noCap
  SMethod_Shutdown -> noCap
  SMethod_Exit -> noCap
  SMethod_WorkspaceApplyEdit -> noCap
  SMethod_TelemetryEvent -> noCap
  SMethod_SetTrace -> noCap
  SMethod_LogTrace -> noCap
  SMethod_CancelRequest -> noCap
  (SMethod_CustomMethod _s) -> noCap
 where
  fileOps :: Lens' ServerCapabilities (Maybe FileOperationOptions)
  fileOps = L.workspace . non emptyWorkspaceServerCaps . L.fileOperations
  documentSync :: Lens' ServerCapabilities (Maybe DocumentSyncCaps)
  documentSync = L.textDocumentSync
  notebookDocumentSync :: Lens' ServerCapabilities (Maybe NotebookDocumentSyncCaps)
  notebookDocumentSync = L.notebookDocumentSync

noCap :: Lens' a (Maybe Void)
noCap = lens g s
 where
  g _ = Nothing
  s a Nothing = a
  s _ (Just v) = absurd v

-- TODO: this is silly
emptyWorkspaceClientCaps :: WorkspaceClientCapabilities
emptyWorkspaceClientCaps = WorkspaceClientCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
emptyWindowClientCaps :: WindowClientCapabilities
emptyWindowClientCaps = WindowClientCapabilities Nothing Nothing Nothing
emptyTextDocumentClientCaps :: TextDocumentClientCapabilities
emptyTextDocumentClientCaps = TextDocumentClientCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
emptyNotebookDocumentClientCaps :: NotebookDocumentClientCapabilities
emptyNotebookDocumentClientCaps = NotebookDocumentClientCapabilities (NotebookDocumentSyncClientCapabilities Nothing Nothing)

emptyWorkspaceServerCaps :: WorkspaceOptions
emptyWorkspaceServerCaps = WorkspaceOptions Nothing Nothing

{- Note [Capability mappings]
Sadly these are all manually defined as we don't have the information in the metamodel to do them automatically.
-}
