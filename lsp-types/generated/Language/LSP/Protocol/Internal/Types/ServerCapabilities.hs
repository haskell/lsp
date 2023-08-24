-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ServerCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOptions
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeActionOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeLensOptions
import qualified Language.LSP.Protocol.Internal.Types.CompletionOptions
import qualified Language.LSP.Protocol.Internal.Types.DeclarationOptions
import qualified Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DefinitionOptions
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticOptions
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolOptions
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandOptions
import qualified Language.LSP.Protocol.Internal.Types.FileOperationOptions
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.HoverOptions
import qualified Language.LSP.Protocol.Internal.Types.ImplementationOptions
import qualified Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InlayHintOptions
import qualified Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InlineValueOptions
import qualified Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.MonikerOptions
import qualified Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.PositionEncodingKind
import qualified Language.LSP.Protocol.Internal.Types.ReferenceOptions
import qualified Language.LSP.Protocol.Internal.Types.RenameOptions
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Defines the capabilities provided by a language
server.
-}
data ServerCapabilities = ServerCapabilities 
  { {-|
  The position encoding the server picked from the encodings offered
  by the client via the client capability `general.positionEncodings`.

  If the client didn't provide any position encodings the only valid
  value that a server can return is 'utf-16'.

  If omitted it defaults to 'utf-16'.

  @since 3.17.0
  -}
  _positionEncoding :: (Maybe Language.LSP.Protocol.Internal.Types.PositionEncodingKind.PositionEncodingKind)
  , {-|
  Defines how text documents are synced. Is either a detailed structure
  defining each notification or for backwards compatibility the
  TextDocumentSyncKind number.
  -}
  _textDocumentSync :: (Maybe (Language.LSP.Protocol.Internal.Types.TextDocumentSyncOptions.TextDocumentSyncOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind.TextDocumentSyncKind))
  , {-|
  Defines how notebook documents are synced.

  @since 3.17.0
  -}
  _notebookDocumentSync :: (Maybe (Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions.NotebookDocumentSyncOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions))
  , {-|
  The server provides completion support.
  -}
  _completionProvider :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionOptions.CompletionOptions)
  , {-|
  The server provides hover support.
  -}
  _hoverProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.HoverOptions.HoverOptions))
  , {-|
  The server provides signature help support.
  -}
  _signatureHelpProvider :: (Maybe Language.LSP.Protocol.Internal.Types.SignatureHelpOptions.SignatureHelpOptions)
  , {-|
  The server provides Goto Declaration support.
  -}
  _declarationProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.DeclarationOptions.DeclarationOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions.DeclarationRegistrationOptions)))
  , {-|
  The server provides goto definition support.
  -}
  _definitionProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DefinitionOptions.DefinitionOptions))
  , {-|
  The server provides Goto Type Definition support.
  -}
  _typeDefinitionProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.TypeDefinitionOptions.TypeDefinitionOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions.TypeDefinitionRegistrationOptions)))
  , {-|
  The server provides Goto Implementation support.
  -}
  _implementationProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.ImplementationOptions.ImplementationOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions.ImplementationRegistrationOptions)))
  , {-|
  The server provides find references support.
  -}
  _referencesProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.ReferenceOptions.ReferenceOptions))
  , {-|
  The server provides document highlight support.
  -}
  _documentHighlightProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentHighlightOptions.DocumentHighlightOptions))
  , {-|
  The server provides document symbol support.
  -}
  _documentSymbolProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentSymbolOptions.DocumentSymbolOptions))
  , {-|
  The server provides code actions. CodeActionOptions may only be
  specified if the client states that it supports
  `codeActionLiteralSupport` in its initial `initialize` request.
  -}
  _codeActionProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.CodeActionOptions.CodeActionOptions))
  , {-|
  The server provides code lens.
  -}
  _codeLensProvider :: (Maybe Language.LSP.Protocol.Internal.Types.CodeLensOptions.CodeLensOptions)
  , {-|
  The server provides document link support.
  -}
  _documentLinkProvider :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentLinkOptions.DocumentLinkOptions)
  , {-|
  The server provides color provider support.
  -}
  _colorProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.DocumentColorOptions.DocumentColorOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions.DocumentColorRegistrationOptions)))
  , {-|
  The server provides workspace symbol support.
  -}
  _workspaceSymbolProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.WorkspaceSymbolOptions.WorkspaceSymbolOptions))
  , {-|
  The server provides document formatting.
  -}
  _documentFormattingProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentFormattingOptions.DocumentFormattingOptions))
  , {-|
  The server provides document range formatting.
  -}
  _documentRangeFormattingProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingOptions.DocumentRangeFormattingOptions))
  , {-|
  The server provides document formatting on typing.
  -}
  _documentOnTypeFormattingProvider :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingOptions.DocumentOnTypeFormattingOptions)
  , {-|
  The server provides rename support. RenameOptions may only be
  specified if the client states that it supports
  `prepareSupport` in its initial `initialize` request.
  -}
  _renameProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.RenameOptions.RenameOptions))
  , {-|
  The server provides folding provider support.
  -}
  _foldingRangeProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.FoldingRangeOptions.FoldingRangeOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions.FoldingRangeRegistrationOptions)))
  , {-|
  The server provides selection range support.
  -}
  _selectionRangeProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.SelectionRangeOptions.SelectionRangeOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions.SelectionRangeRegistrationOptions)))
  , {-|
  The server provides execute command support.
  -}
  _executeCommandProvider :: (Maybe Language.LSP.Protocol.Internal.Types.ExecuteCommandOptions.ExecuteCommandOptions)
  , {-|
  The server provides call hierarchy support.

  @since 3.16.0
  -}
  _callHierarchyProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.CallHierarchyOptions.CallHierarchyOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions.CallHierarchyRegistrationOptions)))
  , {-|
  The server provides linked editing range support.

  @since 3.16.0
  -}
  _linkedEditingRangeProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.LinkedEditingRangeOptions.LinkedEditingRangeOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions.LinkedEditingRangeRegistrationOptions)))
  , {-|
  The server provides semantic tokens support.

  @since 3.16.0
  -}
  _semanticTokensProvider :: (Maybe (Language.LSP.Protocol.Internal.Types.SemanticTokensOptions.SemanticTokensOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions.SemanticTokensRegistrationOptions))
  , {-|
  The server provides moniker support.

  @since 3.16.0
  -}
  _monikerProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.MonikerOptions.MonikerOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions.MonikerRegistrationOptions)))
  , {-|
  The server provides type hierarchy support.

  @since 3.17.0
  -}
  _typeHierarchyProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions.TypeHierarchyOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions.TypeHierarchyRegistrationOptions)))
  , {-|
  The server provides inline values.

  @since 3.17.0
  -}
  _inlineValueProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.InlineValueOptions.InlineValueOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions.InlineValueRegistrationOptions)))
  , {-|
  The server provides inlay hints.

  @since 3.17.0
  -}
  _inlayHintProvider :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.InlayHintOptions.InlayHintOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions.InlayHintRegistrationOptions)))
  , {-|
  The server has support for pull model diagnostics.

  @since 3.17.0
  -}
  _diagnosticProvider :: (Maybe (Language.LSP.Protocol.Internal.Types.DiagnosticOptions.DiagnosticOptions Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions.DiagnosticRegistrationOptions))
  , {-|
  Workspace specific server capabilities.
  -}
  _workspace :: (Maybe (Row.Rec ("workspaceFolders" Row..== (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities.WorkspaceFoldersServerCapabilities) Row..+ ("fileOperations" Row..== (Maybe Language.LSP.Protocol.Internal.Types.FileOperationOptions.FileOperationOptions) Row..+ Row.Empty))))
  , {-|
  Experimental server capabilities.
  -}
  _experimental :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ServerCapabilities)

instance Aeson.ToJSON ServerCapabilities where
  toJSON (ServerCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29 arg30 arg31 arg32 arg33 arg34) = Aeson.object $ concat $  ["positionEncoding" Language.LSP.Protocol.Types.Common..=? arg0
    ,"textDocumentSync" Language.LSP.Protocol.Types.Common..=? arg1
    ,"notebookDocumentSync" Language.LSP.Protocol.Types.Common..=? arg2
    ,"completionProvider" Language.LSP.Protocol.Types.Common..=? arg3
    ,"hoverProvider" Language.LSP.Protocol.Types.Common..=? arg4
    ,"signatureHelpProvider" Language.LSP.Protocol.Types.Common..=? arg5
    ,"declarationProvider" Language.LSP.Protocol.Types.Common..=? arg6
    ,"definitionProvider" Language.LSP.Protocol.Types.Common..=? arg7
    ,"typeDefinitionProvider" Language.LSP.Protocol.Types.Common..=? arg8
    ,"implementationProvider" Language.LSP.Protocol.Types.Common..=? arg9
    ,"referencesProvider" Language.LSP.Protocol.Types.Common..=? arg10
    ,"documentHighlightProvider" Language.LSP.Protocol.Types.Common..=? arg11
    ,"documentSymbolProvider" Language.LSP.Protocol.Types.Common..=? arg12
    ,"codeActionProvider" Language.LSP.Protocol.Types.Common..=? arg13
    ,"codeLensProvider" Language.LSP.Protocol.Types.Common..=? arg14
    ,"documentLinkProvider" Language.LSP.Protocol.Types.Common..=? arg15
    ,"colorProvider" Language.LSP.Protocol.Types.Common..=? arg16
    ,"workspaceSymbolProvider" Language.LSP.Protocol.Types.Common..=? arg17
    ,"documentFormattingProvider" Language.LSP.Protocol.Types.Common..=? arg18
    ,"documentRangeFormattingProvider" Language.LSP.Protocol.Types.Common..=? arg19
    ,"documentOnTypeFormattingProvider" Language.LSP.Protocol.Types.Common..=? arg20
    ,"renameProvider" Language.LSP.Protocol.Types.Common..=? arg21
    ,"foldingRangeProvider" Language.LSP.Protocol.Types.Common..=? arg22
    ,"selectionRangeProvider" Language.LSP.Protocol.Types.Common..=? arg23
    ,"executeCommandProvider" Language.LSP.Protocol.Types.Common..=? arg24
    ,"callHierarchyProvider" Language.LSP.Protocol.Types.Common..=? arg25
    ,"linkedEditingRangeProvider" Language.LSP.Protocol.Types.Common..=? arg26
    ,"semanticTokensProvider" Language.LSP.Protocol.Types.Common..=? arg27
    ,"monikerProvider" Language.LSP.Protocol.Types.Common..=? arg28
    ,"typeHierarchyProvider" Language.LSP.Protocol.Types.Common..=? arg29
    ,"inlineValueProvider" Language.LSP.Protocol.Types.Common..=? arg30
    ,"inlayHintProvider" Language.LSP.Protocol.Types.Common..=? arg31
    ,"diagnosticProvider" Language.LSP.Protocol.Types.Common..=? arg32
    ,"workspace" Language.LSP.Protocol.Types.Common..=? arg33
    ,"experimental" Language.LSP.Protocol.Types.Common..=? arg34]

instance Aeson.FromJSON ServerCapabilities where
  parseJSON = Aeson.withObject "ServerCapabilities" $ \arg -> ServerCapabilities <$> arg Aeson..:! "positionEncoding" <*> arg Aeson..:! "textDocumentSync" <*> arg Aeson..:! "notebookDocumentSync" <*> arg Aeson..:! "completionProvider" <*> arg Aeson..:! "hoverProvider" <*> arg Aeson..:! "signatureHelpProvider" <*> arg Aeson..:! "declarationProvider" <*> arg Aeson..:! "definitionProvider" <*> arg Aeson..:! "typeDefinitionProvider" <*> arg Aeson..:! "implementationProvider" <*> arg Aeson..:! "referencesProvider" <*> arg Aeson..:! "documentHighlightProvider" <*> arg Aeson..:! "documentSymbolProvider" <*> arg Aeson..:! "codeActionProvider" <*> arg Aeson..:! "codeLensProvider" <*> arg Aeson..:! "documentLinkProvider" <*> arg Aeson..:! "colorProvider" <*> arg Aeson..:! "workspaceSymbolProvider" <*> arg Aeson..:! "documentFormattingProvider" <*> arg Aeson..:! "documentRangeFormattingProvider" <*> arg Aeson..:! "documentOnTypeFormattingProvider" <*> arg Aeson..:! "renameProvider" <*> arg Aeson..:! "foldingRangeProvider" <*> arg Aeson..:! "selectionRangeProvider" <*> arg Aeson..:! "executeCommandProvider" <*> arg Aeson..:! "callHierarchyProvider" <*> arg Aeson..:! "linkedEditingRangeProvider" <*> arg Aeson..:! "semanticTokensProvider" <*> arg Aeson..:! "monikerProvider" <*> arg Aeson..:! "typeHierarchyProvider" <*> arg Aeson..:! "inlineValueProvider" <*> arg Aeson..:! "inlayHintProvider" <*> arg Aeson..:! "diagnosticProvider" <*> arg Aeson..:! "workspace" <*> arg Aeson..:! "experimental"
