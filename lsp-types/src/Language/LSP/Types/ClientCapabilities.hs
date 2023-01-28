{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Language.LSP.Types.ClientCapabilities where

import           Data.Aeson.TH
import qualified Data.Aeson as A
import Data.Default
import Data.Text (Text)

import Language.LSP.Types.CallHierarchy
import Language.LSP.Types.CodeAction
import Language.LSP.Types.CodeLens
import Language.LSP.Types.Command
import Language.LSP.Types.Completion
import Language.LSP.Types.Configuration
import Language.LSP.Types.Diagnostic
import Language.LSP.Types.Declaration
import Language.LSP.Types.Definition
import Language.LSP.Types.DocumentColor
import Language.LSP.Types.DocumentHighlight
import Language.LSP.Types.DocumentLink
import Language.LSP.Types.DocumentSymbol
import Language.LSP.Types.FoldingRange
import Language.LSP.Types.Formatting
import Language.LSP.Types.Hover
import Language.LSP.Types.Implementation
import Language.LSP.Types.References
import Language.LSP.Types.Rename
import Language.LSP.Types.SelectionRange
import Language.LSP.Types.SemanticTokens
import Language.LSP.Types.SignatureHelp
import Language.LSP.Types.TextDocument
import Language.LSP.Types.TypeDefinition
import Language.LSP.Types.Utils
import Language.LSP.Types.WatchedFiles
import Language.LSP.Types.WorkspaceEdit
import Language.LSP.Types.WorkspaceSymbol
import Language.LSP.Types.MarkupContent (MarkdownClientCapabilities)
import Language.LSP.Types.Common (List)


data WorkspaceClientCapabilities =
  WorkspaceClientCapabilities
    { -- | The client supports applying batch edits to the workspace by supporting
      -- the request 'workspace/applyEdit'
      _applyEdit :: Maybe Bool

      -- | Capabilities specific to `WorkspaceEdit`s
    , _workspaceEdit :: Maybe WorkspaceEditClientCapabilities

      -- | Capabilities specific to the @workspace/didChangeConfiguration@ notification.
    , _didChangeConfiguration :: Maybe DidChangeConfigurationClientCapabilities

       -- | Capabilities specific to the @workspace/didChangeWatchedFiles@ notification.
    , _didChangeWatchedFiles :: Maybe DidChangeWatchedFilesClientCapabilities

      -- | Capabilities specific to the @workspace/symbol@ request.
    , _symbol :: Maybe WorkspaceSymbolClientCapabilities

      -- | Capabilities specific to the @workspace/executeCommand@ request.
    , _executeCommand :: Maybe ExecuteCommandClientCapabilities

      -- | The client has support for workspace folders.
    , _workspaceFolders :: Maybe Bool

      -- | The client supports @workspace/configuration@ requests.
    , _configuration :: Maybe Bool

      -- | Capabilities specific to the semantic token requests scoped to the
      -- workspace.
      --
      -- @since 3.16.0
    , _semanticTokens :: Maybe SemanticTokensWorkspaceClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceClientCapabilities

instance Default WorkspaceClientCapabilities where
  def = WorkspaceClientCapabilities def def def def def def def def def

-- -------------------------------------

data TextDocumentClientCapabilities =
  TextDocumentClientCapabilities
    { _synchronization :: Maybe TextDocumentSyncClientCapabilities

      -- | Capabilities specific to the @textDocument/completion@
    , _completion :: Maybe CompletionClientCapabilities

      -- | Capabilities specific to the @textDocument/hover@
    , _hover :: Maybe HoverClientCapabilities

      -- | Capabilities specific to the @textDocument/signatureHelp@
    , _signatureHelp :: Maybe SignatureHelpClientCapabilities

      -- | Capabilities specific to the @textDocument/references@
    , _references :: Maybe ReferencesClientCapabilities

      -- | Capabilities specific to the @textDocument/documentHighlight@
    , _documentHighlight :: Maybe DocumentHighlightClientCapabilities

      -- | Capabilities specific to the @textDocument/documentSymbol@
    , _documentSymbol :: Maybe DocumentSymbolClientCapabilities

      -- | Capabilities specific to the @textDocument/formatting@
    , _formatting :: Maybe DocumentFormattingClientCapabilities

      -- | Capabilities specific to the @textDocument/rangeFormatting@
    , _rangeFormatting :: Maybe DocumentRangeFormattingClientCapabilities

      -- | Capabilities specific to the @textDocument/onTypeFormatting@
    , _onTypeFormatting :: Maybe DocumentOnTypeFormattingClientCapabilities

      -- | Capabilities specific to the @textDocument/declaration@ request.
      --
      -- Since LSP 3.14.0
    , _declaration :: Maybe DeclarationClientCapabilities

      -- | Capabilities specific to the @textDocument/definition@
    , _definition :: Maybe DefinitionClientCapabilities

      -- | Capabilities specific to the @textDocument/typeDefinition@
    , _typeDefinition :: Maybe TypeDefinitionClientCapabilities

      -- | Capabilities specific to the @textDocument/implementation@
    , _implementation :: Maybe ImplementationClientCapabilities

      -- | Capabilities specific to the @textDocument/codeAction@
    , _codeAction :: Maybe CodeActionClientCapabilities

      -- | Capabilities specific to the @textDocument/codeLens@
    , _codeLens :: Maybe CodeLensClientCapabilities

      -- | Capabilities specific to the @textDocument/documentLink@
    , _documentLink :: Maybe DocumentLinkClientCapabilities

      -- | Capabilities specific to the @textDocument/documentColor@ and the
      -- @textDocument/colorPresentation@ request
    , _colorProvider :: Maybe DocumentColorClientCapabilities

      -- | Capabilities specific to the @textDocument/rename@
    , _rename :: Maybe RenameClientCapabilities

      -- | Capabilities specific to @textDocument/publishDiagnostics@
    , _publishDiagnostics :: Maybe PublishDiagnosticsClientCapabilities

      -- | Capabilities specific to the @textDocument/foldingRange@ request.
      -- Since LSP 3.10.
      --
      -- @since 0.7.0.0
    , _foldingRange :: Maybe FoldingRangeClientCapabilities

      -- | Capabilities specific to the @textDocument/selectionRange@ request.
      -- Since LSP 3.15.0
    , _selectionRange :: Maybe SelectionRangeClientCapabilities

      -- | Call hierarchy specific to the @textDocument/prepareCallHierarchy@ request.
      -- Since LSP 3.16.0
    , _callHierarchy :: Maybe CallHierarchyClientCapabilities

    -- | Capabilities specific to the various semantic token requests.
    --
    -- @since 3.16.0
    , _semanticTokens :: Maybe SemanticTokensClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentClientCapabilities

instance Default TextDocumentClientCapabilities where
  def = TextDocumentClientCapabilities def def def def def def def def
                                       def def def def def def def def
                                       def def def def def def def def

-- ---------------------------------------------------------------------

-- | Capabilities specific to the `MessageActionItem` type.
data MessageActionItemClientCapabilities =
  MessageActionItemClientCapabilities
    {
      -- | Whether the client supports additional attributes which
      -- are preserved and sent back to the server in the
      -- request's response.
      _additionalPropertiesSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''MessageActionItemClientCapabilities

-- | Show message request client capabilities
data ShowMessageRequestClientCapabilities =
  ShowMessageRequestClientCapabilities
    { -- | Capabilities specific to the `MessageActionItem` type.
      _messageActionItem :: Maybe MessageActionItemClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ShowMessageRequestClientCapabilities

-- | Client capabilities for the show document request.
--
-- @since 3.16.0
data ShowDocumentClientCapabilities =
  ShowDocumentClientCapabilities
    { -- | The client has support for the show document request
      _support :: Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ShowDocumentClientCapabilities

-- | Window specific client capabilities.
data WindowClientCapabilities =
  WindowClientCapabilities
    { -- | Whether client supports handling progress notifications.
      --
      -- @since 3.15.0
      _workDoneProgress :: Maybe Bool
      -- | Capabilities specific to the showMessage request
      --
      -- @since 3.16.0
    , _showMessage :: Maybe ShowMessageRequestClientCapabilities
      -- | Capabilities specific to the showDocument request
      --
      -- @since 3.16.0
    , _showDocument :: Maybe ShowDocumentClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WindowClientCapabilities

instance Default WindowClientCapabilities where
  def = WindowClientCapabilities def def def

-- ---------------------------------------------------------------------

-- | Client capability that signals how the client
-- handles stale requests (e.g. a request
-- for which the client will not process the response
-- anymore since the information is outdated).
-- @since 3.17.0
data StaleRequestClientCapabilities =
  StaleRequestClientCapabilities
    { _cancel :: Bool
    , _retryOnContentModified :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''StaleRequestClientCapabilities

-- | Client capabilities specific to regular expressions.
-- @since 3.16.0
data RegularExpressionsClientCapabilities =
  RegularExpressionsClientCapabilities
    { _engine :: Text
    , _version :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RegularExpressionsClientCapabilities

-- | General client capabilities.
-- @since 3.16.0
data GeneralClientCapabilities =
  GeneralClientCapabilities
    {
      _staleRequestSupport :: Maybe StaleRequestClientCapabilities
      -- | Client capabilities specific to regular expressions.
      -- @since 3.16.0
    , _regularExpressions :: Maybe RegularExpressionsClientCapabilities
      -- | Client capabilities specific to the client's markdown parser.
      -- @since 3.16.0
    , _markdown :: Maybe MarkdownClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''GeneralClientCapabilities

instance Default GeneralClientCapabilities where
  def = GeneralClientCapabilities def def def

-- ---------------------------------------------------------------------

data ClientCapabilities =
  ClientCapabilities
    { -- | Workspace specific client capabilities
      _workspace    :: Maybe WorkspaceClientCapabilities
      -- | Text document specific client capabilities
    , _textDocument :: Maybe TextDocumentClientCapabilities
      -- | Window specific client capabilities.
    , _window :: Maybe WindowClientCapabilities
      -- | General client capabilities.
      -- @since 3.16.0
    , _general :: Maybe GeneralClientCapabilities
      -- | Experimental client capabilities.
    , _experimental :: Maybe A.Object
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ClientCapabilities

instance Default ClientCapabilities where
  def = ClientCapabilities def def def def def
