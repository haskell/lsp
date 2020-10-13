{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Language.LSP.Types.ClientCapabilities where

import           Data.Aeson.TH
import qualified Data.Aeson as A
import Data.Default
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
import Language.LSP.Types.SignatureHelp
import Language.LSP.Types.TextDocument
import Language.LSP.Types.TypeDefinition
import Language.LSP.Types.Utils
import Language.LSP.Types.WatchedFiles
import Language.LSP.Types.WorkspaceEdit
import Language.LSP.Types.WorkspaceSymbol


data WorkspaceClientCapabilities =
  WorkspaceClientCapabilities
    { -- | The client supports applying batch edits to the workspace by supporting
      -- the request 'workspace/applyEdit'
      _applyEdit :: Maybe Bool

      -- | Capabilities specific to `WorkspaceEdit`s
    , _workspaceEdit :: Maybe WorkspaceEditClientCapabilities

      -- | Capabilities specific to the `workspace/didChangeConfiguration` notification.
    , _didChangeConfiguration :: Maybe DidChangeConfigurationClientCapabilities

       -- | Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
    , _didChangeWatchedFiles :: Maybe DidChangeWatchedFilesClientCapabilities

      -- | Capabilities specific to the `workspace/symbol` request.
    , _symbol :: Maybe WorkspaceSymbolClientCapabilities

      -- | Capabilities specific to the `workspace/executeCommand` request.
    , _executeCommand :: Maybe ExecuteCommandClientCapabilities

      -- | The client has support for workspace folders.
    , _workspaceFolders :: Maybe Bool

      -- | The client supports `workspace/configuration` requests.
    , _configuration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceClientCapabilities

instance Default WorkspaceClientCapabilities where
  def = WorkspaceClientCapabilities def def def def def def def def

-- -------------------------------------

data TextDocumentClientCapabilities =
  TextDocumentClientCapabilities
    { _synchronization :: Maybe TextDocumentSyncClientCapabilities

      -- | Capabilities specific to the `textDocument/completion`
    , _completion :: Maybe CompletionClientCapabilities

      -- | Capabilities specific to the `textDocument/hover`
    , _hover :: Maybe HoverClientCapabilities

      -- | Capabilities specific to the `textDocument/signatureHelp`
    , _signatureHelp :: Maybe SignatureHelpClientCapabilities

      -- | Capabilities specific to the `textDocument/references`
    , _references :: Maybe ReferencesClientCapabilities

      -- | Capabilities specific to the `textDocument/documentHighlight`
    , _documentHighlight :: Maybe DocumentHighlightClientCapabilities

      -- | Capabilities specific to the `textDocument/documentSymbol`
    , _documentSymbol :: Maybe DocumentSymbolClientCapabilities

      -- | Capabilities specific to the `textDocument/formatting`
    , _formatting :: Maybe DocumentFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/rangeFormatting`
    , _rangeFormatting :: Maybe DocumentRangeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/onTypeFormatting`
    , _onTypeFormatting :: Maybe DocumentOnTypeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/declaration` request.
      -- 
      -- Since LSP 3.14.0
    , _declaration :: Maybe DeclarationClientCapabilities

      -- | Capabilities specific to the `textDocument/definition`
    , _definition :: Maybe DefinitionClientCapabilities

      -- | Capabilities specific to the `textDocument/typeDefinition`
    , _typeDefinition :: Maybe TypeDefinitionClientCapabilities

      -- | Capabilities specific to the `textDocument/implementation`
    , _implementation :: Maybe ImplementationClientCapabilities

      -- | Capabilities specific to the `textDocument/codeAction`
    , _codeAction :: Maybe CodeActionClientCapabilities

      -- | Capabilities specific to the `textDocument/codeLens`
    , _codeLens :: Maybe CodeLensClientCapabilities

      -- | Capabilities specific to the `textDocument/documentLink`
    , _documentLink :: Maybe DocumentLinkClientCapabilities

      -- | Capabilities specific to the `textDocument/documentColor` and the
      -- `textDocument/colorPresentation` request
    , _colorProvider :: Maybe DocumentColorClientCapabilities

      -- | Capabilities specific to the `textDocument/rename`
    , _rename :: Maybe RenameClientCapabilities

      -- | Capabilities specific to `textDocument/publishDiagnostics`
    , _publishDiagnostics :: Maybe PublishDiagnosticsClientCapabilities

      -- | Capabilities specific to the `textDocument/foldingRange` request.
      -- Since LSP 3.10.
      --
      -- @since 0.7.0.0
    , _foldingRange :: Maybe FoldingRangeClientCapabilities

      -- | Capabilities specific to the `textDocument/selectionRange` request.
      -- Since LSP 3.15.0
    , _selectionRange :: Maybe SelectionRangeClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentClientCapabilities

instance Default TextDocumentClientCapabilities where
  def = TextDocumentClientCapabilities def def def def def def def def
                                       def def def def def def def def
                                       def def def def def def

-- ---------------------------------------------------------------------

-- | Window specific client capabilities.
data WindowClientCapabilities =
  WindowClientCapabilities
    { -- | Whether client supports handling progress notifications.
      _workDoneProgress :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WindowClientCapabilities

instance Default WindowClientCapabilities where
  def = WindowClientCapabilities def

data ClientCapabilities =
  ClientCapabilities
    { _workspace    :: Maybe WorkspaceClientCapabilities
    , _textDocument :: Maybe TextDocumentClientCapabilities
    -- | Capabilities specific to `window/progress` requests. Experimental.
    --
    -- @since 0.10.0.0
    , _window :: Maybe WindowClientCapabilities
    , _experimental :: Maybe A.Object
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ClientCapabilities

instance Default ClientCapabilities where
  def = ClientCapabilities def def def def
