{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.ServerCapabilities where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Language.LSP.Types.CallHierarchy
import Language.LSP.Types.CodeAction
import Language.LSP.Types.CodeLens
import Language.LSP.Types.Command
import Language.LSP.Types.Common
import Language.LSP.Types.Completion
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

-- ---------------------------------------------------------------------

data WorkspaceFoldersServerCapabilities =
  WorkspaceFoldersServerCapabilities
    { -- | The server has support for workspace folders
      _supported :: Maybe Bool
      -- | Whether the server wants to receive workspace folder
      -- change notifications.
      -- If a strings is provided the string is treated as a ID
      -- under which the notification is registered on the client
      -- side. The ID can be used to unregister for these events
      -- using the `client/unregisterCapability` request.
    , _changeNotifications :: Maybe (Text |? Bool)
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceFoldersServerCapabilities

data WorkspaceServerCapabilities =
  WorkspaceServerCapabilities
    { -- | The server supports workspace folder. Since LSP 3.6
      --
      -- @since 0.7.0.0
      _workspaceFolders :: Maybe WorkspaceFoldersServerCapabilities
    }
  deriving (Show, Read, Eq)
deriveJSON lspOptions ''WorkspaceServerCapabilities

data ServerCapabilities =
  ServerCapabilities
    { -- | Defines how text documents are synced. Is either a detailed structure
      -- defining each notification or for backwards compatibility the
      -- 'TextDocumentSyncKind' number.
      -- If omitted it defaults to 'TdSyncNone'.
      _textDocumentSync                 :: Maybe (TextDocumentSyncOptions |? TextDocumentSyncKind)
      -- | The server provides hover support.
    , _hoverProvider                    :: Maybe (Bool |? HoverOptions)
      -- | The server provides completion support.
    , _completionProvider               :: Maybe CompletionOptions
      -- | The server provides signature help support.
    , _signatureHelpProvider            :: Maybe SignatureHelpOptions
      -- | The server provides go to declaration support.
      --
      -- Since LSP 3.14.0
    , _declarationProvider              :: Maybe (Bool |? DeclarationOptions |? DeclarationRegistrationOptions)
      -- | The server provides goto definition support.
    , _definitionProvider               :: Maybe (Bool |? DefinitionOptions)
      -- | The server provides Goto Type Definition support. Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _typeDefinitionProvider           :: Maybe (Bool |? TypeDefinitionOptions |? TypeDefinitionRegistrationOptions)
      -- | The server provides Goto Implementation support. Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _implementationProvider           :: Maybe (Bool |? ImplementationOptions |? ImplementationRegistrationOptions)
      -- | The server provides find references support.
    , _referencesProvider               :: Maybe (Bool |? ReferenceOptions)
      -- | The server provides document highlight support.
    , _documentHighlightProvider        :: Maybe (Bool |? DocumentHighlightOptions)
      -- | The server provides document symbol support.
    , _documentSymbolProvider           :: Maybe (Bool |? DocumentSymbolOptions)
      -- | The server provides code actions.
    , _codeActionProvider               :: Maybe (Bool |? CodeActionOptions)
      -- | The server provides code lens.
    , _codeLensProvider                 :: Maybe CodeLensOptions
      -- | The server provides document link support.
    , _documentLinkProvider             :: Maybe DocumentLinkOptions
      -- | The server provides color provider support. Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _colorProvider                    :: Maybe (Bool |? DocumentColorOptions |? DocumentColorRegistrationOptions)
      -- | The server provides document formatting.
    , _documentFormattingProvider       :: Maybe (Bool |? DocumentFormattingOptions)
      -- | The server provides document range formatting.
    , _documentRangeFormattingProvider  :: Maybe (Bool |? DocumentRangeFormattingOptions)
      -- | The server provides document formatting on typing.
    , _documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
      -- | The server provides rename support.
    , _renameProvider                   :: Maybe (Bool |? RenameOptions)
      -- | The server provides folding provider support. Since LSP 3.10
      --
      -- @since 0.7.0.0
    , _foldingRangeProvider             :: Maybe (Bool |? FoldingRangeOptions |? FoldingRangeRegistrationOptions)
      -- | The server provides execute command support.
    , _executeCommandProvider           :: Maybe ExecuteCommandOptions
      -- | The server provides selection range support. Since LSP 3.15
    , _selectionRangeProvider           :: Maybe (Bool |? SelectionRangeOptions |? SelectionRangeRegistrationOptions)
      -- | The server provides call hierarchy support.
    , _callHierarchyProvider            :: Maybe (Bool |? CallHierarchyOptions |? CallHierarchyRegistrationOptions)
      -- | The server provides semantic tokens support.
      --
      -- @since 3.16.0
    , _semanticTokensProvider           :: Maybe (SemanticTokensOptions |? SemanticTokensRegistrationOptions)
      -- | The server provides workspace symbol support.
    , _workspaceSymbolProvider          :: Maybe Bool
      -- | Workspace specific server capabilities
    , _workspace                        :: Maybe WorkspaceServerCapabilities
      -- | Experimental server capabilities.
    , _experimental                     :: Maybe Value
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ServerCapabilities
