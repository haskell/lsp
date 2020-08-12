{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.LSP.Types.ServerCapabilities where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Language.Haskell.LSP.Types.CodeAction
import Language.Haskell.LSP.Types.CodeLens
import Language.Haskell.LSP.Types.Color
import Language.Haskell.LSP.Types.Command
import Language.Haskell.LSP.Types.Common
import Language.Haskell.LSP.Types.Completion
import Language.Haskell.LSP.Types.Declaration
import Language.Haskell.LSP.Types.Definition
import Language.Haskell.LSP.Types.DocumentHighlight
import Language.Haskell.LSP.Types.FoldingRange
import Language.Haskell.LSP.Types.Formatting
import Language.Haskell.LSP.Types.Hover
import Language.Haskell.LSP.Types.Implementation
import Language.Haskell.LSP.Types.References
import Language.Haskell.LSP.Types.Rename
import Language.Haskell.LSP.Types.SignatureHelp
import Language.Haskell.LSP.Types.Symbol
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.TypeDefinition
import Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------
{-
The server can signal the following capabilities:

/**
 * Defines how the host (editor) should sync document changes to the language server.
 */
enum TextDocumentSyncKind {
    /**
     * Documents should not be synced at all.
     */
    None = 0,
    /**
     * Documents are synced by always sending the full content of the document.
     */
    Full = 1,
    /**
     * Documents are synced by sending the full content on open. After that only incremental
     * updates to the document are sent.
     */
    Incremental = 2
}
-}

-- ^ Note: Omitting this parameter from the capabilities is effectively a fourth
-- state, where DidSave events are generated without sending document contents.
data TextDocumentSyncKind = TdSyncNone
                          | TdSyncFull
                          | TdSyncIncremental
       deriving (Read,Eq,Show)

instance ToJSON TextDocumentSyncKind where
  toJSON TdSyncNone        = Number 0
  toJSON TdSyncFull        = Number 1
  toJSON TdSyncIncremental = Number 2

instance FromJSON TextDocumentSyncKind where
  parseJSON (Number 0) = pure TdSyncNone
  parseJSON (Number 1) = pure TdSyncFull
  parseJSON (Number 2) = pure TdSyncIncremental
  parseJSON _            = mempty


-- ---------------------------------------------------------------------
{-
New in 3.0
----------
/**
 * Save options.
 */
export interface SaveOptions {
        /**
         * The client is supposed to include the content on save.
         */
        includeText?: boolean;
}
-}
data SaveOptions =
  SaveOptions
    { -- | The client is supposed to include the content on save.
      _includeText :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''SaveOptions

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

export interface TextDocumentSyncOptions {
        /**
         * Open and close notifications are sent to the server.
         */
        openClose?: boolean;
        /**
         * Change notificatins are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
         * and TextDocumentSyncKindIncremental.
         */
        change?: number;
        /**
         * Will save notifications are sent to the server.
         */
        willSave?: boolean;
        /**
         * Will save wait until requests are sent to the server.
         */
        willSaveWaitUntil?: boolean;
        /**
         * Save notifications are sent to the server.
         */
        save?: SaveOptions;
}
-}

data TextDocumentSyncOptions =
  TextDocumentSyncOptions
    { -- | Open and close notifications are sent to the server.
      _openClose         :: Maybe Bool

      -- | Change notificatins are sent to the server. See
      -- TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
      -- TextDocumentSyncKindIncremental.
    , _change            :: Maybe TextDocumentSyncKind

      -- | Will save notifications are sent to the server.
    , _willSave          :: Maybe Bool

      -- | Will save wait until requests are sent to the server.
    , _willSaveWaitUntil :: Maybe Bool

      -- | Save notifications are sent to the server.
    , _save              :: Maybe SaveOptions
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentSyncOptions

-- ---------------------------------------------------------------------
{-

Extended in 3.0
---------------

interface ServerCapabilities {
        /**
         * Defines how text documents are synced. Is either a detailed structure defining each notification or
         * for backwards compatibility the TextDocumentSyncKind number. If omitted it defaults to `TextDocumentSyncKind.None`.
         */
        textDocumentSync?: TextDocumentSyncOptions | number;
        /**
         * The server provides hover support.
         */
        hoverProvider?: boolean;
        /**
         * The server provides completion support.
         */
        completionProvider?: CompletionOptions;
        /**
         * The server provides signature help support.
         */
        signatureHelpProvider?: SignatureHelpOptions;
        /**
         * The server provides goto definition support.
         */
        definitionProvider?: boolean;
        /**
         * The server provides Goto Type Definition support.
         *
         * Since 3.6.0
         */
        typeDefinitionProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
        /**
         * The server provides Goto Implementation support.
         *
         * Since 3.6.0
         */
        implementationProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
        /**
         * The server provides find references support.
         */
        referencesProvider?: boolean;
        /**
         * The server provides document highlight support.
         */
        documentHighlightProvider?: boolean;
        /**
         * The server provides document symbol support.
         */
        documentSymbolProvider?: boolean;
        /**
         * The server provides workspace symbol support.
         */
        workspaceSymbolProvider?: boolean;
        /**
         * The server provides code actions. The `CodeActionOptions` return type is only
         * valid if the client signals code action literal support via the property
         * `textDocument.codeAction.codeActionLiteralSupport`.
         */
        codeActionProvider?: boolean | CodeActionOptions;
        /**
         * The server provides code lens.
         */
        codeLensProvider?: CodeLensOptions;
        /**
         * The server provides document formatting.
         */
        documentFormattingProvider?: boolean;
        /**
         * The server provides document range formatting.
         */
        documentRangeFormattingProvider?: boolean;
        /**
         * The server provides document formatting on typing.
         */
        documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
        /**
         * The server provides rename support.
         */
        renameProvider?: boolean;
        /**
         * The server provides document link support.
         */
        documentLinkProvider?: DocumentLinkOptions;
        /**
         * The server provides color provider support.
         *
         * Since 3.6.0
         */
        colorProvider?: boolean | ColorProviderOptions | (ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions);
        /**
         * The server provides folding provider support.
         *
         * Since 3.10.0
         */
        foldingRangeProvider?: boolean | FoldingRangeProviderOptions | (FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions);
        /**
         * The server provides execute command support.
         */
        executeCommandProvider?: ExecuteCommandOptions;
        /**
         * Workspace specific server capabilities
         */
        workspace?: {
                /**
                 * The server supports workspace folder.
                 *
                 * Since 3.6.0
                 */
                workspaceFolders?: {
                        /**
                        * The server has support for workspace folders
                        */
                        supported?: boolean;
                        /**
                        * Whether the server wants to receive workspace folder
                        * change notifications.
                        *
                        * If a strings is provided the string is treated as a ID
                        * under which the notification is registered on the client
                        * side. The ID can be used to unregister for these events
                        * using the `client/unregisterCapability` request.
                        */
                        changeNotifications?: string | boolean;
                }
        }
        /**
         * Experimental server capabilities.
         */
        experimental?: any;
}
-}

-- | Wrapper for TextDocumentSyncKind fallback.
data TDS = TDSOptions TextDocumentSyncOptions
         | TDSKind TextDocumentSyncKind
    deriving (Show, Read, Eq)

instance FromJSON TDS where
    parseJSON x = TDSOptions <$> parseJSON x <|> TDSKind <$> parseJSON x

instance ToJSON TDS where
    toJSON (TDSOptions x) = toJSON x
    toJSON (TDSKind x) = toJSON x

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

/**
 * Document link options
 */
export interface DocumentLinkOptions {
        /**
         * Document links have a resolve provider as well.
         */
        resolveProvider?: boolean;
}
-}

data DocumentLinkOptions =
  DocumentLinkOptions
    { -- | Document links have a resolve provider as well.
      _resolveProvider :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentLinkOptions

data DocumentLinkRegistrationOptions =
  DocumentLinkRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _documentLinkOptions             :: DocumentLinkOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentLinkRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_documentLinkOptions"
  ]

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
      _textDocumentSync                 :: Maybe TDS
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
    , _referencesProvider               :: Maybe (Bool |? ReferenceOptions) -- For some reason the spec doesn't include ReferenceRegistrationOptions here
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
      -- | The server provides workspace symbol support.
    , _workspaceSymbolProvider          :: Maybe Bool
      -- | Workspace specific server capabilities
    , _workspace                        :: Maybe WorkspaceServerCapabilities
      -- | Experimental server capabilities.
    , _experimental                     :: Maybe Value
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ServerCapabilities
