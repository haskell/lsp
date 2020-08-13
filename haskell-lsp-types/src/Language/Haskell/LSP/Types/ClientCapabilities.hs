{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Language.Haskell.LSP.Types.ClientCapabilities where

import           Data.Aeson.TH
import qualified Data.Aeson as A
import Data.Default
import Language.Haskell.LSP.Types.CodeAction
import Language.Haskell.LSP.Types.Completion
import Language.Haskell.LSP.Types.Diagnostic
import Language.Haskell.LSP.Types.Declaration
import Language.Haskell.LSP.Types.Definition
import Language.Haskell.LSP.Types.DocumentHighlight
import Language.Haskell.LSP.Types.DocumentSymbol
import Language.Haskell.LSP.Types.Common
import Language.Haskell.LSP.Types.Hover
import Language.Haskell.LSP.Types.Implementation
import Language.Haskell.LSP.Types.SignatureHelp
import Language.Haskell.LSP.Types.References
import Language.Haskell.LSP.Types.TypeDefinition
import Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

/**
 * Workspace specific client capabilities.
 */
export interface WorkspaceClientCapabilities {
        /**
         * The client supports applying batch edits to the workspace by supporting
         * the request 'workspace/applyEdit'
         */
        applyEdit?: boolean;

        /**
         * Capabilities specific to `WorkspaceEdit`s
         */
        workspaceEdit?: {
                /**
                 * The client supports versioned document changes in `WorkspaceEdit`s
                 */
                documentChanges?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeConfiguration` notification.
         */
        didChangeConfiguration?: {
                /**
                 * Did change configuration notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
         */
        didChangeWatchedFiles?: {
                /**
                 * Did change watched files notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/symbol` request.
         */
        symbol?: {
                /**
                 * Symbol request supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
                 */
                symbolKind?: {
                        /**
                         * The symbol kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the symbol kinds from `File` to `Array` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: SymbolKind[];
                }
        };

        /**
         * Capabilities specific to the `workspace/executeCommand` request.
         */
        executeCommand?: {
                /**
                 * Execute command supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * The client has support for workspace folders.
         *
         * Since 3.6.0
         */
        workspaceFolders?: boolean;

        /**
         * The client supports `workspace/configuration` requests.
         *
         * Since 3.6.0
         */
        configuration?: boolean;
}
-}

-- -------------------------------------

data WorkspaceEditClientCapabilities =
  WorkspaceEditClientCapabilities
  { _documentChanges :: Maybe Bool -- ^The client supports versioned document
                                   -- changes in `WorkspaceEdit`s
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceEditClientCapabilities

-- -------------------------------------

data DidChangeConfigurationClientCapabilities =
  DidChangeConfigurationClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Did change configuration
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeConfigurationClientCapabilities

-- -------------------------------------

data DidChangeWatchedFilesClientCapabilities =
  DidChangeWatchedFilesClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Did change watched files
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeWatchedFilesClientCapabilities

-- -------------------------------------

data WorkspaceSymbolKindClientCapabilities =
  WorkspaceSymbolKindClientCapabilities
   { -- | The symbol kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
     --
     -- If this property is not present the client only supports
     -- the symbol kinds from `File` to `Array` as defined in
     -- the initial version of the protocol.
     _valueSet :: Maybe (List SymbolKind)
   } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceSymbolKindClientCapabilities

instance Default WorkspaceSymbolKindClientCapabilities where
  def = WorkspaceSymbolKindClientCapabilities (Just $ List allKinds)
    where allKinds = [ SkFile
                     , SkModule
                     , SkNamespace
                     , SkPackage
                     , SkClass
                     , SkMethod
                     , SkProperty
                     , SkField
                     , SkConstructor
                     , SkEnum
                     , SkInterface
                     , SkFunction
                     , SkVariable
                     , SkConstant
                     , SkString
                     , SkNumber
                     , SkBoolean
                     , SkArray
                     ]

data WorkspaceSymbolClientCapabilities =
  WorkspaceSymbolClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Symbol request supports dynamic
                                         -- registration.
    , _symbolKind :: Maybe WorkspaceSymbolKindClientCapabilities -- ^ Specific capabilities for the `SymbolKind`.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceSymbolClientCapabilities

-- -------------------------------------

data ExecuteCommandClientCapabilities =
  ExecuteCommandClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Execute command supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandClientCapabilities

-- -------------------------------------

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

-- ---------------------------------------------------------------------
{-
New in 3.0
----------
/**
 * Text document specific client capabilities.
 */
export interface TextDocumentClientCapabilities {

        synchronization?: {
                /**
                 * Whether text document synchronization supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports sending will save notifications.
                 */
                willSave?: boolean;

                /**
                 * The client supports sending a will save request and
                 * waits for a response providing text edits which will
                 * be applied to the document before it is saved.
                 */
                willSaveWaitUntil?: boolean;

                /**
                 * The client supports did save notifications.
                 */
                didSave?: boolean;
        }

        /**
         * Capabilities specific to the `textDocument/completion`
         */
        completion?: {
                /**
                 * Whether completion supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports the following `CompletionItem` specific
                 * capabilities.
                 */
                completionItem?: {
                        /**
                         * Client supports snippets as insert text.
                         *
                         * A snippet can define tab stops and placeholders with `$1`, `$2`
                         * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
                         * the end of the snippet. Placeholders with equal identifiers are linked,
                         * that is typing in one will update others too.
                         */
                        snippetSupport?: boolean;

                        /**
                         * Client supports commit characters on a completion item.
                         */
                        commitCharactersSupport?: boolean

                        /**
                         * Client supports the follow content formats for the documentation
                         * property. The order describes the preferred format of the client.
                         */
                        documentationFormat?: MarkupKind[];

                        /**
                         * Client supports the deprecated property on a completion item.
                         */
                        deprecatedSupport?: boolean;

                        /**
                         * Client supports the preselect property on a completion item.
                         */
                        preselectSupport?: boolean;
                }

                completionItemKind?: {
                        /**
                         * The completion item kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the completion items kinds from `Text` to `Reference` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: CompletionItemKind[];
                },

                /**
                 * The client supports to send additional context information for a
                 * `textDocument/completion` request.
                 */
                contextSupport?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/hover`
         */
        hover?: {
                /**
                 * Whether hover supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Client supports the follow content formats for the content
                 * property. The order describes the preferred format of the client.
                 */
                contentFormat?: MarkupKind[];
        };

        /**
         * Capabilities specific to the `textDocument/signatureHelp`
         */
        signatureHelp?: {
                /**
                 * Whether signature help supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports the following `SignatureInformation`
                 * specific properties.
                 */
                signatureInformation?: {
                        /**
                         * Client supports the follow content formats for the documentation
                         * property. The order describes the preferred format of the client.
                         */
                        documentationFormat?: MarkupKind[];
                };
        };

        /**
         * Capabilities specific to the `textDocument/references`
         */
        references?: {
                /**
                 * Whether references supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentHighlight`
         */
        documentHighlight?: {
                /**
                 * Whether document highlight supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentSymbol`
         */
        documentSymbol?: {
                /**
                 * Whether document symbol supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Specific capabilities for the `SymbolKind`.
                 */
                symbolKind?: {
                        /**
                         * The symbol kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the symbol kinds from `File` to `Array` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: SymbolKind[];
                }
        };

        /**
         * Capabilities specific to the `textDocument/formatting`
         */
        formatting?: {
                /**
                 * Whether formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/rangeFormatting`
         */
        rangeFormatting?: {
                /**
                 * Whether range formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/onTypeFormatting`
         */
        onTypeFormatting?: {
                /**
                 * Whether on type formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/definition`
         */
        definition?: {
                /**
                 * Whether definition supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/typeDefinition`
         *
         * Since 3.6.0
         */
        typeDefinition?: {
                /**
                 * Whether typeDefinition supports dynamic registration. If this is set to `true`
                 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/implementation`.
         *
         * Since 3.6.0
         */
        implementation?: {
                /**
                 * Whether implementation supports dynamic registration. If this is set to `true`
                 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/codeAction`
         */
        codeAction?: {
                /**
                 * Whether code action supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
                /**
                 * The client support code action literals as a valid
                 * response of the `textDocument/codeAction` request.
                 *
                 * Since 3.8.0
                 */
                codeActionLiteralSupport?: {
                        /**
                         * The code action kind is support with the following value
                         * set.
                         */
                        codeActionKind: {

                                /**
                                 * The code action kind values the client supports. When this
                                 * property exists the client also guarantees that it will
                                 * handle values outside its set gracefully and falls back
                                 * to a default value when unknown.
                                 */
                                valueSet: CodeActionKind[];
                        };
                };
        };

        /**
         * Capabilities specific to the `textDocument/codeLens`
         */
        codeLens?: {
                /**
                 * Whether code lens supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentLink`
         */
        documentLink?: {
                /**
                 * Whether document link supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentColor` and the
         * `textDocument/colorPresentation` request.
         *
         * Since 3.6.0
         */
        colorProvider?: {
                /**
                 * Whether colorProvider supports dynamic registration. If this is set to `true`
                 * the client supports the new `(ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        }

        /**
         * Capabilities specific to the `textDocument/rename`
         */
        rename?: {
                /**
                 * Whether rename supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
    /**
     * The client supports testing for validity of rename operations
     * before execution.
                 *
                 * Since 3.12.0
     */
    prepareSupport?: boolean;
        };

        /**
         * Capabilities specific to `textDocument/publishDiagnostics`.
         */
        publishDiagnostics?: {
                /**
                 * Whether the clients accepts diagnostics with related information.
                 */
                relatedInformation?: boolean;
        };

        /**
   * Capabilities specific to `textDocument/foldingRange` requests.
   *
   * Since 3.10.0
   */
  foldingRange?: {
    /**
     * Whether implementation supports dynamic registration for folding range providers. If this is set to `true`
     * the client supports the new `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
     * return value for the corresponding server capability as well.
     */
    dynamicRegistration?: boolean;
    /**
     * The maximum number of folding ranges that the client prefers to receive per document. The value serves as a
     * hint, servers are free to follow the limit.
     */
    rangeLimit?: number;
    /**
     * If set, the client signals that it only supports folding complete lines. If set, client will
     * ignore specified `startCharacter` and `endCharacter` properties in a FoldingRange.
     */
    lineFoldingOnly?: boolean;
  };
}

-}

-- -------------------------------------

-- TODO:AZ: this name is Java-ridiculously long
data SynchronizationTextDocumentClientCapabilities =
  SynchronizationTextDocumentClientCapabilities
    { -- | Whether text document synchronization supports dynamic registration.
      _dynamicRegistration :: Maybe Bool

      -- | The client supports sending will save notifications.
    , _willSave :: Maybe Bool

      -- | The client supports sending a will save request and waits for a
      -- response providing text edits which will be applied to the document
      -- before it is saved.
    , _willSaveWaitUntil :: Maybe Bool

      -- | The client supports did save notifications.
    , _didSave :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''SynchronizationTextDocumentClientCapabilities

instance Default SynchronizationTextDocumentClientCapabilities where
  def = SynchronizationTextDocumentClientCapabilities def def def def


-- -------------------------------------

data FormattingClientCapabilities =
  FormattingClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''FormattingClientCapabilities

-- -------------------------------------

data RangeFormattingClientCapabilities =
  RangeFormattingClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RangeFormattingClientCapabilities

-- -------------------------------------

data OnTypeFormattingClientCapabilities =
  OnTypeFormattingClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''OnTypeFormattingClientCapabilities

-- -------------------------------------

data CodeActionKindClientCapabilities =
  CodeActionKindClientCapabilities
   { -- | The code action kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
      _valueSet :: List CodeActionKind
   } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionKindClientCapabilities

instance Default CodeActionKindClientCapabilities where
  def = CodeActionKindClientCapabilities (List allKinds)
    where allKinds = [ CodeActionQuickFix
                     , CodeActionRefactor
                     , CodeActionRefactorExtract
                     , CodeActionRefactorInline
                     , CodeActionRefactorRewrite
                     , CodeActionSource
                     , CodeActionSourceOrganizeImports
                     ]

data CodeActionLiteralSupport =
  CodeActionLiteralSupport
    { _codeActionKind :: CodeActionKindClientCapabilities -- ^ The code action kind is support with the following value set.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionLiteralSupport

data CodeActionClientCapabilities =
  CodeActionClientCapabilities
    { _dynamicRegistration      :: Maybe Bool -- ^ Whether code action supports dynamic registration.
    , _codeActionLiteralSupport :: Maybe CodeActionLiteralSupport -- ^ The client support code action literals as a valid response
                                                                  -- of the `textDocument/codeAction` request.
                                                                  -- Since 3.8.0
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionClientCapabilities

-- -------------------------------------

data CodeLensClientCapabilities =
  CodeLensClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeLensClientCapabilities

-- -------------------------------------

data DocumentLinkClientCapabilities =
  DocumentLinkClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentLinkClientCapabilities

-- -------------------------------------

data ColorProviderClientCapabilities =
  ColorProviderClientCapabilities
    { -- | Whether colorProvider supports dynamic registration. If this is set to `true`
      --  the client supports the new `(ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.
      _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ColorProviderClientCapabilities

-- -------------------------------------

data RenameClientCapabilities =
  RenameClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    , _prepareSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RenameClientCapabilities

-- -------------------------------------

data PublishDiagnosticsTagsClientCapabilities =
  PublishDiagnosticsTagsClientCapabilities
    { -- | The tags supported by the client.
      _valueSet :: List DiagnosticTag
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''PublishDiagnosticsTagsClientCapabilities

data PublishDiagnosticsClientCapabilities =
  PublishDiagnosticsClientCapabilities
    { -- | Whether the clients accepts diagnostics with related information.
      _relatedInformation :: Maybe Bool
      -- | Client supports the tag property to provide metadata about a
      -- diagnostic.
      --
      -- Clients supporting tags have to handle unknown tags gracefully.
    , _tagSupport :: Maybe PublishDiagnosticsTagsClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''PublishDiagnosticsClientCapabilities

-- -------------------------------------

data FoldingRangeClientCapabilities =
  FoldingRangeClientCapabilities
    { -- | Whether implementation supports dynamic registration for folding range
      -- providers. If this is set to `true` the client supports the new
      -- `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      -- return value for the corresponding server capability as well.
      _dynamicRegistration :: Maybe Bool
      -- | The maximum number of folding ranges that the client prefers to receive
      -- per document. The value serves as a hint, servers are free to follow the limit.
    , _rangeLimit          :: Maybe Int
      -- | If set, the client signals that it only supports folding complete lines. If set,
      -- client will ignore specified `startCharacter` and `endCharacter` properties in a
      -- FoldingRange.
    , _lineFoldingOnly     :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''FoldingRangeClientCapabilities

-- -------------------------------------

data TextDocumentClientCapabilities =
  TextDocumentClientCapabilities
    { _synchronization :: Maybe SynchronizationTextDocumentClientCapabilities

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
    , _formatting :: Maybe FormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/rangeFormatting`
    , _rangeFormatting :: Maybe RangeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/onTypeFormatting`
    , _onTypeFormatting :: Maybe OnTypeFormattingClientCapabilities

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
    , _colorProvider :: Maybe ColorProviderClientCapabilities

      -- | Capabilities specific to the `textDocument/rename`
    , _rename :: Maybe RenameClientCapabilities

      -- | Capabilities specific to `textDocument/publishDiagnostics`
    , _publishDiagnostics :: Maybe PublishDiagnosticsClientCapabilities

      -- | Capabilities specific to `textDocument/foldingRange` requests. Since LSP 3.10.
      --
      -- @since 0.7.0.0
    , _foldingRange :: Maybe FoldingRangeClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentClientCapabilities

instance Default TextDocumentClientCapabilities where
  def = TextDocumentClientCapabilities def def def def def def def def
                                       def def def def def def def def
                                       def def def def def

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

-- ---------------------------------------------------------------------
{-
New in 3.0

-----------

ClientCapabilities

now define capabilities for dynamic registration, workspace and text document
features the client supports. The experimental can be used to pass experimential
capabilities under development. For future compatibility a ClientCapabilities
object literal can have more properties set than currently defined. Servers
receiving a ClientCapabilities object literal with unknown properties should
ignore these properties. A missing property should be interpreted as an absence
of the capability. If a property is missing that defines sub properties all sub
properties should be interpreted as an absence of the capability.

Client capabilities got introduced with the version 3.0 of the protocol. They
therefore only describe capabilities that got introduced in 3.x or later.
Capabilities that existed in the 2.x version of the protocol are still mandatory
for clients. Clients cannot opt out of providing them. So even if a client omits
the ClientCapabilities.textDocument.synchronization it is still required that
the client provides text document synchronization (e.g. open, changed and close
notifications).

interface ClientCapabilities {
        /**
         * Workspace specific client capabilities.
         */
        workspace?: WorkspaceClientCapabilities;

        /**
         * Text document specific client capabilities.
         */
        textDocument?: TextDocumentClientCapabilities;

        /**
         * Experimental client capabilities.
         */
        experimental?: any;

        /**
         * Window specific client capabilities.
         */
        window?: WindowClientCapabilities;
}
-}

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
