{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Haskell.LSP.Types.DataTypesJSON where

import           Control.Applicative
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Language.Haskell.LSP.Types.ClientCapabilities
import           Language.Haskell.LSP.Types.CodeAction
import           Language.Haskell.LSP.Types.Command
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Diagnostic
import           Language.Haskell.LSP.Types.DocumentFilter
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.MarkupContent
import           Language.Haskell.LSP.Types.Message
import           Language.Haskell.LSP.Types.Symbol
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Uri
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders

-- =====================================================================
-- ACTUAL PROTOCOL -----------------------------------------------------
-- =====================================================================

-- ---------------------------------------------------------------------
-- Initialize Request
-- ---------------------------------------------------------------------
{-
Initialize Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#initialize-request

The initialize request is sent as the first request from the client to the server.

Request

    method: 'initialize'
    params: InitializeParams defined as follows:

interface InitializeParams {
        /**
         * The process Id of the parent process that started
         * the server. Is null if the process has not been started by another process.
         * If the parent process is not alive then the server should exit (see exit notification) its process.
         */
        processId: number | null;

        /**
         * The rootPath of the workspace. Is null
         * if no folder is open.
         *
         * @deprecated in favour of rootUri.
         */
        rootPath?: string | null;

        /**
         * The rootUri of the workspace. Is null if no
         * folder is open. If both `rootPath` and `rootUri` are set
         * `rootUri` wins.
         */
        rootUri: DocumentUri | null;

        /**
         * User provided initialization options.
         */
        initializationOptions?: any;

        /**
         * The capabilities provided by the client (editor or tool)
         */
        capabilities: ClientCapabilities;

        /**
         * The initial trace setting. If omitted trace is disabled ('off').
         */
        trace?: 'off' | 'messages' | 'verbose';
}
-}

data Trace = TraceOff | TraceMessages | TraceVerbose
           deriving (Show, Read, Eq)

instance A.ToJSON Trace where
  toJSON TraceOff      = A.String (T.pack "off")
  toJSON TraceMessages = A.String (T.pack "messages")
  toJSON TraceVerbose  = A.String (T.pack "verbose")

instance A.FromJSON Trace where
  parseJSON (A.String s) = case T.unpack s of
    "off"      -> return TraceOff
    "messages" -> return TraceMessages
    "verbose"  -> return TraceVerbose
    _          -> mempty
  parseJSON _                               = mempty

data InitializeParams =
  InitializeParams {
    _processId             :: Maybe Int
  , _rootPath              :: Maybe Text -- ^ Deprecated in favour of _rootUri
  , _rootUri               :: Maybe Uri
  , _initializationOptions :: Maybe A.Value
  , _capabilities          :: ClientCapabilities
  , _trace                 :: Maybe Trace
  -- |  The workspace folders configured in the client when the server starts.
  -- This property is only available if the client supports workspace folders.
  -- It can be `null` if the client supports workspace folders but none are
  -- configured.
  -- Since LSP 3.6
  --
  -- @since 0.7.0.0
  , _workspaceFolders      :: Maybe (List WorkspaceFolder)
  } deriving (Show, Read, Eq)

{-# DEPRECATED _rootPath "Use _rootUri" #-}

deriveJSON lspOptions ''InitializeParams

-- ---------------------------------------------------------------------
-- Initialize Response
-- ---------------------------------------------------------------------
{-

    error.data:

interface InitializeError {
    /**
     * Indicates whether the client should retry to send the
     * initilize request after showing the message provided
     * in the ResponseError.
     */
    retry: boolean;
-
-}
data InitializeError =
  InitializeError
    { _retry :: Bool
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''InitializeError

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

instance A.ToJSON TextDocumentSyncKind where
  toJSON TdSyncNone        = A.Number 0
  toJSON TdSyncFull        = A.Number 1
  toJSON TdSyncIncremental = A.Number 2

instance A.FromJSON TextDocumentSyncKind where
  parseJSON (A.Number 0) = pure TdSyncNone
  parseJSON (A.Number 1) = pure TdSyncFull
  parseJSON (A.Number 2) = pure TdSyncIncremental
  parseJSON _            = mempty

-- ---------------------------------------------------------------------
{-
/**
 * Completion options.
 */
interface CompletionOptions {
    /**
     * The server provides support to resolve additional information for a completion item.
     */
    resolveProvider?: boolean;

    /**
     * The characters that trigger completion automatically.
     */
    triggerCharacters?: string[];
}
-}

data CompletionOptions =
  CompletionOptions
    { _resolveProvider   :: Maybe Bool
    , _triggerCharacters :: Maybe [String]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions {omitNothingFields = True } ''CompletionOptions

-- ---------------------------------------------------------------------
{-
/**
 * Signature help options.
 */
interface SignatureHelpOptions {
    /**
     * The characters that trigger signature help automatically.
     */
    triggerCharacters?: string[];
-}

data SignatureHelpOptions =
  SignatureHelpOptions
    { _triggerCharacters :: Maybe [String]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureHelpOptions

-- ---------------------------------------------------------------------
{-
/**
 * Code Lens options.
 */
interface CodeLensOptions {
    /**
     * Code lens has a resolve provider as well.
     */
    resolveProvider?: boolean;
}
-}

data CodeLensOptions =
  CodeLensOptions
    { _resolveProvider :: Maybe Bool
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeLensOptions

-- ---------------------------------------------------------------------
{-
/**
 * Code Action options.
 */
export interface CodeActionOptions {
    /**
     * CodeActionKinds that this server may return.
     *
     * The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
     * may list out every specific kind they provide.
     */
    codeActionKinds?: CodeActionKind[];
}
-}

data CodeActionOptions =
  CodeActionOptionsStatic Bool
  | CodeActionOptions
    { _codeActionKinds :: [CodeActionKind]
    } deriving (Read,Show,Eq)

deriveJSON (lspOptions { sumEncoding = A.UntaggedValue }) ''CodeActionOptions

-- ---------------------------------------------------------------------
{-
/**
 * Format document on type options
 */
interface DocumentOnTypeFormattingOptions {
    /**
     * A character on which formatting should be triggered, like `}`.
     */
    firstTriggerCharacter: string;
    /**
     * More trigger characters.
     */
    moreTriggerCharacter?: string[]
}
-}
data DocumentOnTypeFormattingOptions =
  DocumentOnTypeFormattingOptions
    { _firstTriggerCharacter :: Text
    , _moreTriggerCharacter  :: Maybe [String]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentOnTypeFormattingOptions

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
    { -- |Document links have a resolve provider as well.
      _resolveProvider :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentLinkOptions

-- ---------------------------------------------------------------------

{-
New in 3.0
-----------

/**
 * Execute command options.
 */
export interface ExecuteCommandOptions {
        /**
         * The commands to be executed on the server
         */
        commands: string[]
}
-}

data ExecuteCommandOptions =
  ExecuteCommandOptions
    { -- | The commands to be executed on the server
      _commands :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandOptions

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
    { -- |The client is supposed to include the content on save.
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

      -- |Save notifications are sent to the server.
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

data GotoOptions = GotoOptionsStatic Bool
                 | GotoOptionsDynamic
                    { -- | A document selector to identify the scope of the registration. If set to null
                      -- the document selector provided on the client side will be used.
                      _documentSelector :: Maybe DocumentSelector
                      -- | The id used to register the request. The id can be used to deregister
                      -- the request again. See also Registration#id.
                    , _id :: Maybe Text
                    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions { sumEncoding = A.UntaggedValue } ''GotoOptions
-- TODO: Figure out how to make Lens', not Traversal', for sum types
--makeFieldsNoPrefix ''GotoOptions

data ColorOptions = ColorOptionsStatic Bool
                  | ColorOptionsDynamic
                  | ColorOptionsDynamicDocument
                    { -- | A document selector to identify the scope of the registration. If set to null
                      -- the document selector provided on the client side will be used.
                      _documentSelector :: Maybe DocumentSelector
                      -- | The id used to register the request. The id can be used to deregister
                      -- the request again. See also Registration#id.
                    , _id :: Maybe Text
                    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions { sumEncoding = A.UntaggedValue } ''ColorOptions
-- makeFieldsNoPrefix ''ColorOptions

data FoldingRangeOptions = FoldingRangeOptionsStatic Bool
                         | FoldingRangeOptionsDynamic
                         | FoldingRangeOptionsDynamicDocument
                           { -- | A document selector to identify the scope of the registration. If set to null
                             -- the document selector provided on the client side will be used.
                             _documentSelector :: Maybe DocumentSelector
                             -- | The id used to register the request. The id can be used to deregister
                             -- the request again. See also Registration#id.
                           , _id :: Maybe Text
                           }
  deriving (Show, Read, Eq)

deriveJSON lspOptions { sumEncoding = A.UntaggedValue } ''FoldingRangeOptions
-- makeFieldsNoPrefix ''FoldingRangeOptions

data WorkspaceFolderChangeNotifications = WorkspaceFolderChangeNotificationsString Text
                                        | WorkspaceFolderChangeNotificationsBool Bool
  deriving (Show, Read, Eq)

deriveJSON lspOptions{ sumEncoding = A.UntaggedValue } ''WorkspaceFolderChangeNotifications

data WorkspaceFolderOptions =
  WorkspaceFolderOptions
    { -- | The server has support for workspace folders
      _supported :: Maybe Bool
      -- | Whether the server wants to receive workspace folder
      -- change notifications.
      -- If a strings is provided the string is treated as a ID
      -- under which the notification is registered on the client
      -- side. The ID can be used to unregister for these events
      -- using the `client/unregisterCapability` request.
    , _changeNotifications :: Maybe WorkspaceFolderChangeNotifications
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceFolderOptions

data WorkspaceOptions =
  WorkspaceOptions
    { -- |The server supports workspace folder. Since LSP 3.6
      --
      -- @since 0.7.0.0
      _workspaceFolders :: Maybe WorkspaceFolderOptions
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceOptions

data InitializeResponseCapabilitiesInner =
  InitializeResponseCapabilitiesInner
    { -- | Defines how text documents are synced. Is either a detailed structure
      -- defining each notification or for backwards compatibility the
      -- 'TextDocumentSyncKind' number.
      -- If omitted it defaults to 'TdSyncNone'.
      _textDocumentSync                 :: Maybe TDS
      -- | The server provides hover support.
    , _hoverProvider                    :: Maybe Bool
      -- | The server provides completion support.
    , _completionProvider               :: Maybe CompletionOptions
      -- | The server provides signature help support.
    , _signatureHelpProvider            :: Maybe SignatureHelpOptions
      -- | The server provides goto definition support.
    , _definitionProvider               :: Maybe Bool
      -- | The server provides Goto Type Definition support. Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _typeDefinitionProvider           :: Maybe GotoOptions
      -- | The server provides Goto Implementation support.
      -- Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _implementationProvider           :: Maybe GotoOptions
      -- | The server provides find references support.
    , _referencesProvider               :: Maybe Bool
      -- | The server provides document highlight support.
    , _documentHighlightProvider        :: Maybe Bool
      -- | The server provides document symbol support.
    , _documentSymbolProvider           :: Maybe Bool
      -- | The server provides workspace symbol support.
    , _workspaceSymbolProvider          :: Maybe Bool
      -- | The server provides code actions.
    , _codeActionProvider               :: Maybe CodeActionOptions
      -- | The server provides code lens.
    , _codeLensProvider                 :: Maybe CodeLensOptions
      -- | The server provides document formatting.
    , _documentFormattingProvider       :: Maybe Bool
      -- | The server provides document range formatting.
    , _documentRangeFormattingProvider  :: Maybe Bool
      -- | The server provides document formatting on typing.
    , _documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
      -- | The server provides rename support.
    , _renameProvider                   :: Maybe Bool
      -- | The server provides document link support.
    , _documentLinkProvider             :: Maybe DocumentLinkOptions
      -- | The server provides color provider support. Since LSP 3.6
      --
      -- @since 0.7.0.0
    , _colorProvider                    :: Maybe ColorOptions
      -- | The server provides folding provider support. Since LSP 3.10
      --
      -- @since 0.7.0.0
    , _foldingRangeProvider             :: Maybe FoldingRangeOptions
      -- | The server provides execute command support.
    , _executeCommandProvider           :: Maybe ExecuteCommandOptions
      -- | Workspace specific server capabilities
    , _workspace                        :: Maybe WorkspaceOptions
      -- | Experimental server capabilities.
    , _experimental                     :: Maybe A.Value
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResponseCapabilitiesInner

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    _capabilities :: InitializeResponseCapabilitiesInner
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResponseCapabilities

-- ---------------------------------------------------------------------

type InitializeResponse = ResponseMessage InitializeResponseCapabilities

type InitializeRequest = RequestMessage ClientMethod InitializeParams InitializeResponseCapabilities

{-
    error.code:

/**
 * Known error codes for an `InitializeError`;
 */
export namespace InitializeError {
        /**
         * If the protocol version provided by the client can't be handled by the server.
         * @deprecated This initialize error got replaced by client capabilities. There is
         * no version handshake in version 3.0x
         */
        export const unknownProtocolVersion: number = 1;
}

    error.data:

interface InitializeError {
        /**
         * Indicates whether the client execute the following retry logic:
         * (1) show the message provided by the ResponseError to the user
         * (2) user selects retry or cancel
         * (3) if user selected retry the initialize method is sent again.
         */
        retry: boolean;
}
-}

-- ---------------------------------------------------------------------

{-
New in 3.0
----------
Initialized Notification

The initialized notification is sent from the client to the server after the
client is fully initialized and is able to listen to arbritary requests and
notifications sent from the server.

Notification:

    method: 'initialized'
    params: void

-}

data InitializedParams =
  InitializedParams
    {
    } deriving (Show, Read, Eq)

instance A.FromJSON InitializedParams where
  parseJSON (A.Object _) = pure InitializedParams
  parseJSON _            = mempty

instance A.ToJSON InitializedParams where
  toJSON InitializedParams = A.Object mempty

type InitializedNotification = NotificationMessage ClientMethod (Maybe InitializedParams)

-- ---------------------------------------------------------------------
{-
Shutdown Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#shutdown-request

The shutdown request is sent from the client to the server. It asks the server
to shut down, but to not exit (otherwise the response might not be delivered
correctly to the client). There is a separate exit notification that asks the
server to exit.

Request

    method: 'shutdown'
    params: undefined

Response

    result: undefined
    error: code and message set in case an exception happens during shutdown request.


-}

type ShutdownRequest  = RequestMessage ClientMethod (Maybe A.Value) Text
type ShutdownResponse = ResponseMessage Text

-- ---------------------------------------------------------------------
{-
Exit Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#exit-notification

A notification to ask the server to exit its process.

Notification

    method: 'exit'

-}

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data ExitParams =
  ExitParams
    {
    } deriving (Show, Read, Eq)

deriveJSON defaultOptions ''ExitParams

type ExitNotification = NotificationMessage ClientMethod (Maybe ExitParams)

-- ---------------------------------------------------------------------
{-
Telemetry Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#telemetry-notification

    New: The telemetry notification is sent from the server to the client to ask
    the client to log a telemetry event.

Notification:

    method: 'telemetry/event'
    params: 'any'
-}


type TelemetryNotification = NotificationMessage ServerMethod A.Value

type CustomClientNotification = NotificationMessage ClientMethod A.Value
type CustomServerNotification = NotificationMessage ServerMethod A.Value

type CustomClientRequest = RequestMessage ClientMethod A.Value A.Value
type CustomServerRequest = RequestMessage ServerMethod A.Value A.Value

type CustomResponse = ResponseMessage A.Value

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Register Capability

The client/registerCapability request is sent from the server to the client to
register for a new capability on the client side. Not all clients need to
support dynamic capability registration. A client opts in via the
ClientCapabilities.dynamicRegistration property.

Request:

    method: 'client/registerCapability'
    params: RegistrationParams

Where RegistrationParams are defined as follows:

/**
 * General paramters to to regsiter for a capability.
 */
export interface Registration {
        /**
         * The id used to register the request. The id can be used to deregister
         * the request again.
         */
        id: string;

        /**
         * The method / capability to register for.
         */
        method: string;

        /**
         * Options necessary for the registration.
         */
        registerOptions?: any;
}

export interface RegistrationParams {
        registrations: Registration[];
}
-}

data Registration =
  Registration
    { -- |The id used to register the request. The id can be used to deregister
      -- the request again.
      _id              :: Text

       -- | The method / capability to register for.
    , _method          :: ClientMethod

      -- | Options necessary for the registration.
    , _registerOptions :: Maybe A.Value
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''Registration

data RegistrationParams =
  RegistrationParams
    { _registrations :: List Registration
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RegistrationParams

-- |Note: originates at the server
type RegisterCapabilityRequest = RequestMessage ServerMethod RegistrationParams ()

type RegisterCapabilityResponse = ResponseMessage ()

-- -------------------------------------

{-
Since most of the registration options require to specify a document selector
there is a base interface that can be used.

export interface TextDocumentRegistrationOptions {
        /**
         * A document selector to identify the scope of the registration. If set to null
         * the document selector provided on the client side will be used.
         */
        documentSelector: DocumentSelector | null;
}
-}

data TextDocumentRegistrationOptions =
  TextDocumentRegistrationOptions
    { _documentSelector :: Maybe DocumentSelector
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentRegistrationOptions

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Unregister Capability

The client/unregisterCapability request is sent from the server to the client to
unregister a previously register capability.

Request:

    method: 'client/unregisterCapability'
    params: UnregistrationParams

Where UnregistrationParams are defined as follows:

/**
 * General parameters to unregister a capability.
 */
export interface Unregistration {
        /**
         * The id used to unregister the request or notification. Usually an id
         * provided during the register request.
         */
        id: string;

        /**
         * The method / capability to unregister for.
         */
        method: string;
}

export interface UnregistrationParams {
        unregisterations: Unregistration[];
}
-}

data Unregistration =
  Unregistration
    { -- | The id used to unregister the request or notification. Usually an id
      -- provided during the register request.
      _id     :: Text

       -- |The method / capability to unregister for.
    , _method :: Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''Unregistration

data UnregistrationParams =
  UnregistrationParams
    { _unregistrations :: List Unregistration
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''UnregistrationParams

type UnregisterCapabilityRequest = RequestMessage ServerMethod UnregistrationParams ()

type UnregisterCapabilityResponse = ResponseMessage ()

-- ---------------------------------------------------------------------
{-
DidChangeConfiguration Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didchangeconfiguration-notification

A notification sent from the client to the server to signal the change of
configuration settings.

Notification:

    method: 'workspace/didChangeConfiguration',
    params: DidChangeConfigurationParams defined as follows:

interface DidChangeConfigurationParams {
    /**
     * The actual changed settings
     */
    settings: any;
}
-}

data DidChangeConfigurationParams =
  DidChangeConfigurationParams {
    _settings :: A.Value
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeConfigurationParams

-- ---------------------------------------------------------------------

type DidChangeConfigurationNotification = NotificationMessage ClientMethod DidChangeConfigurationParams

{-
Configuration Request (:arrow_right_hook:)
Since version 3.6.0

The workspace/configuration request is sent from the server to the client to
fetch configuration settings from the client. The request can fetch n
configuration settings in one roundtrip. The order of the returned configuration
settings correspond to the order of the passed ConfigurationItems (e.g. the
first item in the response is the result for the first configuration item in the
params).

A ConfigurationItem consist of the configuration section to ask for and an
additional scope URI. The configuration section ask for is defined by the server
and doesn’t necessarily need to correspond to the configuration store used be
the client. So a server might ask for a configuration cpp.formatterOptions but
the client stores the configuration in a XML store layout differently. It is up
to the client to do the necessary conversion. If a scope URI is provided the
client should return the setting scoped to the provided resource. If the client
for example uses EditorConfig to manage its settings the configuration should be
returned for the passed resource URI. If the client can’t provide a
configuration setting for a given scope then null need to be present in the
returned array.

Request:

method: ‘workspace/configuration’
params: ConfigurationParams defined as follows
export interface ConfigurationParams {
        items: ConfigurationItem[];
}

export interface ConfigurationItem {
        /**
         * The scope to get the configuration section for.
         */
        scopeUri?: string;

        /**
         * The configuration section asked for.
         */
        section?: string;
}
Response:

result: any[]
error: code and message set in case an exception happens during the
‘workspace/configuration’ request
-}

data ConfigurationItem =
  ConfigurationItem
    { _scopeUri :: Maybe Text -- ^ The scope to get the configuration section for.
    , _section  :: Maybe Text -- ^ The configuration section asked for.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ConfigurationItem

data ConfigurationParams =
  ConfigurationParams
    { _items :: List ConfigurationItem
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ConfigurationParams

type ConfigurationRequest = RequestMessage ServerMethod ConfigurationParams (List A.Value)
type ConfigurationResponse = ResponseMessage (List A.Value)

-- ---------------------------------------------------------------------
{-
DidOpenTextDocument Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didopentextdocument-notification

The document open notification is sent from the client to the server to signal
newly opened text documents. The document's truth is now managed by the client
and the server must not try to read the document's truth using the document's
uri.

Notification:

    method: 'textDocument/didOpen'
    params: DidOpenTextDocumentParams defined as follows:

interface DidOpenTextDocumentParams {
    /**
     * The document that was opened.
     */
    textDocument: TextDocumentItem;
}

Registration Options: TextDocumentRegistrationOptions
-}

data DidOpenTextDocumentParams =
  DidOpenTextDocumentParams {
    _textDocument :: TextDocumentItem
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidOpenTextDocumentParams

type DidOpenTextDocumentNotification = NotificationMessage ClientMethod DidOpenTextDocumentParams

-- ---------------------------------------------------------------------
{-
DidChangeTextDocument Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didchangetextdocument-notification

    Changed: The document change notification is sent from the client to the
    server to signal changes to a text document. In 2.0 the shape of the params
    has changed to include proper version numbers and language ids.

Notification:

    method: 'textDocument/didChange'
    params: DidChangeTextDocumentParams defined as follows:

interface DidChangeTextDocumentParams {
    /**
     * The document that did change. The version number points
     * to the version after all provided content changes have
     * been applied.
     */
    textDocument: VersionedTextDocumentIdentifier;

    /**
     * The actual content changes.
     */
    contentChanges: TextDocumentContentChangeEvent[];
}

/**
 * An event describing a change to a text document. If range and rangeLength are omitted
 * the new text is considered to be the full content of the document.
 */
interface TextDocumentContentChangeEvent {
    /**
     * The range of the document that changed.
     */
    range?: Range;

    /**
     * The length of the range that got replaced.
     */
    rangeLength?: number;

    /**
     * The new text of the document.
     */
    text: string;
}
-}
data TextDocumentContentChangeEvent =
  TextDocumentContentChangeEvent
    { _range       :: Maybe Range
    , _rangeLength :: Maybe Int
    , _text        :: Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions { omitNothingFields = True } ''TextDocumentContentChangeEvent

-- -------------------------------------

data DidChangeTextDocumentParams =
  DidChangeTextDocumentParams
    { _textDocument   :: VersionedTextDocumentIdentifier
    , _contentChanges :: List TextDocumentContentChangeEvent
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DidChangeTextDocumentParams

type DidChangeTextDocumentNotification = NotificationMessage ClientMethod DidChangeTextDocumentParams
{-
New in 3.0
----------

Registration Options: TextDocumentChangeRegistrationOptions defined as follows:

/**
 * Descibe options to be used when registered for text document change events.
 */
export interface TextDocumentChangeRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * How documents are synced to the server. See TextDocumentSyncKind.Full
         * and TextDocumentSyncKindIncremental.
         */
        syncKind: number;
}
-}

data TextDocumentChangeRegistrationOptions =
  TextDocumentChangeRegistrationOptions
    { _documentSelector :: Maybe DocumentSelector
    , _syncKind         :: TextDocumentSyncKind
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentChangeRegistrationOptions

-- ---------------------------------------------------------------------
{-

New in 3.0
----------

WillSaveTextDocument Notification

The document will save notification is sent from the client to the server before
the document is actually saved.

Notification:

    method: 'textDocument/willSave'
    params: WillSaveTextDocumentParams defined as follows:

/**
 * The parameters send in a will save text document notification.
 */
export interface WillSaveTextDocumentParams {
        /**
         * The document that will be saved.
         */
        textDocument: TextDocumentIdentifier;

        /**
         * The 'TextDocumentSaveReason'.
         */
        reason: number;
}

/**
 * Represents reasons why a text document is saved.
 */
export namespace TextDocumentSaveReason {

        /**
         * Manually triggered, e.g. by the user pressing save, by starting debugging,
         * or by an API call.
         */
        export const Manual = 1;

        /**
         * Automatic after a delay.
         */
        export const AfterDelay = 2;

        /**
         * When the editor lost focus.
         */
        export const FocusOut = 3;
}
Registration Options: TextDocumentRegistrationOptions
-}

data TextDocumentSaveReason
  = SaveManual
         -- ^ Manually triggered, e.g. by the user pressing save, by starting
         -- debugging, or by an API call.
  | SaveAfterDelay -- ^ Automatic after a delay
  | SaveFocusOut   -- ^ When the editor lost focus
  deriving (Show, Read, Eq)

instance A.ToJSON TextDocumentSaveReason where
  toJSON SaveManual     = A.Number 1
  toJSON SaveAfterDelay = A.Number 2
  toJSON SaveFocusOut   = A.Number 3

instance A.FromJSON TextDocumentSaveReason where
  parseJSON (A.Number 1) = pure SaveManual
  parseJSON (A.Number 2) = pure SaveAfterDelay
  parseJSON (A.Number 3) = pure SaveFocusOut
  parseJSON _            = mempty

data WillSaveTextDocumentParams =
  WillSaveTextDocumentParams
    { _textDocument :: TextDocumentIdentifier
    , _reason       :: TextDocumentSaveReason
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WillSaveTextDocumentParams

type WillSaveTextDocumentNotification = NotificationMessage ClientMethod WillSaveTextDocumentParams

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

WillSaveWaitUntilTextDocument Request

The document will save request is sent from the client to the server before the
document is actually saved. The request can return an array of TextEdits which
will be applied to the text document before it is saved. Please note that
clients might drop results if computing the text edits took too long or if a
server constantly fails on this request. This is done to keep the save fast and
reliable.

Request:

    method: 'textDocument/willSaveWaitUntil'
    params: WillSaveTextDocumentParams

Response:

    result: TextEdit[]
    error: code and message set in case an exception happens during the willSaveWaitUntil request.

Registration Options: TextDocumentRegistrationOptions
-}

type WillSaveWaitUntilTextDocumentRequest = RequestMessage ClientMethod WillSaveTextDocumentParams (List TextEdit)
type WillSaveWaitUntilTextDocumentResponse = ResponseMessage (List TextEdit)

-- ---------------------------------------------------------------------
{-
DidSaveTextDocument Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didsavetextdocument-notification

    New: The document save notification is sent from the client to the server
    when the document was saved in the client.

    method: 'textDocument/didSave'
    params: DidSaveTextDocumentParams defined as follows:

interface DidSaveTextDocumentParams {
    /**
     * The document that was saved.
     */
    textDocument: TextDocumentIdentifier;
}
-}
data DidSaveTextDocumentParams =
  DidSaveTextDocumentParams
    { _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidSaveTextDocumentParams

type DidSaveTextDocumentNotification = NotificationMessage ClientMethod DidSaveTextDocumentParams



-- ---------------------------------------------------------------------
{-
DidCloseTextDocument Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didclosetextdocument-notification

The document close notification is sent from the client to the server when the
document got closed in the client. The document's truth now exists where the
document's uri points to (e.g. if the document's uri is a file uri the truth now
exists on disk).

    Changed: In 2.0 the params are of type DidCloseTextDocumentParams which
    contains a reference to a text document.

Notification:

    method: 'textDocument/didClose'
    params: DidCloseTextDocumentParams defined as follows:

interface DidCloseTextDocumentParams {
    /**
     * The document that was closed.
     */
    textDocument: TextDocumentIdentifier;
}
-}
data DidCloseTextDocumentParams =
  DidCloseTextDocumentParams
    { _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidCloseTextDocumentParams


type DidCloseTextDocumentNotification = NotificationMessage ClientMethod DidCloseTextDocumentParams

-- ---------------------------------------------------------------------
{-
DidChangeWatchedFiles Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didchangewatchedfiles-notification

The watched files notification is sent from the client to the server when the
client detects changes to files watched by the language client.

Notification:

    method: 'workspace/didChangeWatchedFiles'
    params: DidChangeWatchedFilesParams defined as follows:

interface DidChangeWatchedFilesParams {
    /**
     * The actual file events.
     */
    changes: FileEvent[];
}

Where FileEvents are described as follows:

/**
 * The file event type.
 */
enum FileChangeType {
    /**
     * The file got created.
     */
    Created = 1,
    /**
     * The file got changed.
     */
    Changed = 2,
    /**
     * The file got deleted.
     */
    Deleted = 3
}

/**
 * An event describing a file change.
 */
interface FileEvent {
    /**
     * The file's URI.
     */
    uri: string;
    /**
     * The change type.
     */
    type: number;
-}
data FileChangeType = FcCreated
                    | FcChanged
                    | FcDeleted
       deriving (Read,Show,Eq)

instance A.ToJSON FileChangeType where
  toJSON FcCreated = A.Number 1
  toJSON FcChanged = A.Number 2
  toJSON FcDeleted = A.Number 3

instance A.FromJSON FileChangeType where
  parseJSON (A.Number 1) = pure FcCreated
  parseJSON (A.Number 2) = pure FcChanged
  parseJSON (A.Number 3) = pure FcDeleted
  parseJSON _            = mempty


-- -------------------------------------

data FileEvent =
  FileEvent
    { _uri   :: Uri
    , _xtype :: FileChangeType
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''FileEvent

data DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams
    { _changes :: List FileEvent
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidChangeWatchedFilesParams


type DidChangeWatchedFilesNotification = NotificationMessage ClientMethod DidChangeWatchedFilesParams

-- ---------------------------------------------------------------------
{-
PublishDiagnostics Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#publishdiagnostics-notification

Diagnostics notification are sent from the server to the client to signal
results of validation runs.

Notification

    method: 'textDocument/publishDiagnostics'
    params: PublishDiagnosticsParams defined as follows:

interface PublishDiagnosticsParams {
    /**
     * The URI for which diagnostic information is reported.
     */
    uri: string;

    /**
     * An array of diagnostic information items.
     */
    diagnostics: Diagnostic[];
}
-}

data PublishDiagnosticsParams =
  PublishDiagnosticsParams
    { _uri         :: Uri
    , _diagnostics :: List Diagnostic
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''PublishDiagnosticsParams


type PublishDiagnosticsNotification = NotificationMessage ServerMethod PublishDiagnosticsParams

-- ---------------------------------------------------------------------
{-
Hover Request

The hover request is sent from the client to the server to request hover
information at a given text document position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with a proper
    textDocument and position property. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/hover'
    params: TextDocumentPositionParams

Response

    result: Hover | null defined as follows:


/**
 * The result of a hover request.
 */
interface Hover {
        /**
         * The hover's content
         */
        contents: MarkedString | MarkedString[] | MarkupContent;

        /**
         * An optional range is a range inside a text document
         * that is used to visualize a hover, e.g. by changing the background color.
         */
        range?: Range;
}


/**
 * MarkedString can be used to render human readable text. It is either a markdown string
 * or a code-block that provides a language and a code snippet. The language identifier
 * is semantically equal to the optional language identifier in fenced code blocks in GitHub
 * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
 *
 * The pair of a language and a value is an equivalent to markdown:
 * ```${language}
 * ${value}
 * ```
 *
 * Note that markdown strings will be sanitized - that means html will be escaped.
* @deprecated use MarkupContent instead.
*/
type MarkedString = string | { language: string; value: string };

    error: code and message set in case an exception happens during the hover
    request.

Registration Options: TextDocumentRegistrationOptions

-}

data LanguageString =
  LanguageString
    { _language :: Text
    , _value    :: Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''LanguageString

{-# DEPRECATED MarkedString, PlainString, CodeString "Use MarkupContent instead, since 3.3.0 (11/24/2017)" #-}
data MarkedString =
    PlainString T.Text
  | CodeString LanguageString
    deriving (Eq,Read,Show)

instance ToJSON MarkedString where
  toJSON (PlainString x) = toJSON x
  toJSON (CodeString  x) = toJSON x
instance FromJSON MarkedString where
  parseJSON (A.String t) = pure $ PlainString t
  parseJSON o            = CodeString <$> parseJSON o

-- -------------------------------------

data HoverContents =
    HoverContentsMS (List MarkedString)
  | HoverContents   MarkupContent
  deriving (Read,Show,Eq)

instance ToJSON HoverContents where
  toJSON (HoverContentsMS  x) = toJSON x
  toJSON (HoverContents    x) = toJSON x
instance FromJSON HoverContents where
  parseJSON v@(A.String _) = HoverContentsMS <$> parseJSON v
  parseJSON v@(A.Array _)  = HoverContentsMS <$> parseJSON v
  parseJSON v@(A.Object _) = HoverContents   <$> parseJSON v
                         <|> HoverContentsMS <$> parseJSON v
  parseJSON _ = mempty

-- -------------------------------------

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup HoverContents where
  (<>) = mappend
#endif

instance Monoid HoverContents where
  mempty = HoverContentsMS (List [])

  HoverContents h1   `mappend` HoverContents         h2   = HoverContents (h1 `mappend` h2)
  HoverContents h1   `mappend` HoverContentsMS (List h2s) = HoverContents (mconcat (h1: (map toMarkupContent h2s)))
  HoverContentsMS (List h1s) `mappend` HoverContents         h2    = HoverContents (mconcat ((map toMarkupContent h1s) ++ [h2]))
  HoverContentsMS (List h1s) `mappend` HoverContentsMS (List h2s) = HoverContentsMS (List (h1s `mappend` h2s))

toMarkupContent :: MarkedString -> MarkupContent
toMarkupContent (PlainString s) = unmarkedUpContent s
toMarkupContent (CodeString (LanguageString lang s)) = markedUpContent lang s

-- -------------------------------------

data Hover =
  Hover
    { _contents :: HoverContents
    , _range    :: Maybe Range
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''Hover


type HoverRequest = RequestMessage ClientMethod TextDocumentPositionParams (Maybe Hover)
type HoverResponse = ResponseMessage (Maybe Hover)

-- ---------------------------------------------------------------------
{-
Signature Help Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#signature-help-request

The signature help request is sent from the client to the server to request
signature information at a given cursor position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/signatureHelp'
    params: TextDocumentPositionParams

Response

    result: SignatureHelp defined as follows:

/**
 * Signature help represents the signature of something
 * callable. There can be multiple signature but only one
 * active and only one active parameter.
 */
interface SignatureHelp {
    /**
     * One or more signatures.
     */
    signatures: SignatureInformation[];

    /**
     * The active signature.
     */
    activeSignature?: number;

    /**
     * The active parameter of the active signature.
     */
    activeParameter?: number;
}

/**
 * Represents the signature of something callable. A signature
 * can have a label, like a function-name, a doc-comment, and
 * a set of parameters.
 */
interface SignatureInformation {
    /**
     * The label of this signature. Will be shown in
     * the UI.
     */
    label: string;

    /**
     * The human-readable doc-comment of this signature. Will be shown
     * in the UI but can be omitted.
     */
    documentation?: string;

    /**
     * The parameters of this signature.
     */
    parameters?: ParameterInformation[];
}

/**
 * Represents a parameter of a callable-signature. A parameter can
 * have a label and a doc-comment.
 */
interface ParameterInformation {
    /**
     * The label of this signature. Will be shown in
     * the UI.
     */
    label: string;

    /**
     * The human-readable doc-comment of this signature. Will be shown
     * in the UI but can be omitted.
     */
    documentation?: string;
}

    error: code and message set in case an exception happens during the
    signature help request.
-}


data ParameterInformation =
  ParameterInformation
    { _label         :: Text
    , _documentation :: Maybe Text
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''ParameterInformation


-- -------------------------------------

data SignatureInformation =
  SignatureInformation
    { _label         :: Text
    , _documentation :: Maybe Text
    , _parameters    :: Maybe [ParameterInformation]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureInformation

data SignatureHelp =
  SignatureHelp
    { _signatures      :: List SignatureInformation
    , _activeSignature :: Maybe Int -- ^ The active signature
    , _activeParameter :: Maybe Int -- ^ The active parameter of the active signature
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureHelp

type SignatureHelpRequest = RequestMessage ClientMethod TextDocumentPositionParams SignatureHelp
type SignatureHelpResponse = ResponseMessage SignatureHelp

-- -------------------------------------
{-
New in 3.0
----------
Registration Options: SignatureHelpRegistrationOptions defined as follows:

export interface SignatureHelpRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * The characters that trigger signature help
         * automatically.
         */
        triggerCharacters?: string[];
}
-}

data SignatureHelpRegistrationOptions =
  SignatureHelpRegistrationOptions
    { _documentSelector  :: Maybe DocumentSelector
    , _triggerCharacters :: Maybe (List String)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''SignatureHelpRegistrationOptions

-- ---------------------------------------------------------------------
{-
Goto Definition Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#goto-definition-request

The goto definition request is sent from the client to the server to resolve the
definition location of a symbol at a given text document position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/definition'
    params: TextDocumentPositionParams

Response:

    result: Location | Location[]
    error: code and message set in case an exception happens during the definition request.


-}

-- {"jsonrpc":"2.0","id":1,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/Foo.hs"},"position":{"line":1,"character":8}}}

data LocationResponseParams = SingleLoc Location | MultiLoc [Location]
  deriving (Eq,Read,Show)

instance A.ToJSON LocationResponseParams where
  toJSON (SingleLoc x) = toJSON x
  toJSON (MultiLoc xs) = toJSON xs

instance A.FromJSON LocationResponseParams where
  parseJSON xs@(A.Array _) = MultiLoc <$> parseJSON xs
  parseJSON x              = SingleLoc <$> parseJSON x

type DefinitionRequest  = RequestMessage ClientMethod TextDocumentPositionParams LocationResponseParams
type DefinitionResponse = ResponseMessage LocationResponseParams

-- ---------------------------------------------------------------------

{-
Goto Type Definition Request (:leftwards_arrow_with_hook:)
Since version 3.6.0

The goto type definition request is sent from the client to the server to resolve the type definition location of a symbol at a given text document position.

Request:

method: ‘textDocument/typeDefinition’
params: TextDocumentPositionParams
Response:

result: Location | Location[] | null
error: code and message set in case an exception happens during the definition request.
Registration Options: TextDocumentRegistrationOptions
-}

type TypeDefinitionRequest = RequestMessage ClientMethod TextDocumentPositionParams LocationResponseParams
type TypeDefinitionResponse = ResponseMessage LocationResponseParams

-- ---------------------------------------------------------------------

{-
Goto Implementation Request (:leftwards_arrow_with_hook:)
Since version 3.6.0

The goto implementation request is sent from the client to the server to resolve the implementation location of a symbol at a given text document position.

Request:

method: ‘textDocument/implementation’
params: TextDocumentPositionParams
Response:

result: Location | Location[] | null
error: code and message set in case an exception happens during the definition request.
Registration Options: TextDocumentRegistrationOptions
-}


type ImplementationRequest  = RequestMessage ClientMethod TextDocumentPositionParams LocationResponseParams
type ImplementationResponse = ResponseMessage LocationResponseParams

-- ---------------------------------------------------------------------

{-
Find References Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#find-references-request

The references request is sent from the client to the server to resolve
project-wide references for the symbol denoted by the given text document
position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/references'
    params: ReferenceParams defined as follows:

interface ReferenceParams extends TextDocumentPositionParams {
    context: ReferenceContext
}

interface ReferenceContext {
    /**
     * Include the declaration of the current symbol.
     */
    includeDeclaration: boolean;
}

Response:

    result: Location[]
    error: code and message set in case an exception happens during the
           reference request.
-}

data ReferenceContext =
  ReferenceContext
    { _includeDeclaration :: Bool
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ReferenceContext


data ReferenceParams =
  ReferenceParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _context      :: ReferenceContext
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ReferenceParams


type ReferencesRequest  = RequestMessage ClientMethod ReferenceParams (List Location)
type ReferencesResponse = ResponseMessage (List Location)

-- ---------------------------------------------------------------------
{-
Document Highlights Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-highlights-request

The document highlight request is sent from the client to the server to resolve
a document highlights for a given text document position. For programming
languages this usually highlights all references to the symbol scoped to this
file. However we kept 'textDocument/documentHighlight' and
'textDocument/references' separate requests since the first one is allowed to be
more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read or Write
whereas fuzzy or textual matches use Textas the kind.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/documentHighlight'
    params: TextDocumentPositionParams

Response

    result: DocumentHighlight[] defined as follows:

/**
 * A document highlight is a range inside a text document which deserves
 * special attention. Usually a document highlight is visualized by changing
 * the background color of its range.
 *
 */
interface DocumentHighlight {
    /**
     * The range this highlight applies to.
     */
    range: Range;

    /**
     * The highlight kind, default is DocumentHighlightKind.Text.
     */
    kind?: number;
}

/**
 * A document highlight kind.
 */
enum DocumentHighlightKind {
    /**
     * A textual occurrance.
     */
    Text = 1,

    /**
     * Read-access of a symbol, like reading a variable.
     */
    Read = 2,

    /**
     * Write-access of a symbol, like writing to a variable.
     */
    Write = 3
}

    error: code and message set in case an exception happens during the document
           highlight request.

Registration Options: TextDocumentRegistrationOptions

-}

data DocumentHighlightKind = HkText | HkRead | HkWrite
  deriving (Read,Show,Eq)

instance A.ToJSON DocumentHighlightKind where
  toJSON HkText  = A.Number 1
  toJSON HkRead  = A.Number 2
  toJSON HkWrite = A.Number 3

instance A.FromJSON DocumentHighlightKind where
  parseJSON (A.Number 1) = pure HkText
  parseJSON (A.Number 2) = pure HkRead
  parseJSON (A.Number 3) = pure HkWrite
  parseJSON _            = mempty

-- -------------------------------------

data DocumentHighlight =
  DocumentHighlight
    { _range :: Range
    , _kind  :: Maybe DocumentHighlightKind
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentHighlight

type DocumentHighlightRequest = RequestMessage ClientMethod TextDocumentPositionParams (List DocumentHighlight)
type DocumentHighlightsResponse = ResponseMessage (List DocumentHighlight)

-- ---------------------------------------------------------------------
{-
Workspace Symbols Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#workspace-symbols-request

The workspace symbol request is sent from the client to the server to list
project-wide symbols matching the query string.

Request

    method: 'workspace/symbol'
    params: WorkspaceSymbolParams defined as follows:

/**
 * The parameters of a Workspace Symbol Request.
 */
interface WorkspaceSymbolParams {
    /**
     * A non-empty query string
     */
    query: string;
}

Response

    result: SymbolInformation[] as defined above.
    error: code and message set in case an exception happens during the
           workspace symbol request.
-}

data WorkspaceSymbolParams =
  WorkspaceSymbolParams
    { _query :: Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''WorkspaceSymbolParams

type WorkspaceSymbolRequest  = RequestMessage ClientMethod WorkspaceSymbolParams (List SymbolInformation)
type WorkspaceSymbolsResponse = ResponseMessage (List SymbolInformation)

-- ---------------------------------------------------------------------
{-
Code Lens Request

The code lens request is sent from the client to the server to compute code
lenses for a given text document.

    Changed: In 2.0 the request uses CodeLensParams instead of a single uri.

Request

    method: 'textDocument/codeLens'
    params: CodeLensParams defined as follows:

interface CodeLensParams {
    /**
     * The document to request code lens for.
     */
    textDocument: TextDocumentIdentifier;
}

Response

    result: CodeLens[] defined as follows:

/**
 * A code lens represents a command that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For performance
 * reasons the creation of a code lens and resolving should be done in two stages.
 */
interface CodeLens {
    /**
     * The range in which this code lens is valid. Should only span a single line.
     */
    range: Range;

    /**
     * The command this code lens represents.
     */
    command?: Command;

    /**
     * A data entry field that is preserved on a code lens item between
     * a code lens and a code lens resolve request.
     */
    data?: any
}

    error: code and message set in case an exception happens during the code
           lens request.
-}

data CodeLensParams =
  CodeLensParams
    { _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeLensParams


-- -------------------------------------

data CodeLens =
  CodeLens
    { _range   :: Range
    , _command :: Maybe Command
    , _xdata   :: Maybe A.Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''CodeLens


type CodeLensRequest = RequestMessage ClientMethod CodeLensParams (List CodeLens)
type CodeLensResponse = ResponseMessage (List CodeLens)

-- -------------------------------------
{-
Registration Options: CodeLensRegistrationOptions defined as follows:

export interface CodeLensRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * Code lens has a resolve provider as well.
         */
        resolveProvider?: boolean;
}
-}

data CodeLensRegistrationOptions =
  CodeLensRegistrationOptions
    { _documentSelector :: Maybe DocumentSelector
    , _resolveProvider  :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeLensRegistrationOptions

-- ---------------------------------------------------------------------
{-
Code Lens Resolve Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#code-lens-resolve-request

The code lens resolve request is sent from the client to the server to resolve
the command for a given code lens item.

Request

    method: 'codeLens/resolve'
    params: CodeLens

Response

    result: CodeLens
    error: code and message set in case an exception happens during the code
           lens resolve request.


-}

type CodeLensResolveRequest  = RequestMessage ClientMethod CodeLens (List CodeLens)
type CodeLensResolveResponse = ResponseMessage (List CodeLens)

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Document Link Request

The document links request is sent from the client to the server to request the
location of links in a document.

Request:

    method: 'textDocument/documentLink'
    params: DocumentLinkParams, defined as follows

interface DocumentLinkParams {
        /**
         * The document to provide document links for.
         */
        textDocument: TextDocumentIdentifier;
}

Response:

    result: An array of DocumentLink, or null.

/**
 * A document link is a range in a text document that links to an internal or external resource, like another
 * text document or a web site.
 */
interface DocumentLink {
        /**
         * The range this link applies to.
         */
        range: Range;
        /**
         * The uri this link points to. If missing a resolve request is sent later.
         */
        target?: DocumentUri;
}

    error: code and message set in case an exception happens during the document link request.

Registration Options: DocumentLinkRegistrationOptions defined as follows:

export interface DocumentLinkRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * Document links have a resolve provider as well.
         */
        resolveProvider?: boolean;
}
-}

data DocumentLinkParams =
  DocumentLinkParams
    { _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentLinkParams

data DocumentLink =
  DocumentLink
    { _range  :: Range
    , _target :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentLink

type DocumentLinkRequest = RequestMessage ClientMethod DocumentLinkParams (List DocumentLink)
type DocumentLinkResponse = ResponseMessage (List DocumentLink)

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Document Link Resolve Request

The document link resolve request is sent from the client to the server to resolve the target of a given document link.

Request:

    method: 'documentLink/resolve'
    params: DocumentLink

Response:

    result: DocumentLink
    error: code and message set in case an exception happens during the document link resolve request.

-}

type DocumentLinkResolveRequest  = RequestMessage ClientMethod DocumentLink DocumentLink
type DocumentLinkResolveResponse = ResponseMessage DocumentLink

-- ---------------------------------------------------------------------
{-
Document Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-formatting-request

The document formatting request is sent from the server to the client to format
a whole document.

Request

    method: 'textDocument/formatting'
    params: DocumentFormattingParams defined as follows

interface DocumentFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The format options.
     */
    options: FormattingOptions;
}

/**
 * Value-object describing what options formatting should use.
 */
interface FormattingOptions {
    /**
     * Size of a tab in spaces.
     */
    tabSize: number;

    /**
     * Prefer spaces over tabs.
     */
    insertSpaces: boolean;

    /**
     * Signature for further properties.
     */
    [key: string]: boolean | number | string;
}

Response

    result: TextEdit[] describing the modification to the document to be
            formatted.
    error: code and message set in case an exception happens during the
           formatting request.

Registration Options: TextDocumentRegistrationOptions
-}

data FormattingOptions =
  FormattingOptions
    { _tabSize      :: Int
    , _insertSpaces :: Bool -- ^ Prefer spaces over tabs
    -- Note: May be more properties
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''FormattingOptions

data DocumentFormattingParams =
  DocumentFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _options      :: FormattingOptions
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DocumentFormattingParams

type DocumentFormattingRequest  = RequestMessage ClientMethod DocumentFormattingParams (List TextEdit)
type DocumentFormattingResponse = ResponseMessage (List TextEdit)

-- ---------------------------------------------------------------------
{-
Document Range Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-range-formatting-request

The document range formatting request is sent from the client to the server to
format a given range in a document.

Request

    method: 'textDocument/rangeFormatting',
    params: DocumentRangeFormattingParams defined as follows

interface DocumentRangeFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The range to format
     */
    range: Range;

    /**
     * The format options
     */
    options: FormattingOptions;
}

Response

    result: TextEdit[] describing the modification to the document to be
            formatted.
    error: code and message set in case an exception happens during the range
           formatting request.
-}

data DocumentRangeFormattingParams =
  DocumentRangeFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _range        :: Range
    , _options      :: FormattingOptions
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentRangeFormattingParams

type DocumentRangeFormattingRequest  = RequestMessage ClientMethod DocumentRangeFormattingParams (List TextEdit)
type DocumentRangeFormattingResponse = ResponseMessage (List TextEdit)

-- ---------------------------------------------------------------------
{-
Document on Type Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-on-type-formatting-request

The document on type formatting request is sent from the client to the server to
format parts of the document during typing.

Request

    method: 'textDocument/onTypeFormatting'
    params: DocumentOnTypeFormattingParams defined as follows

interface DocumentOnTypeFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position at which this request was sent.
     */
    position: Position;

    /**
     * The character that has been typed.
     */
    ch: string;

    /**
     * The format options.
     */
    options: FormattingOptions;
}

Response

    result: TextEdit[] describing the modification to the document.
    error: code and message set in case an exception happens during the range
           formatting request.

Registration Options: DocumentOnTypeFormattingRegistrationOptions defined as follows:

export interface DocumentOnTypeFormattingRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * A character on which formatting should be triggered, like `}`.
         */
        firstTriggerCharacter: string;
        /**
         * More trigger characters.
         */
        moreTriggerCharacter?: string[]
}
-}

data DocumentOnTypeFormattingParams =
  DocumentOnTypeFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _ch           :: Text
    , _options      :: FormattingOptions
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentOnTypeFormattingParams

type DocumentOnTypeFormattingRequest  = RequestMessage ClientMethod DocumentOnTypeFormattingParams (List TextEdit)
type DocumentOnTypeFormattingResponse = ResponseMessage (List TextEdit)

data DocumentOnTypeFormattingRegistrationOptions =
  DocumentOnTypeFormattingRegistrationOptions
    { _firstTriggerCharacter :: Text
    , _moreTriggerCharacter  :: Maybe (List String)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentOnTypeFormattingRegistrationOptions

-- ---------------------------------------------------------------------
{-
Rename Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#rename-request

The rename request is sent from the client to the server to perform a
workspace-wide rename of a symbol.

Request

    method: 'textDocument/rename'
    params: RenameParams defined as follows

interface RenameParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position at which this request was sent.
     */
    position: Position;

    /**
     * The new name of the symbol. If the given name is not valid the
     * request must return a [ResponseError](#ResponseError) with an
     * appropriate message set.
     */
    newName: string;
}

Response

    result: WorkspaceEdit describing the modification to the workspace.
    error: code and message set in case an exception happens during the rename
           request.

Registration Options: TextDocumentRegistrationOptions

-}
data RenameParams =
  RenameParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _newName      :: Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RenameParams


-- {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/alanz/mysrc/github/alanz/haskell-lsp/src/HieVscode.hs\"},\"position\":{\"line\":37,\"character\":17},\"newName\":\"getArgs'\"}}

type RenameRequest  = RequestMessage ClientMethod RenameParams WorkspaceEdit
type RenameResponse = ResponseMessage WorkspaceEdit

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Execute a command

The workspace/executeCommand request is sent from the client to the server to
trigger command execution on the server. In most cases the server creates a
WorkspaceEdit structure and applies the changes to the workspace using the
request workspace/applyEdit which is sent from the server to the client.

Request:

    method: 'workspace/executeCommand'
    params: ExecuteCommandParams defined as follows:

export interface ExecuteCommandParams {

        /**
         * The identifier of the actual command handler.
         */
        command: string;
        /**
         * Arguments that the command should be invoked with.
         */
        arguments?: any[];
}

The arguments are typically specified when a command is returned from the server
to the client. Example requests that return a command are
textDocument/codeAction or textDocument/codeLens.

Response:

    result: any
    error: code and message set in case an exception happens during the request.

Registration Options: ExecuteCommandRegistrationOptions defined as follows:

/**
 * Execute command registration options.
 */
export interface ExecuteCommandRegistrationOptions {
        /**
         * The commands to be executed on the server
         */
        commands: string[]
}
-}

data ExecuteCommandParams =
  ExecuteCommandParams
    { _command   :: Text
    , _arguments :: Maybe (List A.Value)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandParams

type ExecuteCommandRequest = RequestMessage ClientMethod ExecuteCommandParams A.Value
type ExecuteCommandResponse = ResponseMessage A.Value

data ExecuteCommandRegistrationOptions =
  ExecuteCommandRegistrationOptions
    { _commands :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandRegistrationOptions

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Applies a WorkspaceEdit

The workspace/applyEdit request is sent from the server to the client to modify
resource on the client side.

Request:

    method: 'workspace/applyEdit'
    params: ApplyWorkspaceEditParams defined as follows:

export interface ApplyWorkspaceEditParams {
        /**
         * The edits to apply.
         */
        edit: WorkspaceEdit;
}

Response:

    result: ApplyWorkspaceEditResponse defined as follows:

export interface ApplyWorkspaceEditResponse {
        /**
         * Indicates whether the edit was applied or not.
         */
        applied: boolean;
}

    error: code and message set in case an exception happens during the request.

-}
data ApplyWorkspaceEditParams =
  ApplyWorkspaceEditParams
    { _edit :: WorkspaceEdit
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditParams

data ApplyWorkspaceEditResponseBody =
  ApplyWorkspaceEditResponseBody
    { _applied :: Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditResponseBody

-- | Sent from the server to the client
type ApplyWorkspaceEditRequest  = RequestMessage ServerMethod ApplyWorkspaceEditParams ApplyWorkspaceEditResponseBody
type ApplyWorkspaceEditResponse = ResponseMessage ApplyWorkspaceEditResponseBody

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- ---------------------------------------------------------------------

data TraceParams =
  TraceParams {
    _value :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TraceParams


data TraceNotification =
  TraceNotification {
    _params :: TraceParams
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TraceNotification
