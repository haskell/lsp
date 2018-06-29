{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Haskell.LSP.TH.DataTypesJSON
    ( module Language.Haskell.LSP.TH.DataTypesJSON
    , module Language.Haskell.LSP.TH.CodeAction
    , module Language.Haskell.LSP.TH.Command
    , module Language.Haskell.LSP.TH.Diagnostic
    , module Language.Haskell.LSP.TH.List
    , module Language.Haskell.LSP.TH.Location
    , module Language.Haskell.LSP.TH.Message
    , module Language.Haskell.LSP.TH.Symbol
    , module Language.Haskell.LSP.TH.TextDocumentIdentifier
    , module Language.Haskell.LSP.TH.Uri
    , module Language.Haskell.LSP.TH.WorkspaceEdit
    ) where

import           Control.Applicative
import           Control.Lens.TH                            (makeFieldsNoPrefix)
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T

import           Language.Haskell.LSP.TH.ClientCapabilities
import           Language.Haskell.LSP.TH.CodeAction
import           Language.Haskell.LSP.TH.Command
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.Diagnostic
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Message
import           Language.Haskell.LSP.TH.Symbol
import           Language.Haskell.LSP.TH.TextDocumentIdentifier
import           Language.Haskell.LSP.TH.Utils
import           Language.Haskell.LSP.TH.Uri
import           Language.Haskell.LSP.TH.WorkspaceEdit

-- ---------------------------------------------------------------------


{-
TextDocumentItem

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentitem

    New: An item to transfer a text document from the client to the server.

interface TextDocumentItem {
    /**
     * The text document's URI.
     */
    uri: string;

    /**
     * The text document's language identifier.
     */
    languageId: string;

    /**
     * The version number of this document (it will strictly increase after each
     * change, including undo/redo).
     */
    version: number;

    /**
     * The content of the opened text document.
     */
    text: string;
}
-}

data TextDocumentItem =
  TextDocumentItem {
    _uri        :: Uri
  , _languageId :: Text
  , _version    :: Int
  , _text       :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentItem
makeFieldsNoPrefix ''TextDocumentItem

-- ---------------------------------------------------------------------
{-
TextDocumentPositionParams

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentpositionparams

    Changed: Was TextDocumentPosition in 1.0 with inlined parameters


interface TextDocumentPositionParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position inside the text document.
     */
    position: Position;
}

-}
data TextDocumentPositionParams =
  TextDocumentPositionParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentPositionParams
makeFieldsNoPrefix ''TextDocumentPositionParams

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

DocumentFilter
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#new-documentfilter

A document filter denotes a document through properties like language, schema or
pattern. Examples are a filter that applies to TypeScript files on disk or a
filter the applies to JSON files with name package.json:

    { language: 'typescript', scheme: 'file' }
    { language: 'json', pattern: '**/package.json' }

export interface DocumentFilter {
        /**
         * A language id, like `typescript`.
         */
        language?: string;

        /**
         * A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
         */
        scheme?: string;

        /**
         * A glob pattern, like `*.{ts,js}`.
         */
        pattern?: string;
}
-}
data DocumentFilter =
  DocumentFilter
    { _language :: Text
    , _scheme   :: Text
    , _pattern  :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentFilter
makeFieldsNoPrefix ''DocumentFilter

{-
A document selector is the combination of one or many document filters.

export type DocumentSelector = DocumentFilter[];
-}
type DocumentSelector = List DocumentFilter

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
  } deriving (Show, Read, Eq)


deriveJSON lspOptions ''InitializeParams
makeFieldsNoPrefix ''InitializeParams

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
makeFieldsNoPrefix ''InitializeError

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
makeFieldsNoPrefix ''CompletionOptions

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
makeFieldsNoPrefix ''SignatureHelpOptions

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
makeFieldsNoPrefix ''CodeLensOptions

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
makeFieldsNoPrefix ''DocumentOnTypeFormattingOptions

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
makeFieldsNoPrefix ''DocumentLinkOptions

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
makeFieldsNoPrefix ''ExecuteCommandOptions

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
makeFieldsNoPrefix ''SaveOptions

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
makeFieldsNoPrefix ''TextDocumentSyncOptions

-- ---------------------------------------------------------------------
{-

Extended in 3.0
---------------

interface ServerCapabilities {
        /**
         * Defines how text documents are synced. Is either a detailed structure defining each notification or
         * for backwards compatibility the TextDocumentSyncKind number.
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
         * The server provides code actions.
         */
        codeActionProvider?: boolean;
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
         * The server provides execute command support.
         */
        executeCommandProvider?: ExecuteCommandOptions;
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
      
data InitializeResponseCapabilitiesInner =
  InitializeResponseCapabilitiesInner
    { _textDocumentSync                 :: Maybe TDS
    , _hoverProvider                    :: Maybe Bool
    , _completionProvider               :: Maybe CompletionOptions
    , _signatureHelpProvider            :: Maybe SignatureHelpOptions
    , _definitionProvider               :: Maybe Bool
    , _referencesProvider               :: Maybe Bool
    , _documentHighlightProvider        :: Maybe Bool
    , _documentSymbolProvider           :: Maybe Bool
    , _workspaceSymbolProvider          :: Maybe Bool
    , _codeActionProvider               :: Maybe Bool
    , _codeLensProvider                 :: Maybe CodeLensOptions
    , _documentFormattingProvider       :: Maybe Bool
    , _documentRangeFormattingProvider  :: Maybe Bool
    , _documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
    , _renameProvider                   :: Maybe Bool
    -- Following are new in 3.0
    , _documentLinkProvider             :: Maybe DocumentLinkOptions
    , _executeCommandProvider           :: Maybe ExecuteCommandOptions
    , _experimental                     :: Maybe A.Value
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResponseCapabilitiesInner
makeFieldsNoPrefix ''InitializeResponseCapabilitiesInner

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    _capabilities :: InitializeResponseCapabilitiesInner
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResponseCapabilities
makeFieldsNoPrefix ''InitializeResponseCapabilities

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
ShowMessage Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#showmessage-notification

The show message notification is sent from a server to a client to ask the
client to display a particular message in the user interface.

Notification:

    method: 'window/showMessage'
    params: ShowMessageParams defined as follows:

interface ShowMessageParams {
    /**
     * The message type. See {@link MessageType}.
     */
    type: number;

    /**
     * The actual message.
     */
    message: string;
}

Where the type is defined as follows:

enum MessageType {
    /**
     * An error message.
     */
    Error = 1,
    /**
     * A warning message.
     */
    Warning = 2,
    /**
     * An information message.
     */
    Info = 3,
    /**
     * A log message.
     */
    Log = 4
}
-}
data MessageType = MtError   -- ^ Error = 1,
                 | MtWarning -- ^ Warning = 2,
                 | MtInfo    -- ^ Info = 3,
                 | MtLog     -- ^ Log = 4
        deriving (Eq,Ord,Show,Read,Enum)

instance A.ToJSON MessageType where
  toJSON MtError   = A.Number 1
  toJSON MtWarning = A.Number 2
  toJSON MtInfo    = A.Number 3
  toJSON MtLog     = A.Number 4

instance A.FromJSON MessageType where
  parseJSON (A.Number 1) = pure MtError
  parseJSON (A.Number 2) = pure MtWarning
  parseJSON (A.Number 3) = pure MtInfo
  parseJSON (A.Number 4) = pure MtLog
  parseJSON _            = mempty

-- ---------------------------------------


data ShowMessageParams =
  ShowMessageParams {
    _xtype   :: MessageType
  , _message :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ShowMessageParams
makeFieldsNoPrefix ''ShowMessageParams

type ShowMessageNotification = NotificationMessage ServerMethod ShowMessageParams

-- ---------------------------------------------------------------------
{-
ShowMessage Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#showmessage-request

    New: The show message request is sent from a server to a client to ask the
    client to display a particular message in the user interface. In addition to
    the show message notification the request allows to pass actions and to wait
    for an answer from the client.

Request:

    method: 'window/showMessageRequest'
    params: ShowMessageRequestParams defined as follows:

Response:

    result: the selected MessageActionItem
    error: code and message set in case an exception happens during showing a message.

interface ShowMessageRequestParams {
    /**
     * The message type. See {@link MessageType}
     */
    type: number;

    /**
     * The actual message
     */
    message: string;

    /**
     * The message action items to present.
     */
    actions?: MessageActionItem[];
}

Where the MessageActionItem is defined as follows:

interface MessageActionItem {
    /**
     * A short title like 'Retry', 'Open Log' etc.
     */
    title: string;
}
-}

data MessageActionItem =
  MessageActionItem
    { _title :: Text
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''MessageActionItem
makeFieldsNoPrefix ''MessageActionItem


data ShowMessageRequestParams =
  ShowMessageRequestParams
    { _xtype   :: MessageType
    , _message :: Text
    , _actions :: Maybe [MessageActionItem]
    } deriving (Show,Read,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ShowMessageRequestParams
makeFieldsNoPrefix ''ShowMessageRequestParams

type ShowMessageRequest = RequestMessage ServerMethod ShowMessageRequestParams Text
type ShowMessageResponse = ResponseMessage Text

-- ---------------------------------------------------------------------
{-
LogMessage Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#logmessage-notification

The log message notification is sent from the server to the client to ask the
client to log a particular message.

Notification:

    method: 'window/logMessage'
    params: LogMessageParams defined as follows:

interface LogMessageParams {
    /**
     * The message type. See {@link MessageType}
     */
    type: number;

    /**
     * The actual message
     */
    message: string;
}

Where type is defined as above.
-}

data LogMessageParams =
  LogMessageParams {
    _xtype   :: MessageType
  , _message :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''LogMessageParams
makeFieldsNoPrefix ''LogMessageParams


type LogMessageNotification = NotificationMessage ServerMethod LogMessageParams

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
makeFieldsNoPrefix ''Registration

data RegistrationParams =
  RegistrationParams
    { _registrations :: List Registration
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RegistrationParams
makeFieldsNoPrefix ''RegistrationParams

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
makeFieldsNoPrefix ''TextDocumentRegistrationOptions

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
makeFieldsNoPrefix ''Unregistration

data UnregistrationParams =
  UnregistrationParams
    { _unregistrations :: List Unregistration
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''UnregistrationParams
makeFieldsNoPrefix ''UnregistrationParams

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
makeFieldsNoPrefix ''DidChangeConfigurationParams


type DidChangeConfigurationNotification = NotificationMessage ClientMethod DidChangeConfigurationParams

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
makeFieldsNoPrefix ''DidOpenTextDocumentParams

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
makeFieldsNoPrefix ''TextDocumentContentChangeEvent

-- -------------------------------------

data DidChangeTextDocumentParams =
  DidChangeTextDocumentParams
    { _textDocument   :: VersionedTextDocumentIdentifier
    , _contentChanges :: List TextDocumentContentChangeEvent
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DidChangeTextDocumentParams
makeFieldsNoPrefix ''DidChangeTextDocumentParams

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
makeFieldsNoPrefix ''TextDocumentChangeRegistrationOptions

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
makeFieldsNoPrefix ''WillSaveTextDocumentParams

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
makeFieldsNoPrefix ''DidSaveTextDocumentParams

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
makeFieldsNoPrefix ''DidCloseTextDocumentParams


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
makeFieldsNoPrefix ''FileEvent

data DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams
    { _changes :: List FileEvent
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidChangeWatchedFilesParams
makeFieldsNoPrefix ''DidChangeWatchedFilesParams


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
makeFieldsNoPrefix ''PublishDiagnosticsParams


type PublishDiagnosticsNotification = NotificationMessage ServerMethod PublishDiagnosticsParams

-- ---------------------------------------------------------------------
{-
Completion Request

The Completion request is sent from the client to the server to compute
completion items at a given cursor position. Completion items are presented in
the IntelliSense user interface. If computing full completion items is
expensive, servers can additionally provide a handler for the completion item
resolve request ('completionItem/resolve'). This request is sent when a
completion item is selected in the user interface. A typically use case is for
example: the 'textDocument/completion' request doesn't fill in the documentation
property for returned completion items since it is expensive to compute. When
the item is selected in the user interface then a 'completionItem/resolve'
request is sent with the selected completion item as a param. The returned
completion item should have the documentation property filled in.

    Changed: In 2.0 the request uses TextDocumentPositionParams with a proper
    textDocument and position property. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/completion'
    params: TextDocumentPositionParams
-}

-- -------------------------------------

{-

Response

    result: CompletionItem[] | CompletionList

/**
 * Represents a collection of [completion items](#CompletionItem) to be presented
 * in the editor.
 */
interface CompletionList {
    /**
     * This list it not complete. Further typing should result in recomputing
     * this list.
     */
    isIncomplete: boolean;
    /**
     * The completion items.
     */
    items: CompletionItem[];
}


New in 3.0 : InsertTextFormat

/**
 * Defines whether the insert text in a completion item should be interpreted as
 * plain text or a snippet.
 */
namespace InsertTextFormat {
        /**
         * The primary text to be inserted is treated as a plain string.
         */
        export const PlainText = 1;

        /**
         * The primary text to be inserted is treated as a snippet.
         *
         * A snippet can define tab stops and placeholders with `$1`, `$2`
         * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
         * the end of the snippet. Placeholders with equal identifiers are linked,
         * that is typing in one will update others too.
         *
         * See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
         */
        export const Snippet = 2;
}



interface CompletionItem {
    /**
     * The label of this completion item. By default
     * also the text that is inserted when selecting
     * this completion.
     */
    label: string;
    /**
     * The kind of this completion item. Based of the kind
     * an icon is chosen by the editor.
     */
    kind?: number;
    /**
     * A human-readable string with additional information
     * about this item, like type or symbol information.
     */
    detail?: string;
    /**
     * A human-readable string that represents a doc-comment.
     */
    documentation?: string;
    /**
     * A string that shoud be used when comparing this item
     * with other items. When `falsy` the label is used.
     */
    sortText?: string;
    /**
     * A string that should be used when filtering a set of
     * completion items. When `falsy` the label is used.
     */
    filterText?: string;
    /**
     * A string that should be inserted a document when selecting
     * this completion. When `falsy` the label is used.
     */
    insertText?: string;
    -- Following field is new in 3.0
        /**
         * The format of the insert text. The format applies to both the `insertText` property
         * and the `newText` property of a provided `textEdit`.
         */
    insertTextFormat?: InsertTextFormat;
        /**
         * An edit which is applied to a document when selecting this completion. When an edit is provided the value of
         * `insertText` is ignored.
         *
         * *Note:* The range of the edit must be a single line range and it must contain the position at which completion
         * has been requested.
         */

    textEdit?: TextEdit;

    -- Following field is new in 3.0
        /**
         * An optional array of additional text edits that are applied when
         * selecting this completion. Edits must not overlap with the main edit
         * nor with themselves.
         */
    additionalTextEdits?: TextEdit[];
    -- Following field is new in 3.0
        /**
         * An optional command that is executed *after* inserting this completion. *Note* that
         * additional modifications to the current document should be described with the
         * additionalTextEdits-property.
         */

    command?: Command;
        /**
         * An data entry field that is preserved on a completion item between
         * a completion and a completion resolve request.
         */

    data?: any
}

Where CompletionItemKind is defined as follows:

/**
 * The kind of a completion entry.
 */
enum CompletionItemKind {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18
}

    error: code and message set in case an exception happens during the completion request.
-}

-- -------------------------------------

data InsertTextFormat
  = PlainText -- ^The primary text to be inserted is treated as a plain string.
  | Snippet
      -- ^ The primary text to be inserted is treated as a snippet.
      --
      -- A snippet can define tab stops and placeholders with `$1`, `$2`
      -- and `${3:foo}`. `$0` defines the final tab stop, it defaults to
      -- the end of the snippet. Placeholders with equal identifiers are linked,
      -- that is typing in one will update others too.
      --
      -- See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
    deriving (Show, Read, Eq)

instance A.ToJSON InsertTextFormat where
  toJSON PlainText = A.Number 1
  toJSON Snippet   = A.Number 2

instance A.FromJSON InsertTextFormat where
  parseJSON (A.Number  1) = pure PlainText
  parseJSON (A.Number  2) = pure Snippet
  parseJSON _             = mempty

-- -------------------------------------

data CompletionItemKind = CiText
                        | CiMethod
                        | CiFunction
                        | CiConstructor
                        | CiField
                        | CiVariable
                        | CiClass
                        | CiInterface
                        | CiModule
                        | CiProperty
                        | CiUnit
                        | CiValue
                        | CiEnum
                        | CiKeyword
                        | CiSnippet
                        | CiColor
                        | CiFile
                        | CiReference
         deriving (Read,Show,Eq,Ord)

instance A.ToJSON CompletionItemKind where
  toJSON CiText        = A.Number 1
  toJSON CiMethod      = A.Number 2
  toJSON CiFunction    = A.Number 3
  toJSON CiConstructor = A.Number 4
  toJSON CiField       = A.Number 5
  toJSON CiVariable    = A.Number 6
  toJSON CiClass       = A.Number 7
  toJSON CiInterface   = A.Number 8
  toJSON CiModule      = A.Number 9
  toJSON CiProperty    = A.Number 10
  toJSON CiUnit        = A.Number 11
  toJSON CiValue       = A.Number 12
  toJSON CiEnum        = A.Number 13
  toJSON CiKeyword     = A.Number 14
  toJSON CiSnippet     = A.Number 15
  toJSON CiColor       = A.Number 16
  toJSON CiFile        = A.Number 17
  toJSON CiReference   = A.Number 18

instance A.FromJSON CompletionItemKind where
  parseJSON (A.Number  1) = pure CiText
  parseJSON (A.Number  2) = pure CiMethod
  parseJSON (A.Number  3) = pure CiFunction
  parseJSON (A.Number  4) = pure CiConstructor
  parseJSON (A.Number  5) = pure CiField
  parseJSON (A.Number  6) = pure CiVariable
  parseJSON (A.Number  7) = pure CiClass
  parseJSON (A.Number  8) = pure CiInterface
  parseJSON (A.Number  9) = pure CiModule
  parseJSON (A.Number 10) = pure CiProperty
  parseJSON (A.Number 11) = pure CiUnit
  parseJSON (A.Number 12) = pure CiValue
  parseJSON (A.Number 13) = pure CiEnum
  parseJSON (A.Number 14) = pure CiKeyword
  parseJSON (A.Number 15) = pure CiSnippet
  parseJSON (A.Number 16) = pure CiColor
  parseJSON (A.Number 17) = pure CiFile
  parseJSON (A.Number 18) = pure CiReference
  parseJSON _             = mempty


-- -------------------------------------

data CompletionItem =
  CompletionItem
    { _label               :: Text -- ^ The label of this completion item. By default also
                       -- the text that is inserted when selecting this
                       -- completion.
    , _kind                :: Maybe CompletionItemKind
    , _detail              :: Maybe Text -- ^ A human-readable string with additional
                              -- information about this item, like type or
                              -- symbol information.
    , _documentation       :: Maybe Text -- ^ A human-readable string that represents
                                    -- a doc-comment.
    , _sortText            :: Maybe Text -- ^ A string that should be used when filtering
                                -- a set of completion items. When `falsy` the
                                -- label is used.
    , _filterText          :: Maybe Text -- ^ A string that should be used when
                                  -- filtering a set of completion items. When
                                  -- `falsy` the label is used.
    , _insertText          :: Maybe Text -- ^ A string that should be inserted a
                                  -- document when selecting this completion.
                                  -- When `falsy` the label is used.
    , _insertTextFormat    :: Maybe InsertTextFormat
         -- ^ The format of the insert text. The format applies to both the
         -- `insertText` property and the `newText` property of a provided
         -- `textEdit`.
    , _textEdit            :: Maybe TextEdit
         -- ^ An edit which is applied to a document when selecting this
         -- completion. When an edit is provided the value of `insertText` is
         -- ignored.
         --
         -- *Note:* The range of the edit must be a single line range and it
         -- must contain the position at which completion has been requested.
    , _additionalTextEdits :: Maybe (List TextEdit)
         -- ^ An optional array of additional text edits that are applied when
         -- selecting this completion. Edits must not overlap with the main edit
         -- nor with themselves.
    , _command             :: Maybe Command
        -- ^ An optional command that is executed *after* inserting this
        -- completion. *Note* that additional modifications to the current
        -- document should be described with the additionalTextEdits-property.
    , _xdata               :: Maybe A.Value -- ^ An data entry field that is preserved on a
                              -- completion item between a completion and a
                              -- completion resolve request.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''CompletionItem
makeFieldsNoPrefix ''CompletionItem

data CompletionListType =
  CompletionListType
    { _isIncomplete :: Bool
    , _items        :: List CompletionItem
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CompletionListType
makeFieldsNoPrefix ''CompletionListType


data CompletionResponseResult
  = CompletionList CompletionListType
  | Completions (List CompletionItem)
  deriving (Read,Show,Eq)

deriveJSON defaultOptions { fieldLabelModifier = rdrop (length ("CompletionResponseResult"::String)), sumEncoding = UntaggedValue } ''CompletionResponseResult

type CompletionResponse = ResponseMessage CompletionResponseResult
type CompletionRequest = RequestMessage ClientMethod TextDocumentPositionParams CompletionResponseResult

-- -------------------------------------
{-
New in 3.0
-----------
Registration Options: CompletionRegistrationOptions options defined as follows:

export interface CompletionRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * The characters that trigger completion automatically.
         */
        triggerCharacters?: string[];

        /**
         * The server provides support to resolve additional
         * information for a completion item.
         */
        resolveProvider?: boolean;
}
-}

data CompletionRegistrationOptions =
  CompletionRegistrationOptions
    { _documentSelector  :: Maybe DocumentSelector
    , _triggerCharacters :: Maybe (List String)
    , _resolveProvider   :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionRegistrationOptions
makeFieldsNoPrefix ''CompletionRegistrationOptions

-- ---------------------------------------------------------------------
{-
Completion Item Resolve Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#completion-item-resolve-request

The request is sent from the client to the server to resolve additional
information for a given completion item.

Request

    method: 'completionItem/resolve'
    params: CompletionItem

Response

    result: CompletionItem
    error: code and message set in case an exception happens during the completion resolve request.
-}

type CompletionItemResolveRequest  = RequestMessage ClientMethod CompletionItem CompletionItem
type CompletionItemResolveResponse = ResponseMessage CompletionItem

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

    result: Hover defined as follows:

/**
 * The result of a hove request.
 */
interface Hover {
    /**
     * The hover's content
     */
    contents: MarkedString | MarkedString[];

    /**
     * An optional range
     */
    range?: Range;
}

Where MarkedString is defined as follows:
/**
 * MarkedString can be used to render human readable text. It is either a markdown string
 * or a code-block that provides a language and a code snippet. The language identifier
 * is sematically equal to the optional language identifier in fenced code blocks in GitHub
 * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
 *
 * The pair of a language and a value is an equivalent to markdown:
 * ```${language}
 * ${value}
 * ```
 *
 * Note that markdown strings will be sanitized - that means html will be escaped.
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
makeFieldsNoPrefix ''LanguageString

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

data Hover =
  Hover
    { _contents :: List MarkedString
    , _range    :: Maybe Range
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''Hover
makeFieldsNoPrefix ''Hover


type HoverRequest = RequestMessage ClientMethod TextDocumentPositionParams Hover
type HoverResponse = ResponseMessage Hover

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
makeFieldsNoPrefix ''ParameterInformation


-- -------------------------------------

data SignatureInformation =
  SignatureInformation
    { _label         :: Text
    , _documentation :: Maybe Text
    , _parameters    :: Maybe [ParameterInformation]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureInformation
makeFieldsNoPrefix ''SignatureInformation

data SignatureHelp =
  SignatureHelp
    { _signatures      :: List SignatureInformation
    , _activeSignature :: Maybe Int -- ^ The active signature
    , _activeParameter :: Maybe Int -- ^ The active parameter of the active signature
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SignatureHelp
makeFieldsNoPrefix ''SignatureHelp

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
makeFieldsNoPrefix ''SignatureHelpRegistrationOptions

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

data DefinitionResponseParams = SingleLoc Location | MultiLoc [Location]
  deriving (Eq,Read,Show)

instance A.ToJSON DefinitionResponseParams where
  toJSON (SingleLoc x) = toJSON x
  toJSON (MultiLoc xs) = toJSON xs

instance A.FromJSON DefinitionResponseParams where
  parseJSON xs@(A.Array _) = MultiLoc <$> parseJSON xs
  parseJSON x              = SingleLoc <$> parseJSON x

type DefinitionRequest  = RequestMessage ClientMethod TextDocumentPositionParams DefinitionResponseParams
type DefinitionResponse = ResponseMessage DefinitionResponseParams

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
makeFieldsNoPrefix ''ReferenceContext


data ReferenceParams =
  ReferenceParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _context      :: ReferenceContext
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ReferenceParams
makeFieldsNoPrefix ''ReferenceParams


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
makeFieldsNoPrefix ''DocumentHighlight

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
makeFieldsNoPrefix ''WorkspaceSymbolParams

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
makeFieldsNoPrefix ''CodeLensParams


-- -------------------------------------

data CodeLens =
  CodeLens
    { _range   :: Range
    , _command :: Maybe Command
    , _xdata   :: Maybe A.Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''CodeLens
makeFieldsNoPrefix ''CodeLens


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
makeFieldsNoPrefix ''CodeLensRegistrationOptions

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
makeFieldsNoPrefix ''DocumentLinkParams

data DocumentLink =
  DocumentLink
    { _range  :: Range
    , _target :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentLink
makeFieldsNoPrefix ''DocumentLink

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
makeFieldsNoPrefix ''FormattingOptions

data DocumentFormattingParams =
  DocumentFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _options      :: FormattingOptions
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DocumentFormattingParams
makeFieldsNoPrefix ''DocumentFormattingParams

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
makeFieldsNoPrefix ''DocumentRangeFormattingParams

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
makeFieldsNoPrefix ''DocumentOnTypeFormattingParams

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
makeFieldsNoPrefix ''RenameParams


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
makeFieldsNoPrefix ''ExecuteCommandParams

type ExecuteCommandRequest = RequestMessage ClientMethod ExecuteCommandParams A.Value
type ExecuteCommandResponse = ResponseMessage A.Value

data ExecuteCommandRegistrationOptions =
  ExecuteCommandRegistrationOptions
    { _commands :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandRegistrationOptions
makeFieldsNoPrefix ''ExecuteCommandRegistrationOptions

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
makeFieldsNoPrefix ''ApplyWorkspaceEditParams

data ApplyWorkspaceEditResponseBody =
  ApplyWorkspaceEditResponseBody
    { _applied :: Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditResponseBody
makeFieldsNoPrefix ''ApplyWorkspaceEditResponseBody

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
makeFieldsNoPrefix ''TraceParams


data TraceNotification =
  TraceNotification {
    _params :: TraceParams
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TraceNotification
makeFieldsNoPrefix ''TraceNotification



-- ---------------------------------------------------------------------

-- Generating lenses
-- Need to do this all in one file or else we get duplicate functions

-- Location
makeFieldsNoPrefix ''Position
makeFieldsNoPrefix ''Range
makeFieldsNoPrefix ''Location

-- CodeActions
makeFieldsNoPrefix ''CodeActionContext
makeFieldsNoPrefix ''CodeActionParams
makeFieldsNoPrefix ''CodeAction

-- WorkspaceEdit
makeFieldsNoPrefix ''TextEdit
makeFieldsNoPrefix ''VersionedTextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentEdit
makeFieldsNoPrefix ''WorkspaceEdit

-- Message
makeFieldsNoPrefix ''RequestMessage
makeFieldsNoPrefix ''ResponseError
makeFieldsNoPrefix ''ResponseMessage
makeFieldsNoPrefix ''NotificationMessage
makeFieldsNoPrefix ''CancelParams

-- TextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentIdentifier

-- Command
makeFieldsNoPrefix ''Command

-- Diagnostic
makeFieldsNoPrefix ''Diagnostic
makeFieldsNoPrefix ''DiagnosticRelatedInformation

-- Symbol
makeFieldsNoPrefix ''DocumentSymbolParams
makeFieldsNoPrefix ''SymbolInformation