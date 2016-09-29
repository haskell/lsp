{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.LSP.TH.DataTypesJSON where

import Data.Aeson.TH
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Data.Monoid
import Language.Haskell.LSP.Utility
import Data.Default

-- ---------------------------------------------------------------------
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#position

Position in a text document expressed as zero-based line and character offset. A
position is between two characters like an 'insert' cursor in a editor.

interface Position {
    /**
     * Line position in a document (zero-based).
     */
    line: number;

    /**
     * Character offset on a line in a document (zero-based).
     */
    character: number;
}
-}
data Position =
  Position
    { linePosition       :: Int
    , characterPosition  :: Int
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Position") } ''Position)

instance Default Position where
  def = Position def def

-- ---------------------------------------------------------------------
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#range

A range in a text document expressed as (zero-based) start and end positions. A
range is comparable to a selection in an editor. Therefore the end position is
exclusive.

interface Range {
    /**
     * The range's start position.
     */
    start: Position;

    /**
     * The range's end position.
     */
    end: Position;
}
-}

data Range =
  Range
    { startRange :: Position
    , endRange   :: Position
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Range") } ''Range)

instance Default Range where
  def = Range def def

-- ---------------------------------------------------------------------
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#location

Represents a location inside a resource, such as a line inside a text file.

interface Location {
    uri: string;
    range: Range;
}
-}

data Location =
  Location
    { uriLocation   :: String
    , rangeLocation :: Range
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Location") } ''Location)

instance Default Location where
  def = Location def def

-- ---------------------------------------------------------------------
{-
The protocol currently supports the following diagnostic severities:

enum DiagnosticSeverity {
    /**
     * Reports an error.
     */
    Error = 1,
    /**
     * Reports a warning.
     */
    Warning = 2,
    /**
     * Reports an information.
     */
    Information = 3,
    /**
     * Reports a hint.
     */
    Hint = 4
}
-}
data DiagnosticSeverity
  = DsError   -- ^ Error = 1,
  | DsWarning -- ^ Warning = 2,
  | DsInfo    -- ^ Info = 3,
  | DsHint    -- ^ Hint = 4
  deriving (Eq,Ord,Show,Read)

instance A.ToJSON DiagnosticSeverity where
  toJSON DsError   = A.Number 1
  toJSON DsWarning = A.Number 2
  toJSON DsInfo    = A.Number 3
  toJSON DsHint    = A.Number 4

instance A.FromJSON DiagnosticSeverity where
  parseJSON (A.Number 1) = pure DsError
  parseJSON (A.Number 2) = pure DsWarning
  parseJSON (A.Number 3) = pure DsInfo
  parseJSON (A.Number 4) = pure DsHint
  parseJSON _            = mempty

instance Default DiagnosticSeverity where
  def = DsError

-- ---------------------------------------------------------------------
{-
Diagnostic

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#diagnostic

Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
are only valid in the scope of a resource.

interface Diagnostic {
    /**
     * The range at which the message applies.
     */
    range: Range;

    /**
     * The diagnostic's severity. Can be omitted. If omitted it is up to the
     * client to interpret diagnostics as error, warning, info or hint.
     */
    severity?: number;

    /**
     * The diagnostic's code. Can be omitted.
     */
    code?: number | string;

    /**
     * A human-readable string describing the source of this
     * diagnostic, e.g. 'typescript' or 'super lint'.
     */
    source?: string;

    /**
     * The diagnostic's message.
     */
    message: string;
}
-}
data Diagnostic =
  Diagnostic
    {  rangeDiagnostic   :: Range
    , severityDiagnostic :: DiagnosticSeverity
    , codeDiagnostic     :: Int
    , sourceDiagnostic   :: String
    , messageDiagnostic  :: String
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Diagnostic") } ''Diagnostic)

instance Default Diagnostic where
  def = Diagnostic def def 0 "" ""

-- ---------------------------------------------------------------------
{-
Command

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#command

Represents a reference to a command. Provides a title which will be used to
represent a command in the UI. Commands are identitifed using a string
identifier and the protocol currently doesn't specify a set of well known
commands. So executing a command requires some tool extension code.

interface Command {
    /**
     * Title of the command, like `save`.
     */
    title: string;
    /**
     * The identifier of the actual command handler.
     */
    command: string;
    /**
     * Arguments that the command handler should be
     * invoked with.
     */
    arguments?: any[];
}
-}

data Command =
  Command
    { titleCommand     :: String
    , commandCommand   :: String
    , argumentsCommand :: Maybe A.Object
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "Command") } ''Command)

instance Default Command where
  def = Command "" "" Nothing

-- ---------------------------------------------------------------------
{-
TextEdit

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textedit

A textual edit applicable to a text document.

interface TextEdit {
    /**
     * The range of the text document to be manipulated. To insert
     * text into a document create a range where start === end.
     */
    range: Range;

    /**
     * The string to be inserted. For delete operations use an
     * empty string.
     */
    newText: string;
}
-}

data TextEdit =
  TextEdit
    { rangeTextEdit   :: Range
    , newTextTextEdit :: String
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TextEdit") } ''TextEdit)

instance Default TextEdit where
  def = TextEdit def def

-- ---------------------------------------------------------------------
{-
WorkspaceEdit

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#workspaceedit

A workspace edit represents changes to many resources managed in the workspace.

interface WorkspaceEdit {
    /**
     * Holds changes to existing resources.
     */
    changes: { [uri: string]: TextEdit[]; };
}
-}

type WorkspaceEditMap = H.HashMap T.Text [TextEdit]

instance Default (H.HashMap T.Text [TextEdit]) where
  def = mempty

data WorkspaceEdit =
  WorkspaceEdit
    { changesWorkspaceEdit :: WorkspaceEditMap
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "WorkspaceEdit") } ''WorkspaceEdit)

instance Default WorkspaceEdit where
  def = WorkspaceEdit def

-- ---------------------------------------------------------------------
{-
TextDocumentIdentifier

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentidentifier

Text documents are identified using a URI. On the protocol level, URIs are
passed as strings. The corresponding JSON structure looks like this:

interface TextDocumentIdentifier {
    /**
     * The text document's URI.
     */
    uri: string;
}
-}
data TextDocumentIdentifier =
  TextDocumentIdentifier
    { uriTextDocumentIdentifier :: String
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TextDocumentIdentifier") } ''TextDocumentIdentifier)

instance Default TextDocumentIdentifier where
  def = TextDocumentIdentifier def

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
    uriTextDocumentItem        :: String
  , languageIdTextDocumentItem :: String
  , versionTextDocumentItem    :: Int
  , textTextDocumentItem       :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TextDocumentItem") } ''TextDocumentItem)

-- ---------------------------------------------------------------------
{-
VersionedTextDocumentIdentifier

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#versionedtextdocumentidentifier

    New: An identifier to denote a specific version of a text document.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    /**
     * The version number of this document.
     */
    version: number;
-}
data VersionedTextDocumentIdentifier =
  VersionedTextDocumentIdentifier
    { uriVersionedTextDocumentIdentifier     :: String
    , versionVersionedTextDocumentIdentifier :: Int
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "VersionedTextDocumentIdentifier") } ''VersionedTextDocumentIdentifier)

instance Default VersionedTextDocumentIdentifier where
  def = VersionedTextDocumentIdentifier def def

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
    { textDocumentTextDocumentPositionParams :: TextDocumentIdentifier
    , positionTextDocumentPositionParams     :: Position
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TextDocumentPositionParams") } ''TextDocumentPositionParams)

instance Default TextDocumentPositionParams where
  def = TextDocumentPositionParams def def


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
     * the server.
     */
    processId: number;

    /**
     * The rootPath of the workspace. Is null
     * if no folder is open.
     */
    rootPath: string;

    /**
     * User provided initialization options.
     */
    initializationOptions?: any;

    /**
     * The capabilities provided by the client (editor)
     */
    capabilities: ClientCapabilities;
}
-}

data InitializeRequestArguments =
  InitializeRequestArguments {
    processIdInitializeRequestArguments    :: Int
  , rootPathInitializeRequestArguments     :: Maybe String
  , capabilitiesInitializeRequestArguments :: A.Object -- None currently defined, but empty object sent
  , initializationOptionsInitializeRequestArguments :: Maybe A.Object
  , traceInitializeRequestArguments        :: String -- observed to be present in the wild
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "InitializeRequestArguments") } ''InitializeRequestArguments)

instance Default InitializeRequestArguments where
  def = InitializeRequestArguments 0 mempty mempty mempty mempty

-- ---------------------------------------------------------------------

-- {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":1749,"capabilities":{},"trace":"off"}}
-- {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":17554,"rootPath":"/home/alanz/mysrc/github/alanz/haskell-lsp","capabilities":{},"trace":"off"}}
data InitializeRequest =
  InitializeRequest {
    idInitializeRequest       :: Int                         -- Sequence number
  , paramsInitializeRequest   :: InitializeRequestArguments  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequest") } ''InitializeRequest)

instance Default InitializeRequest where
  def = InitializeRequest 0 def


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
    { retryInitializeError :: Bool
    } deriving (Read, Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeError") } ''InitializeError)

instance Default InitializeError where
  def = InitializeError False

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
    { resolveProvider   :: Maybe Bool
    , triggerCharacters :: Maybe [String]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions {omitNothingFields = True } ''CompletionOptions)

instance Default CompletionOptions where
  def = CompletionOptions Nothing mempty

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
    { triggerCharactersSHO :: Maybe [String]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "SHO") } ''SignatureHelpOptions)

instance Default SignatureHelpOptions where
  def = SignatureHelpOptions mempty

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
    { resolveProviderCLO :: Maybe Bool
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "CLO") } ''CodeLensOptions)

instance Default CodeLensOptions where
  def = CodeLensOptions Nothing

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
    { firstTriggerCharacter :: String
    , moreTriggerCharacter :: Maybe [String]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions {omitNothingFields = True } ''DocumentOnTypeFormattingOptions)

instance Default DocumentOnTypeFormattingOptions where
  def = DocumentOnTypeFormattingOptions mempty mempty

-- ---------------------------------------------------------------------
{-
interface ServerCapabilities {
    /**
     * Defines how text documents are synced.
     */
    textDocumentSync?: number;
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
    renameProvider?: boolean
}
-}

data InitializeResponseCapabilitiesInner =
  InitializeResponseCapabilitiesInner
    { textDocumentSync                 :: Maybe TextDocumentSyncKind
    , hoverProvider                    :: Maybe Bool
    , completionProvider               :: Maybe CompletionOptions
    , signatureHelpProvider            :: Maybe SignatureHelpOptions
    , definitionProvider               :: Maybe Bool
    , referencesProvider               :: Maybe Bool
    , documentHighlightProvider        :: Maybe Bool
    , documentSymbolProvider           :: Maybe Bool
    , workspaceSymbolProvider          :: Maybe Bool
    , codeActionProvider               :: Maybe Bool
    , codeLensProvider                 :: Maybe CodeLensOptions
    , documentFormattingProvider       :: Maybe Bool
    , documentRangeFormattingProvider  :: Maybe Bool
    , documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
    , renameProvider                   :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { omitNothingFields = True }  ''InitializeResponseCapabilitiesInner)

instance Default InitializeResponseCapabilitiesInner where
  def = InitializeResponseCapabilitiesInner def def def def def def def def def def def def def def def

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    capabilitiesInitializeResponseCapabilities :: InitializeResponseCapabilitiesInner
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilities") } ''InitializeResponseCapabilities)

instance Default InitializeResponseCapabilities where
  def = InitializeResponseCapabilities def

-- ---------------------------------------------------------------------

-- |
--   Server-initiated response to client request
--
data InitializeResponse =
  InitializeResponse {
    jsonrpcInitializeResponse    :: String
  , idInitializeResponse         :: Int     -- Sequence number
  , resultInitializeResponse     :: InitializeResponseCapabilities  -- The capabilities of this debug adapter
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponse") } ''InitializeResponse)


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

data ShutdownRequest =
  ShutdownRequest {
    idShutdownRequest :: Int
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownRequest") } ''ShutdownRequest)

instance Default ShutdownRequest where
  def = ShutdownRequest 0

data ShutdownResponse =
  ShutdownResponse {
    jsonrpcShutdownResponse    :: String
  , idShutdownResponse         :: Int     -- Sequence number
  , resultShutdownResponse     :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownResponse") } ''ShutdownResponse)

instance Default ShutdownResponse where
  def = ShutdownResponse "2.0" 0 ""

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
data ExitNotification =
  ExitNotification
    {
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExitNotification") } ''ExitNotification)

instance Default ExitNotification where
  def = ExitNotification

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

instance Default MessageType where
  def = MtWarning -- Pick something arbitrary

-- ---------------------------------------

data MessageNotificationParams =
  MessageNotificationParams {
    typeMessageNotificationParams    :: MessageType
  , messageMessageNotificationParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "MessageNotificationParams") } ''MessageNotificationParams)

instance Default MessageNotificationParams where
  def = MessageNotificationParams MtWarning ""

-- ---------------------------------------

data MessageNotification =
  MessageNotification {
    jsonrpcMessageNotification :: String
  , methodMessageNotification  :: String
  , paramsMessageNotification  :: MessageNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "MessageNotification") } ''MessageNotification)

defShowMessage :: MessageType -> String -> MessageNotification
defShowMessage mt msg = MessageNotification "2.0" "window/showMessage" (MessageNotificationParams mt msg)

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
    { titleMessageActionItem :: String
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "MessageActionItem") } ''MessageActionItem)

instance Default MessageActionItem where
  def = MessageActionItem def

data ShowMessageRequestParams =
  ShowMessageRequestParams
    { typeShowMessageRequestParams    :: MessageType
    , messageShowMessageRequestParams :: String
    , actionsShowMessageRequestParams :: Maybe [MessageActionItem]
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ShowMessageRequestParams") } ''ShowMessageRequestParams)

instance Default ShowMessageRequestParams where
  def = ShowMessageRequestParams def def def

data ShowMessageRequest =
  ShowMessageRequest
    { jsonrpcShowMessageRequest :: String
    , methodShowMessageRequest :: String
    , idShowMessageRequest :: Int
    , paramsShowMessageRequest :: ShowMessageRequestParams
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShowMessageRequest") } ''ShowMessageRequest)

instance Default ShowMessageRequest where
  def = ShowMessageRequest "2.0" "window/showMessageRequest" def def

data ShowMessageResponse =
  ShowMessageResponse
    { idShowMessageResponse :: Int
    , resultShowMessageResponse :: Maybe String -- Selected MessageActionItem.
    , errorShowMessageResponse :: Maybe A.Object -- If an error occurred.
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ShowMessageResponse") } ''ShowMessageResponse)

instance Default ShowMessageResponse where
  def = ShowMessageResponse def def def

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
defLogMessage :: MessageType -> String -> MessageNotification
defLogMessage mt msg = MessageNotification "2.0" "window/logMessage"   (MessageNotificationParams mt msg)

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

data TelemetryNotification =
  TelemetryNotification
    { jsonrpcTelemetryNotification :: String
    , methodTelemetryNotification :: String
    , paramsTelemetryNotification :: A.Object -- ^Can be anything
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TelemetryNotification") } ''TelemetryNotification)

instance Default TelemetryNotification where
  def = TelemetryNotification "2.0" "telemetry/event" mempty

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

data DidChangeConfigurationParamsNotificationParams =
  DidChangeConfigurationParamsNotificationParams {
    settingsDidChangeConfigurationParamsNotificationParams :: A.Object
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeConfigurationParamsNotificationParams") } ''DidChangeConfigurationParamsNotificationParams)

instance Default DidChangeConfigurationParamsNotificationParams where
  def = DidChangeConfigurationParamsNotificationParams mempty

-- -------------------------------------

data DidChangeConfigurationParamsNotification =
  DidChangeConfigurationParamsNotification {
    paramsDidChangeConfigurationParamsNotification :: DidChangeConfigurationParamsNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeConfigurationParamsNotification") } ''DidChangeConfigurationParamsNotification)

instance Default DidChangeConfigurationParamsNotification where
  def = DidChangeConfigurationParamsNotification def

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
-}
-- ---------------------------------------------------------------------

data DidOpenTextDocumentNotificationParams =
  DidOpenTextDocumentNotificationParams {
    textDocumentDidOpenTextDocumentNotificationParams :: TextDocumentItem
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidOpenTextDocumentNotificationParams") } ''DidOpenTextDocumentNotificationParams)

-- ---------------------------------------

data DidOpenTextDocumentNotification =
  DidOpenTextDocumentNotification {
    paramsDidOpenTextDocumentNotification :: DidOpenTextDocumentNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidOpenTextDocumentNotification") } ''DidOpenTextDocumentNotification)

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
    { rangeTextDocumentContentChangeEvent       :: Maybe Range
    , rangeLengthTextDocumentContentChangeEvent :: Maybe Int
    , textTextDocumentContentChangeEvent        :: String
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "TextDocumentContentChangeEvent") } ''TextDocumentContentChangeEvent)

instance Default TextDocumentContentChangeEvent where
  def = TextDocumentContentChangeEvent Nothing Nothing def

-- -------------------------------------
data DidChangeTextDocumentParams =
  DidChangeTextDocumentParams
    { textDocumentDidChangeTextDocumentParams :: VersionedTextDocumentIdentifier
    , contentChangesDidChangeTextDocumentParams:: [TextDocumentContentChangeEvent]
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeTextDocumentParams") } ''DidChangeTextDocumentParams)

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
    { textDocumentDidCloseTextDocumentParams :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidCloseTextDocumentParams") } ''DidCloseTextDocumentParams)

instance Default DidCloseTextDocumentParams where
  def = DidCloseTextDocumentParams def

data DidCloseTextDocumentNotification =
  DidCloseTextDocumentNotification
    { methodDidCloseTextDocumentNotification :: String
    , paramsDidCloseTextDocumentNotification :: DidCloseTextDocumentParams
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidCloseTextDocumentNotification") } ''DidCloseTextDocumentNotification)

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
    { textDocumentDidSaveTextDocumentParams :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidSaveTextDocumentParams") } ''DidSaveTextDocumentParams)

instance Default DidSaveTextDocumentParams where
  def = DidSaveTextDocumentParams def

data DidSaveTextDocumentNotification =
  DidSaveTextDocumentNotification
    { methodDidSaveTextDocumentNotification :: String
    , paramsDidSaveTextDocumentNotification :: DidSaveTextDocumentParams
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidSaveTextDocumentNotification") } ''DidSaveTextDocumentNotification)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- ---------------------------------------------------------------------

data DefinitionRequestParams =
  DefinitionRequestParams
    { textDocumentDefinitionRequestParams :: TextDocumentIdentifier
    , positionDefinitionRequestParams     :: Position
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DefinitionRequestParams") } ''DefinitionRequestParams)

instance Default DefinitionRequestParams where
  def = DefinitionRequestParams def def

-- |
--   Client-initiated request
--

-- {"jsonrpc":"2.0","id":1,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/Foo.hs"},"position":{"line":1,"character":8}}}
data DefinitionRequest =
  DefinitionRequest {
    idDefinitionRequest       :: Int
  , paramsDefinitionRequest   :: DefinitionRequestParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DefinitionRequest") } ''DefinitionRequest)

defaultDefinitionRequest :: DefinitionRequest
defaultDefinitionRequest = DefinitionRequest 0 def

-- ---------------------------------------------------------------------

-- |
--   Server-initiated response to client request
--
data DefinitionResponse =
  DefinitionResponse {
    jsonrpcDefinitionResponse    :: String
  , idDefinitionResponse         :: Int     -- Sequence number
  , resultDefinitionResponse     :: Location
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DefinitionResponse") } ''DefinitionResponse)


-- |
--
parseErrorDefinitionResponse :: Int -> String -> DefinitionResponse
parseErrorDefinitionResponse seq msg =
  DefinitionResponse  "2.0" seq def

-- |
--
errorDefinitionResponse :: DefinitionRequest -> String -> DefinitionResponse
errorDefinitionResponse (DefinitionRequest reqSeq _) msg =
  DefinitionResponse "2.0" reqSeq def

-- ---------------------------------------------------------------------

data RenameRequestParams =
  RenameRequestParams
    { textDocumentRenameRequestParams :: TextDocumentIdentifier
    , positionRenameRequestParams     :: Position
    , newNameRenameRequestParams      :: String
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "RenameRequestParams") } ''RenameRequestParams)

instance Default RenameRequestParams where
  def = RenameRequestParams def def def

-- |
--   Client-initiated request
--

-- {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/alanz/mysrc/github/alanz/haskell-lsp/src/HieVscode.hs\"},\"position\":{\"line\":37,\"character\":17},\"newName\":\"getArgs'\"}}
data RenameRequest =
  RenameRequest {
    idRenameRequest       :: Int
  , paramsRenameRequest   :: RenameRequestParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "RenameRequest") } ''RenameRequest)

defaultRenameRequest :: RenameRequest
defaultRenameRequest = RenameRequest 0 def

-- ---------------------------------------------------------------------

-- |
--   Server-initiated response to client request
--
data RenameResponse =
  RenameResponse {
    jsonrpcRenameResponse    :: String
  , idRenameResponse         :: Int     -- Sequence number
  , resultRenameResponse     :: Location
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "RenameResponse") } ''RenameResponse)


-- |
--
parseErrorRenameResponse :: Int -> String -> RenameResponse
parseErrorRenameResponse seq msg =
  RenameResponse  "2.0" seq def

-- |
--
errorRenameResponse :: RenameRequest -> String -> RenameResponse
errorRenameResponse (RenameRequest reqSeq _) msg =
  RenameResponse "2.0" reqSeq def

-- ---------------------------------------

-- ---------------------------------------------------------------------

-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEventBody =
  OutputEventBody {
    categoryOutputEventBody :: String        -- The category of output (such as: 'console', 'stdout', 'stderr', 'telemetry'). If not specified, 'console' is assumed. 
  , outputOutputEventBody   :: String        -- The output to report.
  , dataOutputEventBody     :: Maybe String  -- Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "OutputEventBody") } ''OutputEventBody)

defaultOutputEventBody :: OutputEventBody
defaultOutputEventBody = OutputEventBody "console" "" Nothing

-- ---------------------------------------------------------------------

-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEvent =
  OutputEvent {
    seqOutputEvent   :: Int     -- Sequence number
  , typeOutputEvent  :: String  -- One of "request", "response", or "event"
  , eventOutputEvent :: String  -- Type of event
  , bodyOutputEvent  :: OutputEventBody 
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "OutputEvent") } ''OutputEvent)

defaultOutputEvent :: Int -> OutputEvent
defaultOutputEvent resSeq = OutputEvent resSeq "event" "output" defaultOutputEventBody

-- ---------------------------------------------------------------------

-- |
--   Client-initiated request
--
data Request =
  Request {
    methodRequest   :: String    -- The command to execute
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)

-- ---------------------------------------------------------------------

-- |
--   Client-initiated request
--
data ErrorResponse =
  ErrorResponse {
    jsonrpcErrorResponse :: String    -- Always "2.0"
  , idErrorResponse      :: Int -- Original request id
  , errorErrorResponse   :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ErrorResponse") } ''ErrorResponse)

instance Default ErrorResponse where
  def = ErrorResponse "2.0" 0 ""


-- ---------------------------------------------------------------------

-- |
--   Event message for "terminated" event types.
-- The event indicates that debugging of the debuggee has terminated.
--
data TerminatedEventBody =
  TerminatedEventBody {
    restartTerminatedEventBody :: Bool  -- A debug adapter may set 'restart' to true to request that the front end restarts the session.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TerminatedEventBody") } ''TerminatedEventBody)

defaultTerminatedEventBody :: TerminatedEventBody
defaultTerminatedEventBody = TerminatedEventBody False

-- ---------------------------------------------------------------------

-- |
--   Event message for "terminated" event types.
--   The event indicates that debugging of the debuggee has terminated.
--
data TerminatedEvent =
  TerminatedEvent {
    seqTerminatedEvent   :: Int     -- Sequence number
  , typeTerminatedEvent  :: String  -- One of "request", "response", or "event"
  , eventTerminatedEvent :: String  -- Type of event
  , bodyTerminatedEvent  :: TerminatedEventBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TerminatedEvent") } ''TerminatedEvent)

defaultTerminatedEvent :: Int -> TerminatedEvent
defaultTerminatedEvent seq = TerminatedEvent seq "event" "terminated" defaultTerminatedEventBody

-- ---------------------------------------------------------------------

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data TraceNotificationParams =
  TraceNotificationParams {
    valueTraceNotificationParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TraceNotificationParams") } ''TraceNotificationParams)

defaultTraceNotificationParams :: TraceNotificationParams
defaultTraceNotificationParams = TraceNotificationParams mempty

-- ---------------------------------------
-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data TraceNotification =
  TraceNotification {
    paramsTraceNotification :: TraceNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TraceNotification") } ''TraceNotification)

defaultTraceNotification :: TraceNotification
defaultTraceNotification = TraceNotification defaultTraceNotificationParams

-- ---------------------------------------------------------------------

-- -------------------------------------
{-
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
-}




-- ---------------------------------------------------------------------
