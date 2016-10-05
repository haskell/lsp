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

-- |
--   Client-initiated request. only pull out the method, for routing
--
data Request =
  Request {
    methodRequest   :: String    -- The command to execute
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)

-- ---------------------------------------------------------------------

data RequestMessage a =
  RequestMessage
    { jsonrpcRequestMessage :: String
    , idRequestMessage      :: Int
    , methodRequestMessage  :: String
    , paramsRequestMessage  :: Maybe a
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "RequestMessage") } ''RequestMessage)

instance (Default a) => Default (RequestMessage a) where
  def = RequestMessage "2.0" def def def

-- ---------------------------------------------------------------------
{-
interface ResponseError<D> {
    /**
     * A number indicating the error type that occurred.
     */
    code: number;

    /**
     * A string providing a short description of the error.
     */
    message: string;

    /**
     * A Primitive or Structured value that contains additional
     * information about the error. Can be omitted.
     */
    data?: D;
}

export namespace ErrorCodes {
    export const ParseError: number = -32700;
    export const InvalidRequest: number = -32600;
    export const MethodNotFound: number = -32601;
    export const InvalidParams: number = -32602;
    export const InternalError: number = -32603;
    export const serverErrorStart: number = -32099
    export const serverErrorEnd: number = -32000;
}
-}

data ErrorCode = ParseError
               | InvalidRequest
               | MethodNotFound
               | InvalidParams
               | InternalError
               -- ^ Note: server error codes are reserved from -32099 to -32000
               deriving (Read,Show,Eq)

instance A.ToJSON ErrorCode where
  toJSON ParseError     = A.Number (-32700)
  toJSON InvalidRequest = A.Number (-32600)
  toJSON MethodNotFound = A.Number (-32601)
  toJSON InvalidParams  = A.Number (-32602)
  toJSON InternalError  = A.Number (-32603)

instance A.FromJSON ErrorCode where
  parseJSON (A.Number (-32700)) = pure ParseError
  parseJSON (A.Number (-32600)) = pure InvalidRequest
  parseJSON (A.Number (-32601)) = pure MethodNotFound
  parseJSON (A.Number (-32602)) = pure InvalidParams
  parseJSON (A.Number (-32603)) = pure InternalError
  parseJSON _                   = mempty

instance Default ErrorCode where
  def = InternalError -- Choose something random

-- -------------------------------------

data ResponseError =
  ResponseError
    { codeResponseError    :: ErrorCode
    , messageResponseError :: String
    , dataResponseError    :: Maybe A.Object
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ResponseError") } ''ResponseError)

instance Default ResponseError where
  def = ResponseError def def Nothing

-- ---------------------------------------------------------------------

data ResponseMessage a =
  ResponseMessage
    { jsonrpcResponseMessage :: String
    , idResponseMessage :: Int
    , resultResponseMessage :: Maybe a
    , errorResponseMessage  :: Maybe ResponseError
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ResponseMessage") } ''ResponseMessage)

instance (Default a) => Default (ResponseMessage a) where
  def = ResponseMessage "2.0" def def Nothing

type ErrorResponse = ResponseMessage ()

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
-- data InitializeRequest =
--   InitializeRequest {
--     idInitializeRequest       :: Int                         -- Sequence number
--   , paramsInitializeRequest   :: InitializeRequestArguments  -- Object containing arguments for the command
--   } deriving (Show, Read, Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequest") } ''InitializeRequest)

-- instance Default InitializeRequest where
--   def = InitializeRequest 0 def

type InitializeRequest = RequestMessage InitializeRequestArguments

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

type InitializeResponse = ResponseMessage InitializeResponseCapabilities

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

-- data ShutdownRequest =
--   ShutdownRequest {
--     idShutdownRequest :: Int
--   } deriving (Show, Read, Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownRequest") } ''ShutdownRequest)

-- instance Default ShutdownRequest where
--   def = ShutdownRequest 0

type ShutdownRequest  = RequestMessage ()
type ShutdownResponse = ResponseMessage String

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

-- data ShowMessageRequest =
--   ShowMessageRequest
--     { jsonrpcShowMessageRequest :: String
--     , methodShowMessageRequest :: String
--     , idShowMessageRequest :: Int
--     , paramsShowMessageRequest :: ShowMessageRequestParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShowMessageRequest") } ''ShowMessageRequest)

-- instance Default ShowMessageRequest where
--   def = ShowMessageRequest "2.0" "window/showMessageRequest" def def

type ShowMessageRequest = RequestMessage ShowMessageRequestParams

-- data ShowMessageResponse =
--   ShowMessageResponse
--     { idShowMessageResponse :: Int
--     , resultShowMessageResponse :: Maybe String -- Selected MessageActionItem.
--     , errorShowMessageResponse :: Maybe A.Object -- If an error occurred.
--     } deriving (Show,Read,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ShowMessageResponse") } ''ShowMessageResponse)

-- instance Default ShowMessageResponse where
--   def = ShowMessageResponse def def def

type ShowMessageResponse = ResponseMessage String

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

instance Default FileChangeType where
  def = FcChanged -- Choose something random

-- -------------------------------------

data FileEvent =
  FileEvent
    { uriFileEvent  :: String
    , typeFileEvent :: FileChangeType
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "FileEvent") } ''FileEvent)

data DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams
    { paramsDidChangeWatchedFilesParams :: [FileEvent]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeWatchedFilesParams") } ''DidChangeWatchedFilesParams)

instance Default DidChangeWatchedFilesParams where
  def = DidChangeWatchedFilesParams def

data DidChangeWatchedFilesNotification =
  DidChangeWatchedFilesNotification
    { methodDidChangeWatchedFilesNotification :: String
    , paramsDidChangeWatchedFilesNotification :: DidChangeWatchedFilesParams
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeWatchedFilesNotification") } ''DidChangeWatchedFilesNotification)

instance Default DidChangeWatchedFilesNotification where
  def = DidChangeWatchedFilesNotification def def

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
    { uriPublishDiagnosticsParams :: String
    , diagnosticsPublishDiagnosticsParams :: [Diagnostic]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "PublishDiagnosticsParams") } ''PublishDiagnosticsParams)

instance Default PublishDiagnosticsParams where
  def = PublishDiagnosticsParams def def

data PublishDiagnosticsNotification =
  PublishDiagnosticsNotification
    { methodPublishDiagnosticsNotification :: String
    , paramsPublishDiagnosticsNotification :: PublishDiagnosticsParams
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "PublishDiagnosticsNotification") } ''PublishDiagnosticsNotification)

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

-- data CompletionRequest =
--   CompletionRequest
--     { idCompletionRequest     :: Int
--     , paramsCompletionRequest :: TextDocumentPositionParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionRequest") } ''CompletionRequest)

type CompletionRequest = RequestMessage TextDocumentPositionParams

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
    /**
     * An edit which is applied to a document when selecting
     * this completion. When an edit is provided the value of
     * insertText is ignored.
     */
    textEdit?: TextEdit;
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
         deriving (Read,Show,Eq)

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
  parseJSON _            = mempty

instance Default CompletionItemKind where
  def = CiText

-- -------------------------------------

data CompletionItem =
  CompletionItem
    { labelCompletionItem :: String -- ^ The label of this completion item. By
                                    -- default also the text that is inserted
                                    -- when selecting this completion.
    , kindCompletionItem :: Maybe CompletionItemKind
    , detailCompletionItem :: Maybe String -- ^ A human-readable string with
                                           -- additional information about this
                                           -- item, like type or symbol
                                           -- information.
    , documentationCompletionItem :: Maybe String-- ^ A human-readable string
                                                 -- that represents a
                                                 -- doc-comment.
    , sortTextCompletionItem :: Maybe String -- ^ A string that should be used
                                             -- when filtering a set of
                                             -- completion items. When `falsy`
                                             -- the label is used.
    , filterTextCompletionItem :: Maybe String -- ^ A string that should be used
                                               -- when filtering a set of
                                               -- completion items. When `falsy`
                                               -- the label is used.
    , insertTextCompletionItem :: Maybe String -- ^ A string that should be
                                               -- inserted a document when
                                               -- selecting this completion.
                                               -- When `falsy` the label is
                                               -- used.
    , textEditCompletionItem :: Maybe TextEdit -- ^ An edit which is applied to
                                               -- a document when selecting this
                                               -- completion. When an edit is
                                               -- provided the value of
                                               -- insertText is ignored.
    , dataCompletionItem :: Maybe A.Object -- ^ An data entry field that is
                                           -- preserved on a completion item
                                           -- between a completion and a
                                           -- completion resolve request.
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "CompletionItem") } ''CompletionItem)

data CompletionListType =
  CompletionListType
    { isIncompleteCompletionListType :: Bool
    , itemsCompletionListType :: [CompletionItem]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionListType") } ''CompletionListType)

instance Default CompletionListType where
  def = CompletionListType False []

data CompletionResponseResult
  = CompletionList CompletionListType
  | Completions [CompletionItem]
  deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionResponseResult") } ''CompletionResponseResult)

type CompletionResponse = ResponseMessage CompletionResponseResult

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

-- data CompletionItemResolveRequest =
--   CompletionItemResolveRequest
--     { methodCompletionItemResolveRequest :: String
--     , paramsCompletionItemResolveRequest :: CompletionItem
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionItemResolveRequest") } ''CompletionItemResolveRequest)

type CompletionItemResolveRequest  = RequestMessage CompletionItem
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

type MarkedString = string | { language: string; value: string };

    error: code and message set in case an exception happens during the hover
    request.
-}

-- data HoverRequest =
--   HoverRequest
--     { methodHoverRequest :: String
--     , paramsHoverRequest :: TextDocumentPositionParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "HoverRequest") } ''HoverRequest)
type HoverRequest = RequestMessage TextDocumentPositionParams

data MarkedString =
  MarkedString
    { languageMarkedString :: String
    , valueMarkedString    :: String
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "MarkedString") } ''MarkedString)

instance Default MarkedString where
  def = MarkedString def def

data Hover =
  Hover
    { contentsHover :: [MarkedString]
    , rangeHover    :: Maybe Range
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "Hover") } ''Hover)

instance Default Hover where
  def = Hover def Nothing

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

-- data SignatureHelpRequest =
--   SignatureHelpRequest
--     { methodSignatureHelpRequest :: String
--     , paramsSignatureHelpRequest :: TextDocumentPositionParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "SignatureHelpRequest") } ''SignatureHelpRequest)

type SignatureHelpRequest = RequestMessage TextDocumentPositionParams

-- -------------------------------------

data ParameterInformation =
  ParameterInformation
    { labelParameterInformation :: String
    , documentationParameterInformation :: Maybe String
    } deriving (Read,Show,Eq)
$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "ParameterInformation") } ''ParameterInformation)

instance Default ParameterInformation where
  def = ParameterInformation def def

-- -------------------------------------

data SignatureInformation =
  SignatureInformation
    { labelSignatureInformation         :: String
    , documentationSignatureInformation :: Maybe String
    , parametersSignatureInformation    :: Maybe [ParameterInformation]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "SignatureInformation") } ''SignatureInformation)

data SignatureHelp =
  SignatureHelp
    { signaturesSignatureHelp :: [SignatureInformation]
    , activeSignature :: Maybe Int -- ^ The active signature
    , activeParameter :: Maybe Int -- ^ The active parameter of the active signature
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "SignatureHelp") } ''SignatureHelp)

instance Default SignatureHelp where
  def = SignatureHelp def Nothing Nothing

type SignatureHelpResponse = ResponseMessage SignatureHelp

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

data DefinitionRequestParams =
  DefinitionRequestParams
    { textDocumentDefinitionRequestParams :: TextDocumentIdentifier
    , positionDefinitionRequestParams     :: Position
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DefinitionRequestParams") } ''DefinitionRequestParams)

instance Default DefinitionRequestParams where
  def = DefinitionRequestParams def def

-- {"jsonrpc":"2.0","id":1,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/Foo.hs"},"position":{"line":1,"character":8}}}
-- data DefinitionRequest =
--   DefinitionRequest {
--     idDefinitionRequest       :: Int
--   , paramsDefinitionRequest   :: DefinitionRequestParams
--   } deriving (Show, Read, Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DefinitionRequest") } ''DefinitionRequest)

-- instance Default DefinitionRequest where
--   def = DefinitionRequest 0 def

type DefinitionRequest  = RequestMessage DefinitionRequestParams
type DefinitionResponse = ResponseMessage Location

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
    { includeDeclarationReferenceContext :: Bool
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ReferenceContext") } ''ReferenceContext)

instance Default ReferenceContext where
  def = ReferenceContext False

data ReferenceParams =
  ReferenceParams
    { textDocumentReferenceParams :: TextDocumentIdentifier
    , positionReferenceParams     :: Position
    , contextReferenceParams      :: ReferenceContext
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ReferenceParams") } ''ReferenceParams)

instance Default ReferenceParams where
  def = ReferenceParams def def def

-- data FindReferencesRequest =
--   FindReferencesRequest
--     { methodFindReferencesRequest :: String
--     , paramsFindReferencesRequest :: ReferenceParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "FindReferencesRequest") } ''FindReferencesRequest)

type FindReferencesRequest  = RequestMessage ReferenceParams
type FindReferencesResponse = ResponseMessage [Location]

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

-}

-- data DocumentHighlightsRequest =
--   DocumentHighlightsRequest
--     { methodDocumentHighlightsRequest :: String
--     , paramsDocumentHighlightsRequest :: TextDocumentPositionParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "DocumentHighlightsRequest") } ''DocumentHighlightsRequest)

type DocumentHighlightsRequest = RequestMessage TextDocumentPositionParams

-- -------------------------------------

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
    { rangeDocumentHighlight :: Range
    , kindDocumentHighlight  :: Maybe DocumentHighlightKind
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "DocumentHighlight") } ''DocumentHighlight)

type DocumentHighlightsResponse = ResponseMessage [DocumentHighlight]

-- ---------------------------------------------------------------------
{-
Document Symbols Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-symbols-request

The document symbol request is sent from the client to the server to list all
symbols found in a given text document.

    Changed: In 2.0 the request uses DocumentSymbolParams instead of a single
             uri.

Request

    method: 'textDocument/documentSymbol'
    params: DocumentSymbolParams defined as follows:

interface DocumentSymbolParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier;
}

Response

    result: SymbolInformation[] defined as follows:

/**
 * Represents information about programming constructs like variables, classes,
 * interfaces etc.
 */
interface SymbolInformation {
    /**
     * The name of this symbol.
     */
    name: string;

    /**
     * The kind of this symbol.
     */
    kind: number;

    /**
     * The location of this symbol.
     */
    location: Location;

    /**
     * The name of the symbol containing this symbol.
     */
    containerName?: string;
}

Where the kind is defined like this:

/**
 * A symbol kind.
 */
export enum SymbolKind {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
}

    error: code and message set in case an exception happens during the document
           symbol request.
-}

data DocumentSymbolParams =
  DocumentSymbolParams
    { textDocumentDocumentSymbolParams :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "DocumentSymbolParams") } ''DocumentSymbolParams)

instance Default DocumentSymbolParams where
  def = DocumentSymbolParams def

-- -------------------------------------

-- data DocumentSymbolsRequest =
--   DocumentSymbolsRequest
--     { methodDocumentSymbolsRequest :: String
--     , paramsDocumentSymbolsRequest :: DocumentSymbolParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "DocumentSymbolsRequest") } ''DocumentSymbolsRequest)

type DocumentSymbolsRequest = RequestMessage DocumentSymbolParams

-- -------------------------------------

data SymbolKind
    = SkFile
    | SkModule
    | SkNamespace
    | SkPackage
    | SkClass
    | SkMethod
    | SkProperty
    | SkField
    | SkConstructor
    | SkEnum
    | SkInterface
    | SkFunction
    | SkVariable
    | SkConstant
    | SkString
    | SkNumber
    | SkBoolean
    | SkArray
    deriving (Read,Show,Eq)

instance A.ToJSON SymbolKind where
  toJSON SkFile        = A.Number 1
  toJSON SkModule      = A.Number 2
  toJSON SkNamespace   = A.Number 3
  toJSON SkPackage     = A.Number 4
  toJSON SkClass       = A.Number 5
  toJSON SkMethod      = A.Number 6
  toJSON SkProperty    = A.Number 7
  toJSON SkField       = A.Number 8
  toJSON SkConstructor = A.Number 9
  toJSON SkEnum        = A.Number 10
  toJSON SkInterface   = A.Number 11
  toJSON SkFunction    = A.Number 12
  toJSON SkVariable    = A.Number 13
  toJSON SkConstant    = A.Number 14
  toJSON SkString      = A.Number 15
  toJSON SkNumber      = A.Number 16
  toJSON SkBoolean     = A.Number 17
  toJSON SkArray       = A.Number 18

instance A.FromJSON SymbolKind where
  parseJSON (A.Number  1) = pure SkFile
  parseJSON (A.Number  2) = pure SkModule
  parseJSON (A.Number  3) = pure SkNamespace
  parseJSON (A.Number  4) = pure SkPackage
  parseJSON (A.Number  5) = pure SkClass
  parseJSON (A.Number  6) = pure SkMethod
  parseJSON (A.Number  7) = pure SkProperty
  parseJSON (A.Number  8) = pure SkField
  parseJSON (A.Number  9) = pure SkConstructor
  parseJSON (A.Number 10) = pure SkEnum
  parseJSON (A.Number 11) = pure SkInterface
  parseJSON (A.Number 12) = pure SkFunction
  parseJSON (A.Number 13) = pure SkVariable
  parseJSON (A.Number 14) = pure SkConstant
  parseJSON (A.Number 15) = pure SkString
  parseJSON (A.Number 16) = pure SkNumber
  parseJSON (A.Number 17) = pure SkBoolean
  parseJSON (A.Number 18) = pure SkArray
  parseJSON _             = mempty

instance Default SymbolKind where
  def = SkFile -- Choose something random

-- ---------------------------------------------------------------------

data SymbolInformation =
  SymbolInformation
    { nameSymbolInformation          :: String
    , kindSymbolInformation          :: SymbolKind
    , locationSymbolInformation      :: Location
    , containerNameSymbolInformation :: Maybe String -- ^The name of the symbol
                                                     -- containing this symbol.
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "SymbolInformation") } ''SymbolInformation)

instance Default SymbolInformation where
  def = SymbolInformation def def def def

-- -------------------------------------

type DocumentSymbolsResponse = ResponseMessage [SymbolInformation]

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
    { queryWorkspaceSymbolParams :: String
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "WorkspaceSymbolParams") } ''WorkspaceSymbolParams)

-- data WorkspaceSymbolsRequest =
--   WorkspaceSymbolsRequest
--     { methodWorkspaceSymbolsRequest :: String
--     , paramsWorkspaceSymbolsRequest :: WorkspaceSymbolParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "WorkspaceSymbolsRequest") } ''WorkspaceSymbolsRequest)

type WorkspaceSymbolsRequest  = RequestMessage WorkspaceSymbolParams
type WorkspaceSymbolsResponse = ResponseMessage [SymbolInformation]

-- ---------------------------------------------------------------------
{-
Code Action Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#code-action-request

The code action request is sent from the client to the server to compute
commands for a given text document and range. The request is triggered when the
user moves the cursor into a problem marker in the editor or presses the
lightbulb associated with a marker.

Request

    method: 'textDocument/codeAction'
    params: CodeActionParams defined as follows:

/**
 * Params for the CodeActionRequest
 */
interface CodeActionParams {
    /**
     * The document in which the command was invoked.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The range for which the command was invoked.
     */
    range: Range;

    /**
     * Context carrying additional information.
     */
    context: CodeActionContext;
}

/**
 * Contains additional diagnostic information about the context in which
 * a code action is run.
 */
interface CodeActionContext {
    /**
     * An array of diagnostics.
     */
    diagnostics: Diagnostic[];
}

Response

    result: Command[] defined as follows:
    error: code and message set in case an exception happens during the code
           action request.

-}

data CodeActionContext =
  CodeActionContext
    { diagnosticsCodeActionContext :: [Diagnostic]
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CodeActionContext") } ''CodeActionContext)

instance Default CodeActionContext where
  def = CodeActionContext def

data CodeActionParams =
  CodeActionParams
    { textDocumentCodeActionParams :: TextDocumentIdentifier
    , rangeCodeActionParams        :: Range
    , contextCodeActionParams      :: CodeActionContext
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CodeActionParams") } ''CodeActionParams)

instance Default CodeActionParams where
  def = CodeActionParams def def def

-- -------------------------------------

-- data CodeActionRequest =
--   CodeActionRequest
--     { methodCodeActionRequest :: String
--     , paramsCodeActionRequest :: CodeActionParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CodeActionRequest") } ''CodeActionRequest)

type CodeActionRequest  = RequestMessage CodeActionParams
type CodeActionResponse = ResponseMessage [Command]

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
    { textDocumentCodeLensParams :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "CodeLensParams") } ''CodeLensParams)

-- data CodeLensRequest =
--   CodeLensRequest
--     { methodCodeLensRequest :: String
--     , paramsCodeLensRequest :: CodeLensParams
--     } deriving (Show,Read,Eq)

-- $(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "CodeLensRequest") } ''CodeLensRequest)

type CodeLensRequest = RequestMessage CodeLensParams

-- -------------------------------------

data CodeLens =
  CodeLens
    { rangeCodeLens :: Range
    , commandCodeLens :: Maybe Command
    , dataCodeLens :: Maybe A.Object
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = rdrop (length "CodeLens") } ''CodeLens)

instance Default CodeLens where
  def = CodeLens def def def

type CodeLensResponse = ResponseMessage [CodeLens]

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

-- data CodeLensResolveRequest =
--   CodeLensResolveRequest
--     { methodCodeLensResolveRequest :: String
--     , paramsCodeLensResolveRequest :: CodeLens
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CodeLensResolveRequest") } ''CodeLensResolveRequest)

type CodeLensResolveRequest  = RequestMessage CodeLens
type CodeLensResolveResponse = ResponseMessage [CodeLens]

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
-}

data FormattingOptions =
  FormattingOptions
    { tabSizeFormattingOptions      :: Int
    , insertSpacesFormattingOptions :: Bool -- ^ Prefer spaces over tabs
    -- Note: May be more properties
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "FormattingOptions") } ''FormattingOptions)

data DocumentFormattingParams =
  DocumentFormattingParams
    { textDocumentDocumentFormattingParams :: TextDocumentIdentifier
    , optionsDocumentFormattingParams      :: FormattingOptions
    } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentFormattingParams") } ''DocumentFormattingParams)

-- data DocumentFormattingRequest =
--   DocumentFormattingRequest
--     { methodDocumentFormattingRequest :: String
--     , paramsDocumentFormattingRequest :: DocumentFormattingParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentFormattingRequest") } ''DocumentFormattingRequest)

type DocumentFormattingRequest  = RequestMessage DocumentFormattingParams
type DocumentFormattingResponse = ResponseMessage [TextEdit]

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
    { textDocumentDocumentRangeFormattingParams :: TextDocumentIdentifier
    , rangeDocumentRangeFormattingParams        :: Range
    , optionsDocumentRangeFormattingParams      :: FormattingOptions
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentRangeFormattingParams") } ''DocumentRangeFormattingParams)

-- data DocumentRangeFormattingRequest =
--   DocumentRangeFormattingRequest
--     { methodDocumentRangeFormattingRequest :: String
--     , paramsDocumentRangeFormattingRequest :: DocumentRangeFormattingParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentRangeFormattingRequest") } ''DocumentRangeFormattingRequest)

type DocumentRangeFormattingRequest  = RequestMessage DocumentRangeFormattingParams
type DocumentRangeFormattingResponse = ResponseMessage [TextEdit]

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
-}

data DocumentOnTypeFormattingParams =
  DocumentOnTypeFormattingParams
    { textDocumentDocumentOnTypeFormattingParams :: TextDocumentIdentifier
    , positionDocumentOnTypeFormattingParams     :: Position
    , chDocumentOnTypeFormattingParams           :: String
    , optionsDocumentOnTypeFormattingParams      :: FormattingOptions
    } deriving (Read,Show,Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentOnTypeFormattingParams") } ''DocumentOnTypeFormattingParams)

-- data DocumentOnTypeFormattingRequest =
--   DocumentOnTypeFormattingRequest
--     { methodDocumentOnTypeFormattingRequest :: String
--     , paramsDocumentOnTypeFormattingRequest :: DocumentOnTypeFormattingParams
--     } deriving (Read,Show,Eq)

-- $(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DocumentOnTypeFormattingRequest") } ''DocumentOnTypeFormattingRequest)

type DocumentOnTypeFormattingRequest  = RequestMessage DocumentOnTypeFormattingParams
type DocumentOnTypeFormattingResponse = ResponseMessage [TextEdit]

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

-}
data RenameRequestParams =
  RenameRequestParams
    { textDocumentRenameRequestParams :: TextDocumentIdentifier
    , positionRenameRequestParams     :: Position
    , newNameRenameRequestParams      :: String
    } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "RenameRequestParams") } ''RenameRequestParams)

instance Default RenameRequestParams where
  def = RenameRequestParams def def def

-- {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/alanz/mysrc/github/alanz/haskell-lsp/src/HieVscode.hs\"},\"position\":{\"line\":37,\"character\":17},\"newName\":\"getArgs'\"}}

type RenameRequest  = RequestMessage RenameRequestParams
type RenameResponse = ResponseMessage Location

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- ---------------------------------------------------------------------

data TraceNotificationParams =
  TraceNotificationParams {
    valueTraceNotificationParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TraceNotificationParams") } ''TraceNotificationParams)

instance Default TraceNotificationParams where
  def = TraceNotificationParams mempty

data TraceNotification =
  TraceNotification {
    paramsTraceNotification :: TraceNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TraceNotification") } ''TraceNotification)

instance Default TraceNotification where
  def = TraceNotification def

-- ---------------------------------------------------------------------
