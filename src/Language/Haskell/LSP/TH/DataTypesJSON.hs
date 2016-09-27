{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.LSP.TH.DataTypesJSON where

import Data.Aeson.TH
import qualified Data.Aeson as A

import Data.Monoid
import Language.Haskell.LSP.Utility
import Data.Default

-- ---------------------------------------------------------------------

{-

Typescript definition

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
data MessageType = MtError  -- ^ Error = 1,
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

-- ---------------------------------------------------------------------
{-
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

-- ---------------------------------------------------------------------

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data DidChangeConfigurationParamsNotificationParams =
  DidChangeConfigurationParamsNotificationParams {
    settingsDidChangeConfigurationParamsNotificationParams :: A.Object
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeConfigurationParamsNotificationParams") } ''DidChangeConfigurationParamsNotificationParams)

defaultDidChangeConfigurationParamsNotificationParams :: DidChangeConfigurationParamsNotificationParams
defaultDidChangeConfigurationParamsNotificationParams = DidChangeConfigurationParamsNotificationParams mempty

-- ---------------------------------------
-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data DidChangeConfigurationParamsNotification =
  DidChangeConfigurationParamsNotification {
    paramsDidChangeConfigurationParamsNotification :: DidChangeConfigurationParamsNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidChangeConfigurationParamsNotification") } ''DidChangeConfigurationParamsNotification)

defaultDidChangeConfigurationParamsNotification :: DidChangeConfigurationParamsNotification
defaultDidChangeConfigurationParamsNotification = DidChangeConfigurationParamsNotification defaultDidChangeConfigurationParamsNotificationParams

-- ---------------------------------------------------------------------
-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data DidOpenTextDocumentNotificationParams =
  DidOpenTextDocumentNotificationParams {
    textDocumentDidOpenTextDocumentNotificationParams :: TextDocumentItem
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidOpenTextDocumentNotificationParams") } ''DidOpenTextDocumentNotificationParams)

-- ---------------------------------------
-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data DidOpenTextDocumentNotification =
  DidOpenTextDocumentNotification {
    paramsDidOpenTextDocumentNotification :: DidOpenTextDocumentNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DidOpenTextDocumentNotification") } ''DidOpenTextDocumentNotification)

-- defaultDidOpenTextDocumentNotification :: DidOpenTextDocumentNotification
-- defaultDidOpenTextDocumentNotification = DidOpenTextDocumentNotification defaultDidOpenTextDocumentNotificationParams

-- ---------------------------------------------------------------------

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data ExitNotification =
  ExitNotification {
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExitNotification") } ''ExitNotification)

defaultExitNotification :: ExitNotification
defaultExitNotification = ExitNotification

-- ---------------------------------------------------------------------

-- |
--   Initialize request; value of command field is "initialize".
--
data InitializeRequestArguments =
  InitializeRequestArguments {
    processIdInitializeRequestArguments    :: Int
  , capabilitiesInitializeRequestArguments :: A.Object -- None currently defined, but empty object sent
  , traceInitializeRequestArguments        :: String
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequestArguments") } ''InitializeRequestArguments)

defaultInitializeRequestArguments :: InitializeRequestArguments
defaultInitializeRequestArguments = InitializeRequestArguments 0 mempty mempty

-- ---------------------------------------------------------------------

-- |
--   Client-initiated request
--

-- {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":1749,"capabilities":{},"trace":"off"}}
data InitializeRequest =
  InitializeRequest {
    idInitializeRequest       :: Int                         -- Sequence number
  , rootPathInitializeRequest :: Maybe String
  , paramsInitializeRequest   :: InitializeRequestArguments  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequest") } ''InitializeRequest)

defaultInitializeRequest :: InitializeRequest
defaultInitializeRequest = InitializeRequest 0 Nothing defaultInitializeRequestArguments

-- ---------------------------------------------------------------------

-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilitiesInner =
  InitializeResponseCapabilitiesInner {
    definitionProviderInitializeResponseCapabilitiesInner :: Bool
  , renameProviderInitializeResponseCapabilitiesInner     :: Bool
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilitiesInner") } ''InitializeResponseCapabilitiesInner)

-- |
--
defaultInitializeResponseCapabilitiesInner :: InitializeResponseCapabilitiesInner
defaultInitializeResponseCapabilitiesInner = InitializeResponseCapabilitiesInner True True

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    capabilitiesInitializeResponseCapabilities :: InitializeResponseCapabilitiesInner
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilities") } ''InitializeResponseCapabilities)

-- |
--
defaultInitializeResponseCapabilities :: InitializeResponseCapabilities
defaultInitializeResponseCapabilities = InitializeResponseCapabilities defaultInitializeResponseCapabilitiesInner

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


-- |
--
parseErrorInitializeResponse :: Int -> String -> InitializeResponse
parseErrorInitializeResponse seq msg =
  InitializeResponse  "2.0" seq defaultInitializeResponseCapabilities

-- |
--
errorInitializeResponse :: InitializeRequest -> String -> InitializeResponse
errorInitializeResponse (InitializeRequest reqSeq _ _) msg =
  InitializeResponse "2.0" reqSeq defaultInitializeResponseCapabilities

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


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "OutputEventBody") } ''OutputEventBody)

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
  -- , argumentsRequest :: [String]  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)

-- ---------------------------------------------------------------------

-- |
--   Client-initiated request
--

data ShutdownRequest =
  ShutdownRequest {
    idShutdownRequest       :: Int                         -- Sequence number
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownRequest") } ''ShutdownRequest)

defaultShutdownRequest :: ShutdownRequest
defaultShutdownRequest = ShutdownRequest 0

-- ---------------------------------------------------------------------

-- |
--   Server-initiated response to client request
--
data ShutdownResponse =
  ShutdownResponse {
    jsonrpcShutdownResponse    :: String
  , idShutdownResponse         :: Int     -- Sequence number
  , resultShutdownResponse     :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownResponse") } ''ShutdownResponse)


-- |
--
parseErrorShutdownResponse :: Int -> String -> ShutdownResponse
parseErrorShutdownResponse seq msg =
  ShutdownResponse  "2.0" seq msg

-- |
--
errorShutdownResponse :: Int -> ShutdownRequest -> String -> ShutdownResponse
errorShutdownResponse seq (ShutdownRequest reqSeq) msg =
  ShutdownResponse "2.0" seq msg

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
{-
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
-}

-- |
--
data MessageNotificationParams =
  MessageNotificationParams {
    typeMessageNotificationParams    :: MessageType
  , messageMessageNotificationParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "MessageNotificationParams") } ''MessageNotificationParams)

instance Default MessageNotificationParams where
  def = MessageNotificationParams MtWarning ""

-- ---------------------------------------
-- |
--
data MessageNotification =
  MessageNotification {
    jsonrpcMessageNotification :: String
  , methodMessageNotification  :: String
  , paramsMessageNotification  :: MessageNotificationParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "MessageNotification") } ''MessageNotification)

-- -------------------------------------

type LogMessageNotification = MessageNotification

instance Default LogMessageNotification where
  def = MessageNotification "2.0" "window/logMessage" def

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


defLogMessage :: MessageType -> String -> MessageNotification
defLogMessage mt msg = MessageNotification "2.0" "window/logMessage"   (MessageNotificationParams mt msg)

defShowMessage :: MessageType -> String -> MessageNotification
defShowMessage mt msg = MessageNotification "2.0" "window/showMessage" (MessageNotificationParams mt msg)

-- ---------------------------------------------------------------------
