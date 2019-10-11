{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.Message where

import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
-- For <= 8.2.2
import           Data.Text                                  (Text)
import           Language.Haskell.LSP.Types.Constants


-- | Id used for a request, Can be either a String or an Int
data LspId = IdInt Int | IdString Text
            deriving (Show,Read,Eq,Ord)

instance A.ToJSON LspId where
  toJSON (IdInt i)    = toJSON i
  toJSON (IdString s) = toJSON s

instance A.FromJSON LspId where
  parseJSON v@(A.Number _) = IdInt <$> parseJSON v
  parseJSON  (A.String  s) = return (IdString s)
  parseJSON _              = mempty

instance Hashable LspId where
  hashWithSalt salt (IdInt i) = hashWithSalt salt i
  hashWithSalt salt (IdString s) = hashWithSalt salt s

-- ---------------------------------------------------------------------

-- | Id used for a response, Can be either a String or an Int, or Null. If a
-- request doesn't provide a result value the receiver of a request still needs
-- to return a response message to conform to the JSON RPC specification. The
-- result property of the ResponseMessage should be set to null in this case to
-- signal a successful request.
data LspIdRsp = IdRspInt Int | IdRspString Text | IdRspNull
            deriving (Show,Read,Eq)

instance A.ToJSON LspIdRsp where
  toJSON (IdRspInt i)    = toJSON i
  toJSON (IdRspString s) = toJSON s
  toJSON IdRspNull       = A.Null

instance A.FromJSON LspIdRsp where
  parseJSON v@(A.Number _) = IdRspInt <$> parseJSON v
  parseJSON  (A.String  s) = return $ IdRspString s
  parseJSON  A.Null        = return IdRspNull
  parseJSON _              = mempty

instance Hashable LspIdRsp where
  hashWithSalt salt (IdRspInt i) = hashWithSalt salt i
  hashWithSalt salt (IdRspString s) = hashWithSalt salt s
  hashWithSalt _ IdRspNull = 0

-- | Converts an LspId to its LspIdRsp counterpart.
responseId :: LspId -> LspIdRsp
responseId (IdInt    i) = IdRspInt i
responseId (IdString s) = IdRspString s

-- | Converts an LspIdRsp to its LspId counterpart.
requestId :: LspIdRsp -> LspId
requestId (IdRspInt    i) = IdInt i
requestId (IdRspString s) = IdString s
requestId IdRspNull       = error "Null response id"

-- ---------------------------------------------------------------------

-- Client Methods
data ClientMethod =
 -- General
   Initialize
 | Initialized
 | Shutdown
 | Exit
 | CancelRequest
 -- Workspace
 | WorkspaceDidChangeWorkspaceFolders
 | WorkspaceDidChangeConfiguration
 | WorkspaceDidChangeWatchedFiles
 | WorkspaceSymbol
 | WorkspaceExecuteCommand
 -- Progress
 | WorkDoneProgressCancel
 -- Document
 | TextDocumentDidOpen
 | TextDocumentDidChange
 | TextDocumentWillSave
 | TextDocumentWillSaveWaitUntil
 | TextDocumentDidSave
 | TextDocumentDidClose
 | TextDocumentCompletion
 | CompletionItemResolve
 | TextDocumentHover
 | TextDocumentSignatureHelp
 | TextDocumentDefinition
 | TextDocumentTypeDefinition
 | TextDocumentImplementation
 | TextDocumentReferences
 | TextDocumentDocumentHighlight
 | TextDocumentDocumentSymbol
 | TextDocumentCodeAction
 | TextDocumentCodeLens
 | CodeLensResolve
 | TextDocumentDocumentLink
 | DocumentLinkResolve
 | TextDocumentDocumentColor
 | TextDocumentColorPresentation
 | TextDocumentFormatting
 | TextDocumentRangeFormatting
 | TextDocumentOnTypeFormatting
 | TextDocumentRename
 | TextDocumentPrepareRename
 | TextDocumentFoldingRange
 -- A custom message type. It is not enforced that this starts with $/.
 | CustomClientMethod Text
   deriving (Eq,Ord,Read,Show)

instance A.FromJSON ClientMethod where
  -- General
  parseJSON (A.String "initialize")                       = return Initialize
  parseJSON (A.String "initialized")                      = return Initialized
  parseJSON (A.String "shutdown")                         = return Shutdown
  parseJSON (A.String "exit")                             = return Exit
  parseJSON (A.String "$/cancelRequest")                  = return CancelRequest
 -- Workspace
  parseJSON (A.String "workspace/didChangeWorkspaceFolders") = return WorkspaceDidChangeWorkspaceFolders
  parseJSON (A.String "workspace/didChangeConfiguration") = return WorkspaceDidChangeConfiguration
  parseJSON (A.String "workspace/didChangeWatchedFiles")  = return WorkspaceDidChangeWatchedFiles
  parseJSON (A.String "workspace/symbol")                 = return WorkspaceSymbol
  parseJSON (A.String "workspace/executeCommand")         = return WorkspaceExecuteCommand
 -- Document
  parseJSON (A.String "textDocument/didOpen")             = return TextDocumentDidOpen
  parseJSON (A.String "textDocument/didChange")           = return TextDocumentDidChange
  parseJSON (A.String "textDocument/willSave")            = return TextDocumentWillSave
  parseJSON (A.String "textDocument/willSaveWaitUntil")   = return TextDocumentWillSaveWaitUntil
  parseJSON (A.String "textDocument/didSave")             = return TextDocumentDidSave
  parseJSON (A.String "textDocument/didClose")            = return TextDocumentDidClose
  parseJSON (A.String "textDocument/completion")          = return TextDocumentCompletion
  parseJSON (A.String "completionItem/resolve")           = return CompletionItemResolve
  parseJSON (A.String "textDocument/hover")               = return TextDocumentHover
  parseJSON (A.String "textDocument/signatureHelp")       = return TextDocumentSignatureHelp
  parseJSON (A.String "textDocument/definition")          = return TextDocumentDefinition
  parseJSON (A.String "textDocument/typeDefinition")      = return TextDocumentTypeDefinition
  parseJSON (A.String "textDocument/implementation")      = return TextDocumentImplementation
  parseJSON (A.String "textDocument/references")          = return TextDocumentReferences
  parseJSON (A.String "textDocument/documentHighlight")   = return TextDocumentDocumentHighlight
  parseJSON (A.String "textDocument/documentSymbol")      = return TextDocumentDocumentSymbol
  parseJSON (A.String "textDocument/codeAction")          = return TextDocumentCodeAction
  parseJSON (A.String "textDocument/codeLens")            = return TextDocumentCodeLens
  parseJSON (A.String "codeLens/resolve")                 = return CodeLensResolve
  parseJSON (A.String "textDocument/documentLink")        = return TextDocumentDocumentLink
  parseJSON (A.String "documentLink/resolve")             = return DocumentLinkResolve
  parseJSON (A.String "textDocument/documentColor")       = return TextDocumentDocumentColor
  parseJSON (A.String "textDocument/colorPresentation")   = return TextDocumentColorPresentation
  parseJSON (A.String "textDocument/formatting")          = return TextDocumentFormatting
  parseJSON (A.String "textDocument/rangeFormatting")     = return TextDocumentRangeFormatting
  parseJSON (A.String "textDocument/onTypeFormatting")    = return TextDocumentOnTypeFormatting
  parseJSON (A.String "textDocument/rename")              = return TextDocumentRename
  parseJSON (A.String "textDocument/prepareRename")       = return TextDocumentPrepareRename
  parseJSON (A.String "textDocument/foldingRange")        = return TextDocumentFoldingRange
  parseJSON (A.String "window/workDoneProgress/cancel")   = return WorkDoneProgressCancel
  parseJSON (A.String x)                                  = return (CustomClientMethod x)
  parseJSON _                                             = mempty

instance A.ToJSON ClientMethod where
  -- General
  toJSON Initialize                      = A.String "initialize"
  toJSON Initialized                     = A.String "initialized"
  toJSON Shutdown                        = A.String "shutdown"
  toJSON Exit                            = A.String "exit"
  toJSON CancelRequest                   = A.String "$/cancelRequest"
  -- Workspace
  toJSON WorkspaceDidChangeWorkspaceFolders = A.String "workspace/didChangeWorkspaceFolders"
  toJSON WorkspaceDidChangeConfiguration = A.String "workspace/didChangeConfiguration"
  toJSON WorkspaceDidChangeWatchedFiles  = A.String "workspace/didChangeWatchedFiles"
  toJSON WorkspaceSymbol                 = A.String "workspace/symbol"
  toJSON WorkspaceExecuteCommand         = A.String "workspace/executeCommand"
  -- Document
  toJSON TextDocumentDidOpen             = A.String "textDocument/didOpen"
  toJSON TextDocumentDidChange           = A.String "textDocument/didChange"
  toJSON TextDocumentWillSave            = A.String "textDocument/willSave"
  toJSON TextDocumentWillSaveWaitUntil   = A.String "textDocument/willSaveWaitUntil"
  toJSON TextDocumentDidSave             = A.String "textDocument/didSave"
  toJSON TextDocumentDidClose            = A.String "textDocument/didClose"
  toJSON TextDocumentCompletion          = A.String "textDocument/completion"
  toJSON CompletionItemResolve           = A.String "completionItem/resolve"
  toJSON TextDocumentHover               = A.String "textDocument/hover"
  toJSON TextDocumentSignatureHelp       = A.String "textDocument/signatureHelp"
  toJSON TextDocumentReferences          = A.String "textDocument/references"
  toJSON TextDocumentDocumentHighlight   = A.String "textDocument/documentHighlight"
  toJSON TextDocumentDocumentSymbol      = A.String "textDocument/documentSymbol"
  toJSON TextDocumentDefinition          = A.String "textDocument/definition"
  toJSON TextDocumentTypeDefinition      = A.String "textDocument/typeDefinition"
  toJSON TextDocumentImplementation      = A.String "textDocument/implementation"
  toJSON TextDocumentCodeAction          = A.String "textDocument/codeAction"
  toJSON TextDocumentCodeLens            = A.String "textDocument/codeLens"
  toJSON CodeLensResolve                 = A.String "codeLens/resolve"
  toJSON TextDocumentDocumentColor       = A.String "textDocument/documentColor"
  toJSON TextDocumentColorPresentation   = A.String "textDocument/colorPresentation"
  toJSON TextDocumentFormatting          = A.String "textDocument/formatting"
  toJSON TextDocumentRangeFormatting     = A.String "textDocument/rangeFormatting"
  toJSON TextDocumentOnTypeFormatting    = A.String "textDocument/onTypeFormatting"
  toJSON TextDocumentRename              = A.String "textDocument/rename"
  toJSON TextDocumentPrepareRename       = A.String "textDocument/prepareRename"
  toJSON TextDocumentFoldingRange        = A.String "textDocument/foldingRange"
  toJSON TextDocumentDocumentLink        = A.String "textDocument/documentLink"
  toJSON DocumentLinkResolve             = A.String "documentLink/resolve"
  toJSON WorkDoneProgressCancel          = A.String "window/workDoneProgress/cancel"
  toJSON (CustomClientMethod xs)         = A.String xs

data ServerMethod =
  -- Window
    WindowShowMessage
  | WindowShowMessageRequest
  | WindowLogMessage
  | WindowWorkDoneProgressCreate
  | Progress
  | TelemetryEvent
  -- Client
  | ClientRegisterCapability
  | ClientUnregisterCapability
  -- Workspace
  | WorkspaceWorkspaceFolders
  | WorkspaceConfiguration
  | WorkspaceApplyEdit
  -- Document
  | TextDocumentPublishDiagnostics
  -- Cancelling
  | CancelRequestServer
  | CustomServerMethod Text
   deriving (Eq,Ord,Read,Show)

instance A.FromJSON ServerMethod where
  -- Window
  parseJSON (A.String "window/showMessage")              = return WindowShowMessage
  parseJSON (A.String "window/showMessageRequest")       = return WindowShowMessageRequest
  parseJSON (A.String "window/logMessage")               = return WindowLogMessage
  parseJSON (A.String "window/workDoneProgress/create")  = return WindowWorkDoneProgressCreate
  parseJSON (A.String "$/progress")                      = return Progress
  parseJSON (A.String "telemetry/event")                 = return TelemetryEvent
  -- Client
  parseJSON (A.String "client/registerCapability")       = return ClientRegisterCapability
  parseJSON (A.String "client/unregisterCapability")     = return ClientUnregisterCapability
  -- Workspace
  parseJSON (A.String "workspace/workspaceFolders")      = return WorkspaceWorkspaceFolders
  parseJSON (A.String "workspace/configuration")         = return WorkspaceConfiguration
  parseJSON (A.String "workspace/applyEdit")             = return WorkspaceApplyEdit
  -- Document
  parseJSON (A.String "textDocument/publishDiagnostics") = return TextDocumentPublishDiagnostics
  -- Cancelling
  parseJSON (A.String "$/cancelRequest")                 = return CancelRequestServer
  parseJSON (A.String m)                                 = return (CustomServerMethod m)
  parseJSON _                                            = mempty

instance A.ToJSON ServerMethod where
  -- Window
  toJSON WindowShowMessage        = A.String "window/showMessage"
  toJSON WindowShowMessageRequest = A.String "window/showMessageRequest"
  toJSON WindowLogMessage         = A.String "window/logMessage"
  toJSON WindowWorkDoneProgressCreate = A.String "window/workDoneProgress/create"
  toJSON Progress                 = A.String "$/progress"
  toJSON TelemetryEvent           = A.String "telemetry/event"
  -- Client
  toJSON ClientRegisterCapability   = A.String "client/registerCapability"
  toJSON ClientUnregisterCapability = A.String "client/unregisterCapability"
  -- Workspace
  toJSON WorkspaceWorkspaceFolders = A.String "workspace/workspaceFolders"
  toJSON WorkspaceConfiguration    = A.String "workspace/configuration"
  toJSON WorkspaceApplyEdit        = A.String "workspace/applyEdit"
  -- Document
  toJSON TextDocumentPublishDiagnostics = A.String "textDocument/publishDiagnostics"
  -- Cancelling
  toJSON CancelRequestServer = A.String "$/cancelRequest"
  toJSON (CustomServerMethod m) = A.String m

data RequestMessage m req resp =
  RequestMessage
    { _jsonrpc :: Text
    , _id      :: LspId
    , _method  :: m
    , _params  :: req
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''RequestMessage

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
        // Defined by JSON RPC
        export const ParseError: number = -32700;
        export const InvalidRequest: number = -32600;
        export const MethodNotFound: number = -32601;
        export const InvalidParams: number = -32602;
        export const InternalError: number = -32603;
        export const serverErrorStart: number = -32099;
        export const serverErrorEnd: number = -32000;
        export const ServerNotInitialized: number = -32002;
        export const UnknownErrorCode: number = -32001;

        // Defined by the protocol.
        export const RequestCancelled: number = -32800;
}
-}

data ErrorCode = ParseError
               | InvalidRequest
               | MethodNotFound
               | InvalidParams
               | InternalError
               | ServerErrorStart
               | ServerErrorEnd
               | ServerNotInitialized
               | UnknownErrorCode
               | RequestCancelled
               -- ^ Note: server error codes are reserved from -32099 to -32000
               deriving (Read,Show,Eq)

instance A.ToJSON ErrorCode where
  toJSON ParseError           = A.Number (-32700)
  toJSON InvalidRequest       = A.Number (-32600)
  toJSON MethodNotFound       = A.Number (-32601)
  toJSON InvalidParams        = A.Number (-32602)
  toJSON InternalError        = A.Number (-32603)
  toJSON ServerErrorStart     = A.Number (-32099)
  toJSON ServerErrorEnd       = A.Number (-32000)
  toJSON ServerNotInitialized = A.Number (-32002)
  toJSON UnknownErrorCode     = A.Number (-32001)
  toJSON RequestCancelled     = A.Number (-32800)

instance A.FromJSON ErrorCode where
  parseJSON (A.Number (-32700)) = pure ParseError
  parseJSON (A.Number (-32600)) = pure InvalidRequest
  parseJSON (A.Number (-32601)) = pure MethodNotFound
  parseJSON (A.Number (-32602)) = pure InvalidParams
  parseJSON (A.Number (-32603)) = pure InternalError
  parseJSON (A.Number (-32099)) = pure ServerErrorStart
  parseJSON (A.Number (-32000)) = pure ServerErrorEnd
  parseJSON (A.Number (-32002)) = pure ServerNotInitialized
  parseJSON (A.Number (-32001)) = pure UnknownErrorCode
  parseJSON (A.Number (-32800)) = pure RequestCancelled
  parseJSON _                   = mempty

-- -------------------------------------

data ResponseError =
  ResponseError
    { _code    :: ErrorCode
    , _message :: Text
    , _xdata   :: Maybe A.Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ResponseError

-- ---------------------------------------------------------------------

data ResponseMessage a =
  ResponseMessage
    { _jsonrpc :: Text
    , _id      :: LspIdRsp
    , _result  :: Maybe a
    , _error   :: Maybe ResponseError
    } deriving (Read,Show,Eq)

deriveToJSON lspOptions ''ResponseMessage

instance FromJSON a => FromJSON (ResponseMessage a) where
  parseJSON = withObject "Response" $ \o ->
    ResponseMessage
      <$> o .: "jsonrpc"
      <*> o .: "id"
      -- It is important to use .:! so that result = null gets decoded as Just Nothing
      <*> o .:! "result"
      <*> o .:! "error"

type ErrorResponse = ResponseMessage ()

-- ---------------------------------------------------------------------

type BareResponseMessage = ResponseMessage A.Value

-- ---------------------------------------------------------------------
{-
$ Notifications and Requests

Notification and requests ids starting with '$/' are messages which are protocol
implementation dependent and might not be implementable in all clients or
servers. For example if the server implementation uses a single threaded
synchronous programming language then there is little a server can do to react
to a '$/cancelRequest'. If a server or client receives notifications or requests
starting with '$/' it is free to ignore them if they are unknown.
-}

data NotificationMessage m a =
  NotificationMessage
    { _jsonrpc :: Text
    , _method  :: m
    , _params  :: a
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''NotificationMessage

-- ---------------------------------------------------------------------
{-
Cancellation Support

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#cancellation-support

    New: The base protocol now offers support for request cancellation. To
    cancel a request, a notification message with the following properties is
    sent:

Notification:

    method: '$/cancelRequest'
    params: CancelParams defined as follows:

interface CancelParams {
    /**
     * The request id to cancel.
     */
    id: number | string;
}

A request that got canceled still needs to return from the server and send a
response back. It can not be left open / hanging. This is in line with the JSON
RPC protocol that requires that every request sends a response back. In addition
it allows for returning partial results on cancel.
-}

data CancelParams =
  CancelParams
    { _id :: LspId
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CancelParams

type CancelNotification = NotificationMessage ClientMethod CancelParams
type CancelNotificationServer = NotificationMessage ServerMethod CancelParams

-- ---------------------------------------------------------------------
