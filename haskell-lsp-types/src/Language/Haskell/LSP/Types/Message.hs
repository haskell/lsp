{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.Haskell.LSP.Types.Message where

import           Language.Haskell.LSP.Types.DataTypesJSON
import           Language.Haskell.LSP.Types.CodeAction
import           Language.Haskell.LSP.Types.Color
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Completion
import           Language.Haskell.LSP.Types.FoldingRange
import           Language.Haskell.LSP.Types.Hover
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.LspId
import           Language.Haskell.LSP.Types.Method
import           Language.Haskell.LSP.Types.Symbol
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Window
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders

import Data.Kind
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics

-- ---------------------------------------------------------------------
-- PARAMS definition
-- Map Methods to params/responses
-- ---------------------------------------------------------------------

-- | Map a method to the message payload type
type family MessageParams (m :: Method p t) :: Type where
  --   RequestMessage <method> <params> <response>
  -- | NotificationMessage <method> <params>
-- Client
  -- General
  MessageParams Initialize                         = InitializeParams
  MessageParams Initialized                        = Maybe InitializedParams
  MessageParams Shutdown                           = Maybe Value
  MessageParams Exit                               = Maybe ExitParams
  -- Workspace
  MessageParams WorkspaceDidChangeWorkspaceFolders = DidChangeWorkspaceFoldersParams
  MessageParams WorkspaceDidChangeConfiguration    = DidChangeConfigurationParams
  MessageParams WorkspaceDidChangeWatchedFiles     = DidChangeWatchedFilesParams
  MessageParams WorkspaceSymbol                    = WorkspaceSymbolParams 
  MessageParams WorkspaceExecuteCommand            = ExecuteCommandParams
  -- Progress
  MessageParams WorkDoneProgressCancel             = WorkDoneProgressCancelParams
  -- Sync/Document state
  MessageParams TextDocumentDidOpen                = DidOpenTextDocumentParams
  MessageParams TextDocumentDidChange              = DidChangeTextDocumentParams
  MessageParams TextDocumentWillSave               = WillSaveTextDocumentParams
  MessageParams TextDocumentWillSaveWaitUntil      = WillSaveTextDocumentParams 
  MessageParams TextDocumentDidSave                = DidSaveTextDocumentParams
  MessageParams TextDocumentDidClose               = DidCloseTextDocumentParams
  -- Completion
  MessageParams TextDocumentCompletion             = CompletionParams
  MessageParams CompletionItemResolve              = CompletionItem
  -- Language Queries
  MessageParams TextDocumentHover                  = TextDocumentPositionParams 
  MessageParams TextDocumentSignatureHelp          = TextDocumentPositionParams
  MessageParams TextDocumentDefinition             = TextDocumentPositionParams
  MessageParams TextDocumentTypeDefinition         = TextDocumentPositionParams
  MessageParams TextDocumentImplementation         = TextDocumentPositionParams
  MessageParams TextDocumentReferences             = ReferenceParams
  MessageParams TextDocumentDocumentHighlight      = TextDocumentPositionParams
  MessageParams TextDocumentDocumentSymbol         = DocumentSymbolParams
  -- Code Action/Lens/Link
  MessageParams TextDocumentCodeAction             = CodeActionParams 
  MessageParams TextDocumentCodeLens               = CodeLensParams
  MessageParams CodeLensResolve                    = CodeLens
  MessageParams TextDocumentDocumentLink           = DocumentLinkParams
  MessageParams DocumentLinkResolve                = DocumentLink
  -- Syntax highlighting/coloring
  MessageParams TextDocumentDocumentColor          = DocumentColorParams
  MessageParams TextDocumentColorPresentation      = ColorPresentationParams
  -- Formatting
  MessageParams TextDocumentFormatting             = DocumentFormattingParams
  MessageParams TextDocumentRangeFormatting        = DocumentRangeFormattingParams
  MessageParams TextDocumentOnTypeFormatting       = DocumentOnTypeFormattingParams
  -- Rename
  MessageParams TextDocumentRename                 = RenameParams
  MessageParams TextDocumentPrepareRename          = TextDocumentPositionParams
  -- FoldingRange
  MessageParams TextDocumentFoldingRange           = FoldingRangeParams
-- Server
    -- Window
  MessageParams WindowShowMessage                  = ShowMessageParams
  MessageParams WindowShowMessageRequest           = ShowMessageRequestParams
  MessageParams WindowLogMessage                   = LogMessageParams
  MessageParams WindowWorkDoneProgressCreate       = WorkDoneProgressCreateParams
  MessageParams Progress                           = ProgressParams SomeProgressParams
  MessageParams TelemetryEvent                     = Value
  -- Capability
  MessageParams ClientRegisterCapability           = RegistrationParams
  MessageParams ClientUnregisterCapability         = UnregistrationParams
  -- Workspace
  MessageParams WorkspaceWorkspaceFolders          = Empty
  MessageParams WorkspaceConfiguration             = ConfigurationParams
  MessageParams WorkspaceApplyEdit                 = ApplyWorkspaceEditParams
  -- Document/Diagnostic
  MessageParams TextDocumentPublishDiagnostics     = PublishDiagnosticsParams
  -- Cancel
  MessageParams CancelRequest                      = CancelParams
  -- Custom
  MessageParams CustomMethod                       = Value

-- | Map a request method to the response payload type
type family ResponseParams (m :: Method p Request) :: Type where
-- Client
  -- General
  ResponseParams Initialize                    = InitializeResponseCapabilities
  ResponseParams Shutdown                      = Empty
  -- Workspace
  ResponseParams WorkspaceSymbol               = List SymbolInformation
  ResponseParams WorkspaceExecuteCommand       = Value
  -- Sync/Document state
  ResponseParams TextDocumentWillSaveWaitUntil = List TextEdit
  -- Completion
  ResponseParams TextDocumentCompletion        = CompletionResponseResult
  ResponseParams CompletionItemResolve         = CompletionItem
  -- Language Queries
  ResponseParams TextDocumentHover             = Maybe Hover
  ResponseParams TextDocumentSignatureHelp     = SignatureHelp
  ResponseParams TextDocumentDefinition        = LocationResponseParams
  ResponseParams TextDocumentTypeDefinition    = LocationResponseParams
  ResponseParams TextDocumentImplementation    = LocationResponseParams
  ResponseParams TextDocumentReferences        = List Location
  ResponseParams TextDocumentDocumentHighlight = List DocumentHighlight
  ResponseParams TextDocumentDocumentSymbol    = DSResult
  -- Code Action/Lens/Link
  ResponseParams TextDocumentCodeAction        = List CAResult
  ResponseParams TextDocumentCodeLens          = List CodeLens
  ResponseParams CodeLensResolve               = CodeLens
  ResponseParams TextDocumentDocumentLink      = List DocumentLink
  ResponseParams DocumentLinkResolve           = DocumentLink
  -- Syntax highlighting/coloring
  ResponseParams TextDocumentDocumentColor     = List ColorInformation
  ResponseParams TextDocumentColorPresentation = List ColorPresentation
  -- Formatting
  ResponseParams TextDocumentFormatting        = List TextEdit
  ResponseParams TextDocumentRangeFormatting   = List TextEdit
  ResponseParams TextDocumentOnTypeFormatting  = List TextEdit
  -- Rename
  ResponseParams TextDocumentRename            = WorkspaceEdit
  ResponseParams TextDocumentPrepareRename     = Maybe RangeOrRangeWithPlaceholder
  -- FoldingRange
  ResponseParams TextDocumentFoldingRange      = List FoldingRange
  -- Custom can be either a notification or a message
-- Server
  -- Window
  ResponseParams WindowShowMessageRequest      = Maybe MessageActionItem
  ResponseParams WindowWorkDoneProgressCreate  = ()
  -- Capability
  ResponseParams ClientRegisterCapability      = Empty
  ResponseParams ClientUnregisterCapability    = Empty
  -- Workspace
  ResponseParams WorkspaceWorkspaceFolders     = Maybe (List WorkspaceFolder)
  ResponseParams WorkspaceConfiguration        = List Value
  ResponseParams WorkspaceApplyEdit            = ApplyWorkspaceEditResponseBody
-- Custom
  ResponseParams CustomMethod                  = Value

data Empty = Empty deriving (Eq,Ord,Show)
instance ToJSON Empty where
  toJSON Empty = Null
instance FromJSON Empty where
  parseJSON Null = pure Empty
  parseJSON _ = mempty


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

data NotificationMessage (m :: Method p Notification) =
  NotificationMessage
    { _jsonrpc :: Text
    , _method  :: SMethod m
    , _params  :: MessageParams m
    } deriving Generic

deriving instance Eq   (MessageParams m) => Eq (NotificationMessage m)
deriving instance Show (MessageParams m) => Show (NotificationMessage m)

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (NotificationMessage m) where
  parseJSON = genericParseJSON lspOptions
instance (ToJSON (MessageParams m)) => ToJSON (NotificationMessage m) where
  toJSON     = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

data RequestMessage (m :: Method p Request) = RequestMessage
    { _jsonrpc :: Text
    , _id      :: LspId m
    , _method  :: SMethod m
    , _params  :: MessageParams m
    } deriving Generic

deriving instance Eq   (MessageParams m) => Eq (RequestMessage m)
deriving instance (Read (SMethod m), Read (MessageParams m)) => Read (RequestMessage m)
deriving instance Show (MessageParams m) => Show (RequestMessage m)

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (RequestMessage m) where
  parseJSON = genericParseJSON lspOptions
instance (ToJSON (MessageParams m), FromJSON (SMethod m)) => ToJSON (RequestMessage m) where
  toJSON     = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

data CustomMessage p t where
  ReqMess :: RequestMessage (CustomMethod :: Method p Request) -> CustomMessage p Request
  NotMess :: NotificationMessage (CustomMethod :: Method p Notification) -> CustomMessage p Notification

deriving instance Show (CustomMessage p t)

instance ToJSON (CustomMessage p t) where
  toJSON (ReqMess a) = toJSON a
  toJSON (NotMess a) = toJSON a

instance FromJSON (CustomMessage p Request) where
  parseJSON v = ReqMess <$> parseJSON v
instance FromJSON (CustomMessage p Notification) where
  parseJSON v = NotMess <$> parseJSON v

-- ---------------------------------------------------------------------
-- Response Message
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
        export const ContentModified: number = -32801;
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
               | ContentModified
               -- ^ Note: server error codes are reserved from -32099 to -32000
               deriving (Read,Show,Eq)

instance ToJSON ErrorCode where
  toJSON ParseError           = Number (-32700)
  toJSON InvalidRequest       = Number (-32600)
  toJSON MethodNotFound       = Number (-32601)
  toJSON InvalidParams        = Number (-32602)
  toJSON InternalError        = Number (-32603)
  toJSON ServerErrorStart     = Number (-32099)
  toJSON ServerErrorEnd       = Number (-32000)
  toJSON ServerNotInitialized = Number (-32002)
  toJSON UnknownErrorCode     = Number (-32001)
  toJSON RequestCancelled     = Number (-32800)
  toJSON ContentModified      = Number (-32801)

instance FromJSON ErrorCode where
  parseJSON (Number (-32700)) = pure ParseError
  parseJSON (Number (-32600)) = pure InvalidRequest
  parseJSON (Number (-32601)) = pure MethodNotFound
  parseJSON (Number (-32602)) = pure InvalidParams
  parseJSON (Number (-32603)) = pure InternalError
  parseJSON (Number (-32099)) = pure ServerErrorStart
  parseJSON (Number (-32000)) = pure ServerErrorEnd
  parseJSON (Number (-32002)) = pure ServerNotInitialized
  parseJSON (Number (-32001)) = pure UnknownErrorCode
  parseJSON (Number (-32800)) = pure RequestCancelled
  parseJSON (Number (-32801)) = pure ContentModified
  parseJSON _                   = mempty

-- -------------------------------------

{-
  https://microsoft.github.io/language-server-protocol/specification#responseMessage

  interface ResponseError {
    /**
    * A number indicating the error type that occurred.
    */
    code: number;

    /**
    * A string providing a short description of the error.
    */
    message: string;

    /**
    * A primitive or structured value that contains additional
    * information about the error. Can be omitted.
    */
    data?: string | number | boolean | array | object | null;
  }
-}

data ResponseError =
  ResponseError
    { _code    :: ErrorCode
    , _message :: Text
    , _xdata   :: Maybe Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ResponseError

-- | Either result or error must be Just.
data ResponseMessage (m :: Method p Request) =
  ResponseMessage
    { _jsonrpc :: Text
    , _id      :: Maybe (LspId m)
    , _result  :: Either ResponseError (ResponseParams m)
    } deriving Generic

deriving instance Eq   (ResponseParams m) => Eq (ResponseMessage m)
deriving instance Read (ResponseParams m) => Read (ResponseMessage m)
deriving instance Show (ResponseParams m) => Show (ResponseMessage m)

instance (ToJSON (ResponseParams m)) => ToJSON (ResponseMessage m) where
  toJSON (ResponseMessage { _jsonrpc = jsonrpc, _id = lspid, _result = result })
    = object
      [ "jsonrpc" .= jsonrpc
      , "id" .= lspid
      , case result of
        Left  err -> "error" .= err
        Right a   -> "result" .= a
      ]

instance FromJSON (ResponseParams a) => FromJSON (ResponseMessage a) where
  parseJSON = withObject "Response" $ \o -> do
    _jsonrpc <- o .: "jsonrpc"
    _id      <- o .: "id"
    -- It is important to use .:! so that "result = null" (without error) gets decoded as Just Null
    _result  <- o .:! "result"
    _error   <- o .:? "error"
    result   <- case (_error, _result) of
      ((Just err), Nothing   ) -> pure $ Left err
      (Nothing   , (Just res)) -> pure $ Right res
      ((Just _err), (Just _res)) -> fail $ "both error and result cannot be present: " ++ show o
      (Nothing, Nothing) -> fail "both error and result cannot be Nothing"
    return $ ResponseMessage _jsonrpc _id $ result

-- ---------------------------------------------------------------------
-- Helper Type Families
-- ---------------------------------------------------------------------

-- | Map a method to the Request/Notification type with the correct
-- payload
type family Message (m :: Method p t) :: Type where
  Message (CustomMethod :: Method p t) = CustomMessage p t
  Message (m :: Method p t) = BaseMessage t m

type family BaseMessage (t :: MethodType) :: Method p t -> Type where
  BaseMessage Request = RequestMessage
  BaseMessage Notification = NotificationMessage

type HandlerFunc a = Either ResponseError a -> IO ()

-- | Map a method to a handler for its response
-- Either ResponseError (ResponseParams m) -> IO () for Requests
-- () for Notifications
-- This is a callback that will be invoked when your request
-- recieves a response
type family ResponseHandlerFunc m :: Type where
  ResponseHandlerFunc CustomMethod = Maybe (HandlerFunc Value)
  ResponseHandlerFunc (m :: Method p t) = BaseHandlerFunc t m

type family BaseHandlerFunc (t :: MethodType) (m :: Method p t) :: Type where
  BaseHandlerFunc Request m = HandlerFunc (ResponseParams m)
  BaseHandlerFunc Notification m = ()

-- Some helpful type synonyms
type ClientMessage (m :: Method FromClient t) = Message m
type ServerMessage (m :: Method FromServer t) = Message m
type SClientMethod (m :: Method FromClient t) = SMethod m
type SServerMethod (m :: Method FromServer t) = SMethod m

-- ---------------------------------------------------------------------
-- Working with arbritary messages
-- ---------------------------------------------------------------------

data FromServerMessage where
  FromServerMess :: forall t (m :: Method FromServer t). SMethod m -> Message m -> FromServerMessage
  FromServerRsp  :: forall (m :: Method FromClient Request). SMethod m -> ResponseMessage m -> FromServerMessage

instance ToJSON FromServerMessage where
  toJSON (FromServerMess m p) = serverMethodJSON m (toJSON p)
  toJSON (FromServerRsp m p) = clientResponseJSON m (toJSON p)

fromServerNot :: forall (m :: Method FromServer Notification).
  Message m ~ NotificationMessage m => NotificationMessage m -> FromServerMessage
fromServerNot m@NotificationMessage{_method=meth} = FromServerMess meth m

fromServerReq :: forall (m :: Method FromServer Request).
  Message m ~ RequestMessage m => RequestMessage m -> FromServerMessage
fromServerReq m@RequestMessage{_method=meth} = FromServerMess meth m

data FromClientMessage where
  FromClientMess :: forall t (m :: Method FromClient t). SMethod m -> Message m -> FromClientMessage
  FromClientRsp  :: forall m. SMethod m -> ResponseMessage m -> FromClientMessage


-- ---------------------------------------------------------------------
-- Helper Utilities
-- ---------------------------------------------------------------------

clientResponseJSON :: SClientMethod m -> (ToJSON (ResponseMessage m) => x) -> x
clientResponseJSON m x = case splitClientMethod m of
  IsClientReq -> x
  IsClientEither -> x

clientMethodJSON :: SClientMethod m -> (ToJSON (ClientMessage m) => x) -> x
clientMethodJSON m x =
  case splitClientMethod m of
    IsClientNot -> x
    IsClientReq -> x
    IsClientEither -> x

serverMethodJSON :: SServerMethod m -> (ToJSON (ServerMessage m) => x) -> x
serverMethodJSON m x =
  case splitServerMethod m of
    IsServerNot -> x
    IsServerReq -> x
    IsServerEither -> x

type HasJSON a = (ToJSON a,FromJSON a,Eq a)

-- Reify universal properties about Client/Server Messages

data ClientNotOrReq (m :: Method FromClient t) where
  IsClientNot
    :: ( HasJSON (ClientMessage m)
       , Message m ~ NotificationMessage m
       , ResponseHandlerFunc m ~ ())
    => ClientNotOrReq (m :: Method FromClient Notification)
  IsClientReq
    :: forall (m :: Method FromClient Request).
    ( HasJSON (ClientMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m
    , ResponseHandlerFunc m ~ HandlerFunc (ResponseParams m))
    => ClientNotOrReq m
  IsClientEither
    :: ClientNotOrReq CustomMethod

data ServerNotOrReq (m :: Method FromServer t) where
  IsServerNot
    :: ( HasJSON (ServerMessage m)
       , Message m ~ NotificationMessage m
       , ResponseHandlerFunc m ~ ())
    => ServerNotOrReq (m :: Method FromServer Notification)
  IsServerReq
    :: forall (m :: Method FromServer Request).
    ( HasJSON (ServerMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m
    , ResponseHandlerFunc m ~ HandlerFunc (ResponseParams m))
    => ServerNotOrReq m
  IsServerEither
    :: ServerNotOrReq CustomMethod

splitClientMethod :: SClientMethod m -> ClientNotOrReq m
splitClientMethod SInitialize = IsClientReq
splitClientMethod SInitialized = IsClientNot
splitClientMethod SShutdown = IsClientReq
splitClientMethod SExit = IsClientNot
splitClientMethod SWorkspaceDidChangeWorkspaceFolders = IsClientNot
splitClientMethod SWorkspaceDidChangeConfiguration = IsClientNot
splitClientMethod SWorkspaceDidChangeWatchedFiles = IsClientNot
splitClientMethod SWorkspaceSymbol = IsClientReq
splitClientMethod SWorkspaceExecuteCommand = IsClientReq
splitClientMethod SWorkDoneProgressCancel = IsClientNot
splitClientMethod STextDocumentDidOpen = IsClientNot
splitClientMethod STextDocumentDidChange = IsClientNot
splitClientMethod STextDocumentWillSave = IsClientNot
splitClientMethod STextDocumentWillSaveWaitUntil = IsClientReq
splitClientMethod STextDocumentDidSave = IsClientNot
splitClientMethod STextDocumentDidClose = IsClientNot
splitClientMethod STextDocumentCompletion = IsClientReq
splitClientMethod SCompletionItemResolve = IsClientReq
splitClientMethod STextDocumentHover = IsClientReq
splitClientMethod STextDocumentSignatureHelp = IsClientReq
splitClientMethod STextDocumentDefinition = IsClientReq
splitClientMethod STextDocumentTypeDefinition = IsClientReq
splitClientMethod STextDocumentImplementation = IsClientReq
splitClientMethod STextDocumentReferences = IsClientReq
splitClientMethod STextDocumentDocumentHighlight = IsClientReq
splitClientMethod STextDocumentDocumentSymbol = IsClientReq
splitClientMethod STextDocumentCodeAction = IsClientReq
splitClientMethod STextDocumentCodeLens = IsClientReq
splitClientMethod SCodeLensResolve = IsClientReq
splitClientMethod STextDocumentDocumentLink = IsClientReq
splitClientMethod SDocumentLinkResolve = IsClientReq
splitClientMethod STextDocumentDocumentColor = IsClientReq
splitClientMethod STextDocumentColorPresentation = IsClientReq
splitClientMethod STextDocumentFormatting = IsClientReq
splitClientMethod STextDocumentRangeFormatting = IsClientReq
splitClientMethod STextDocumentOnTypeFormatting = IsClientReq
splitClientMethod STextDocumentRename = IsClientReq
splitClientMethod STextDocumentPrepareRename = IsClientReq
splitClientMethod STextDocumentFoldingRange = IsClientReq
splitClientMethod SCancelRequest = IsClientNot
splitClientMethod SCustomMethod{} = IsClientEither

splitServerMethod :: SServerMethod m -> ServerNotOrReq m
splitServerMethod SWindowShowMessage = IsServerNot
splitServerMethod SWindowShowMessageRequest = IsServerReq
splitServerMethod SWindowLogMessage = IsServerNot
splitServerMethod SWindowWorkDoneProgressCreate = IsServerReq
splitServerMethod SProgress = IsServerNot
splitServerMethod STelemetryEvent = IsServerNot
splitServerMethod SClientRegisterCapability = IsServerReq
splitServerMethod SClientUnregisterCapability = IsServerReq
splitServerMethod SWorkspaceWorkspaceFolders = IsServerReq
splitServerMethod SWorkspaceConfiguration = IsServerReq
splitServerMethod SWorkspaceApplyEdit = IsServerReq
splitServerMethod STextDocumentPublishDiagnostics = IsServerNot
splitServerMethod SCancelRequest = IsServerNot
splitServerMethod SCustomMethod{} = IsServerEither
