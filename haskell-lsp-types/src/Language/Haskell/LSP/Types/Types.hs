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

module Language.Haskell.LSP.Types.Types where

import           Language.Haskell.LSP.Types.DataTypesJSON
import           Language.Haskell.LSP.Types.CodeAction
import           Language.Haskell.LSP.Types.Color
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Completion
import           Language.Haskell.LSP.Types.FoldingRange
import           Language.Haskell.LSP.Types.Hover
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Message
import           Language.Haskell.LSP.Types.Symbol
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Window
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders

import Data.Kind
import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Monad
import GHC.Generics

-- ---------------------------------------------------------------------
-- PARAMS
-- ---------------------------------------------------------------------

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

-- | Either result or error must be Just.
data ResponseMessage (m :: Method p Request) =
  ResponseMessage
    { _jsonrpc :: Text
    , _id      :: Maybe (LspId m)
    , _result  :: Maybe (ResponseParams m)
    , _error   :: Maybe ResponseError
    } deriving Generic

deriving instance Eq   (ResponseParams m) => Eq (ResponseMessage m)
deriving instance Read (ResponseParams m) => Read (ResponseMessage m)
deriving instance Show (ResponseParams m) => Show (ResponseMessage m)

instance (ToJSON (ResponseParams m)) => ToJSON (ResponseMessage m) where
  toJSON     = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

instance FromJSON (ResponseParams a) => FromJSON (ResponseMessage a) where
  parseJSON = withObject "Response" $ \o -> do
    rsp <- ResponseMessage
      <$> o .: "jsonrpc"
      <*> o .: "id"
      -- It is important to use .:! so that result = null gets decoded as Just Nothing
      <*> o .:! "result"
      <*> o .:! "error"
    -- We make sure that one of them is present. Without this check we can end up
    -- parsing a Request as a ResponseMessage.
    unless (isJust (_result rsp) || isJust (_error rsp)) $
      fail "ResponseMessage must either have a result or an error"
    pure rsp

-- TODO
-- type ErrorResponse = ResponseMessage ()

-- ---------------------------------------------------------------------

-- TODO
-- type BareResponseMessage = ResponseMessage A.Value

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


type family Message (m :: Method p t) :: Type where
  Message (CustomMethod :: Method p t) = CustomMessage p t
  Message (m :: Method p t) = BaseMessage t m

type family BaseMessage (t :: MethodType) :: Method p t -> Type where
  BaseMessage Request = RequestMessage
  BaseMessage Notification = NotificationMessage

-- Server Messages

-- Window
type ShowMessageNotification = Message WindowShowMessage
type ShowMessageRequest = Message WindowShowMessageRequest
type ShowMessageResponse = ResponseMessage WindowShowMessageRequest
type LogMessageNotification = Message WindowLogMessage

-- | The window/progress/start notification is sent from the server to the
-- client to ask the client to start progress.
--
-- @since 0.10.0.0
-- type ProgressStartNotification = ServerMessage WindowProgressStart

-- | The window/progress/report notification is sent from the server to the
-- client to report progress for a previously started progress.
--
-- @since 0.10.0.0
-- type ProgressReportNotification = ServerMessage WindowProgressReport

-- | The window/progress/done notification is sent from the server to the
-- client to stop a previously started progress.
--
-- @since 0.10.0.0
-- type ProgressDoneNotification = ServerMessage WindowProgressDone

type TelemetryNotification = Message TelemetryEvent

-- Capability
type RegisterCapabilityRequest = Message ClientRegisterCapability
type RegisterCapabilityResponse = ResponseMessage ClientRegisterCapability
type UnregisterCapabilityRequest = Message ClientUnregisterCapability
type UnregisterCapabilityResponse = ResponseMessage ClientUnregisterCapability

-- Workspace
type WorkspaceFoldersRequest = Message WorkspaceWorkspaceFolders
type WorkspaceFoldersResponse = ResponseMessage WorkspaceWorkspaceFolders
type ConfigurationRequest = Message WorkspaceConfiguration
type ConfigurationResponse = ResponseMessage WorkspaceConfiguration
type ApplyWorkspaceEditRequest  = Message WorkspaceApplyEdit
type ApplyWorkspaceEditResponse = ResponseMessage WorkspaceApplyEdit

-- Document/Diagnostic
type PublishDiagnosticsNotification = Message TextDocumentPublishDiagnostics

-- Cancel
type CancelNotificationServer = Message CancelRequest

-- Custom
type CustomServerNotification = Message CustomMethod
type CustomServerRequest = Message CustomMethod

-- Client Messages

-- General
type InitializeRequest       = Message Initialize
type InitializeResponse      = ResponseMessage Initialize
type InitializedNotification = Message Initialized
type ShutdownRequest         = Message Shutdown
type ShutdownResponse        = ResponseMessage Shutdown
type ExitNotification        = Message Exit
type CancelNotification      = Message CancelRequest

-- Workspace
type DidChangeWorkspaceFoldersNotification = Message WorkspaceDidChangeWorkspaceFolders
type DidChangeConfigurationNotification    = Message WorkspaceDidChangeConfiguration 
type DidChangeWatchedFilesNotification     = Message WorkspaceDidChangeWatchedFiles 
type WorkspaceSymbolRequest                = Message WorkspaceSymbol 
type WorkspaceSymbolsResponse              = ResponseMessage WorkspaceSymbol 
type ExecuteCommandRequest                 = Message WorkspaceExecuteCommand 
type ExecuteCommandResponse                = ResponseMessage WorkspaceExecuteCommand 

-- Progress

-- | The window/progress/cancel notification is sent from the client to the server
-- to inform the server that the user has pressed the cancel button on the progress UX.
-- A server receiving a cancel request must still close a progress using the done notification.
--
-- @since 0.10.0.0
type WorkDoneProgressCreateRequest      = Message WindowWorkDoneProgressCreate
type WorkDoneProgressCancelNotification = Message WorkDoneProgressCancel
type WorkDoneProgressBeginNotification  = Message Progress
type WorkDoneProgressReportNotification = Message Progress
type WorkDoneProgressEndNotification    = Message Progress

-- Document/Sync
type DidOpenTextDocumentNotification       = Message TextDocumentDidOpen
type DidChangeTextDocumentNotification     = Message TextDocumentDidChange
type WillSaveTextDocumentNotification      = Message TextDocumentWillSave
type WillSaveWaitUntilTextDocumentRequest  = Message TextDocumentWillSaveWaitUntil
type WillSaveWaitUntilTextDocumentResponse = ResponseMessage TextDocumentWillSaveWaitUntil
type DidSaveTextDocumentNotification       = Message TextDocumentDidSave
type DidCloseTextDocumentNotification      = Message TextDocumentDidClose

-- Completion

type CompletionRequest  = Message TextDocumentCompletion
type CompletionResponse = ResponseMessage TextDocumentCompletion

type CompletionItemResolveRequest  = Message CompletionItemResolve
type CompletionItemResolveResponse = ResponseMessage CompletionItemResolve

-- Queries
type HoverRequest               = Message TextDocumentHover
type HoverResponse              = ResponseMessage TextDocumentHover
type SignatureHelpRequest       = Message TextDocumentSignatureHelp
type SignatureHelpResponse      = ResponseMessage TextDocumentSignatureHelp
type DefinitionRequest          = Message TextDocumentDefinition
type DefinitionResponse         = ResponseMessage TextDocumentDefinition
type TypeDefinitionRequest      = Message TextDocumentTypeDefinition
type TypeDefinitionResponse     = ResponseMessage TextDocumentTypeDefinition
type ImplementationRequest      = Message TextDocumentImplementation
type ImplementationResponse     = ResponseMessage TextDocumentImplementation
type ReferencesRequest          = Message TextDocumentReferences
type ReferencesResponse         = ResponseMessage TextDocumentReferences
type DocumentHighlightRequest   = Message TextDocumentDocumentHighlight
type DocumentHighlightsResponse = ResponseMessage TextDocumentDocumentHighlight
type DocumentSymbolRequest      = Message TextDocumentDocumentSymbol
type DocumentSymbolsResponse    = ResponseMessage TextDocumentDocumentSymbol

-- Code Lens/Action/Link

type CodeActionRequest  = Message TextDocumentCodeAction
type CodeActionResponse = ResponseMessage TextDocumentCodeAction

type CodeLensRequest  = Message TextDocumentCodeLens
type CodeLensResponse = ResponseMessage TextDocumentCodeLens
type CodeLensResolveRequest  = Message CodeLensResolve
type CodeLensResolveResponse = ResponseMessage CodeLensResolve

type DocumentLinkRequest  = Message TextDocumentDocumentLink
type DocumentLinkResponse = ResponseMessage TextDocumentDocumentLink
type DocumentLinkResolveRequest  = Message DocumentLinkResolve
type DocumentLinkResolveResponse = ResponseMessage DocumentLinkResolve

-- Color/Syntax

type DocumentColorRequest  = Message TextDocumentDocumentColor
type DocumentColorResponse = ResponseMessage TextDocumentDocumentColor

type ColorPresentationRequest  = Message TextDocumentColorPresentation
type ColorPresentationResponse = ResponseMessage TextDocumentColorPresentation


-- Formatting
type DocumentFormattingRequest  = Message TextDocumentFormatting
type DocumentFormattingResponse = ResponseMessage TextDocumentFormatting

type DocumentRangeFormattingRequest  = Message TextDocumentRangeFormatting
type DocumentRangeFormattingResponse = ResponseMessage TextDocumentRangeFormatting

type DocumentOnTypeFormattingRequest  = Message TextDocumentOnTypeFormatting
type DocumentOnTypeFormattingResponse = ResponseMessage TextDocumentOnTypeFormatting

-- Rename
type RenameRequest  = Message TextDocumentRename
type RenameResponse = ResponseMessage TextDocumentRename
type PrepareRenameRequest  = Message TextDocumentPrepareRename
type PrepareRenameResponse = ResponseMessage TextDocumentPrepareRename

-- Folding
type FoldingRangeRequest  = Message TextDocumentFoldingRange
type FoldingRangeResponse = ResponseMessage TextDocumentFoldingRange

-- Custom
type CustomClientNotification = Message CustomMethod

type CustomClientRequest = Message CustomMethod

type CustomResponse = ResponseMessage CustomMethod

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


type ClientMessage (m :: Method FromClient t) = Message m
type ServerMessage (m :: Method FromServer t) = Message m
type SClientMethod (m :: Method FromClient t) = SMethod m
type SServerMethod (m :: Method FromServer t) = SMethod m

type HandlerFunc a = Either ResponseError a -> IO ()

type family ResponseHandlerFunc m :: Type where
  ResponseHandlerFunc CustomMethod = Maybe (HandlerFunc Value)
  ResponseHandlerFunc (m :: Method p t) = BaseHandlerFunc t m

type family BaseHandlerFunc (t :: MethodType) (m :: Method p t) :: Type where
  BaseHandlerFunc Request m = HandlerFunc (ResponseParams m)
  BaseHandlerFunc Notification m = ()


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

-- ---------------------------------------------------------------------
{-
instance Show FromServerMessage where
  show = show . A.toJSON

instance Eq FromServerMessage where
  (FromServerMess m1 a) == (FromServerMess m2 b) = case geq m1 m2 of
    Nothing -> False
    Just Refl -> case splitServerMethod m1 of
      IsServerReq -> a == b
      IsServerNot -> a == b
      IsServerEither -> a == b
  (FromServerRsp m1 a) == (FromServerRsp m2 b) = case geq m1 m2 of
    Nothing -> False
    Just Refl -> case splitClientMethod m1 of
      IsClientReq -> a == b
      IsClientEither -> a == b
      IsClientNot -> error "impossible"
  _ == _ = False
instance A.ToJSON FromServerMessage where
  toJSON (FromServerMess m x) = case splitServerMethod m of
      IsServerReq -> A.toJSON x
      IsServerNot -> A.toJSON x
      IsServerEither -> A.toJSON x
  toJSON (FromServerRsp m x) = case splitClientMethod m of
      IsClientReq -> A.toJSON x
      IsClientEither -> A.toJSON x
      IsClientNot -> error "impossible"


instance Show FromClientMessage where
  show = show . A.toJSON

instance Eq FromClientMessage where
  (FromClientMess m1 a) == (FromClientMess m2 b) = case geq m1 m2 of
    Nothing -> False
    Just Refl -> case splitClientMethod m1 of
      IsClientReq -> a == b
      IsClientNot -> a == b
      IsClientEither -> a == b
  (FromClientRsp m1 a) == (FromClientRsp m2 b) = case geq m1 m2 of
    Nothing -> False
    Just Refl -> case splitServerMethod m1 of
      IsServerReq -> a == b
      IsServerEither -> a == b
      IsServerNot -> error "impossible"
  _ == _ = False
instance A.ToJSON FromClientMessage where
  toJSON (FromClientMess m x) = case splitClientMethod m of
      IsClientReq -> A.toJSON x
      IsClientNot -> A.toJSON x
      IsClientEither -> A.toJSON x
  toJSON (FromClientRsp m x) = case splitServerMethod m of
      IsServerReq -> A.toJSON x
      IsServerEither -> A.toJSON x
      IsServerNot -> error "impossible"


-- Proof that every ClientMessage has a To/FromJSON instance


type family Response (t :: Type) :: Type where
  Response (RequestMessage m req) = ResponseMessage m rsp
  Response (CustomMessage m) = ResponseMessage A.Value
  Response a = TypeError ('Text "Messages of type " :<>: ShowType a :<>:
                          'Text " cannot be responded too")
type family ServerResponse (m :: ClientMethod) :: Type where
  ServerResponse m = Response (ClientMessage m)
type family ClientResponse (m :: ServerMethod) :: Type where
  ClientResponse m = Response (ServerMessage m)




-}
