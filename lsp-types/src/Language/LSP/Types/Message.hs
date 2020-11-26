{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.LSP.Types.Message where

import           Language.LSP.Types.Cancellation
import           Language.LSP.Types.CodeAction
import           Language.LSP.Types.CodeLens
import           Language.LSP.Types.Command
import           Language.LSP.Types.Common
import           Language.LSP.Types.Configuration
import           Language.LSP.Types.Completion
import           Language.LSP.Types.Declaration
import           Language.LSP.Types.Definition
import           Language.LSP.Types.Diagnostic
import           Language.LSP.Types.DocumentColor
import           Language.LSP.Types.DocumentHighlight
import           Language.LSP.Types.DocumentLink
import           Language.LSP.Types.DocumentSymbol
import           Language.LSP.Types.FoldingRange
import           Language.LSP.Types.Formatting
import           Language.LSP.Types.Hover
import           Language.LSP.Types.Implementation
import           Language.LSP.Types.Initialize
import           Language.LSP.Types.Location
import           Language.LSP.Types.LspId
import           Language.LSP.Types.Method
import           Language.LSP.Types.Progress
import           Language.LSP.Types.Registration
import           Language.LSP.Types.Rename
import           Language.LSP.Types.References
import           Language.LSP.Types.SelectionRange
import           Language.LSP.Types.SignatureHelp
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.TypeDefinition
import           Language.LSP.Types.Utils
import           Language.LSP.Types.Window
import           Language.LSP.Types.WatchedFiles
import           Language.LSP.Types.WorkspaceEdit
import           Language.LSP.Types.WorkspaceFolders
import           Language.LSP.Types.WorkspaceSymbol
import qualified Data.HashMap.Strict as HM

import Data.Kind
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.GADT.Compare
import Data.Text (Text)
import Data.Type.Equality
import Data.Function (on)
import GHC.Generics

-- ---------------------------------------------------------------------
-- PARAMS definition
-- Map Methods to params/responses
-- ---------------------------------------------------------------------

-- | Map a method to the message payload type
type family MessageParams (m :: Method f t) :: Type where
-- Client
  -- General
  MessageParams Initialize                         = InitializeParams
  MessageParams Initialized                        = Maybe InitializedParams
  MessageParams Shutdown                           = Empty
  MessageParams Exit                               = Empty
  -- Workspace
  MessageParams WorkspaceDidChangeWorkspaceFolders = DidChangeWorkspaceFoldersParams
  MessageParams WorkspaceDidChangeConfiguration    = DidChangeConfigurationParams
  MessageParams WorkspaceDidChangeWatchedFiles     = DidChangeWatchedFilesParams
  MessageParams WorkspaceSymbol                    = WorkspaceSymbolParams
  MessageParams WorkspaceExecuteCommand            = ExecuteCommandParams
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
  MessageParams TextDocumentHover                  = HoverParams
  MessageParams TextDocumentSignatureHelp          = SignatureHelpParams
  MessageParams TextDocumentDeclaration            = DeclarationParams
  MessageParams TextDocumentDefinition             = DefinitionParams
  MessageParams TextDocumentTypeDefinition         = TypeDefinitionParams
  MessageParams TextDocumentImplementation         = ImplementationParams
  MessageParams TextDocumentReferences             = ReferenceParams
  MessageParams TextDocumentDocumentHighlight      = DocumentHighlightParams
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
  MessageParams TextDocumentPrepareRename          = PrepareRenameParams
  -- Folding Range
  MessageParams TextDocumentFoldingRange           = FoldingRangeParams
  -- Selection Range
  MessageParams TextDocumentSelectionRange         = SelectionRangeParams
-- Server
    -- Window
  MessageParams WindowShowMessage                  = ShowMessageParams
  MessageParams WindowShowMessageRequest           = ShowMessageRequestParams
  MessageParams WindowLogMessage                   = LogMessageParams
  -- Progress
  MessageParams WindowWorkDoneProgressCreate       = WorkDoneProgressCreateParams
  MessageParams WindowWorkDoneProgressCancel       = WorkDoneProgressCancelParams
  MessageParams Progress                           = ProgressParams SomeProgressParams
  -- Telemetry
  MessageParams TelemetryEvent                     = Value
  -- Client
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
type family ResponseResult (m :: Method f Request) :: Type where
-- Even though the specification mentions that the result types are
-- @x | y | ... | null@, they don't actually need to be wrapped in a Maybe since
-- (we think) this is just to account for how the response field is always
-- nullable. I.e. if it is null, then the error field is set

-- Client
  -- General
  ResponseResult Initialize                    = InitializeResult
  ResponseResult Shutdown                      = Empty
  -- Workspace
  ResponseResult WorkspaceSymbol               = List SymbolInformation
  ResponseResult WorkspaceExecuteCommand       = Value
  -- Sync/Document state
  ResponseResult TextDocumentWillSaveWaitUntil = List TextEdit
  -- Completion
  ResponseResult TextDocumentCompletion        = List CompletionItem |? CompletionList
  ResponseResult CompletionItemResolve         = CompletionItem
  -- Language Queries
  ResponseResult TextDocumentHover             = Maybe Hover
  ResponseResult TextDocumentSignatureHelp     = SignatureHelp
  ResponseResult TextDocumentDeclaration       = Location |? List Location |? List LocationLink
  ResponseResult TextDocumentDefinition        = Location |? List Location |? List LocationLink
  ResponseResult TextDocumentTypeDefinition    = Location |? List Location |? List LocationLink
  ResponseResult TextDocumentImplementation    = Location |? List Location |? List LocationLink
  ResponseResult TextDocumentReferences        = List Location
  ResponseResult TextDocumentDocumentHighlight = List DocumentHighlight
  ResponseResult TextDocumentDocumentSymbol    = List DocumentSymbol |? List SymbolInformation
  -- Code Action/Lens/Link
  ResponseResult TextDocumentCodeAction        = List (Command |? CodeAction)
  ResponseResult TextDocumentCodeLens          = List CodeLens
  ResponseResult CodeLensResolve               = CodeLens
  ResponseResult TextDocumentDocumentLink      = List DocumentLink
  ResponseResult DocumentLinkResolve           = DocumentLink
  -- Syntax highlighting/coloring
  ResponseResult TextDocumentDocumentColor     = List ColorInformation
  ResponseResult TextDocumentColorPresentation = List ColorPresentation
  -- Formatting
  ResponseResult TextDocumentFormatting        = List TextEdit
  ResponseResult TextDocumentRangeFormatting   = List TextEdit
  ResponseResult TextDocumentOnTypeFormatting  = List TextEdit
  -- Rename
  ResponseResult TextDocumentRename            = WorkspaceEdit
  ResponseResult TextDocumentPrepareRename     = Maybe (Range |? RangeWithPlaceholder)
  -- FoldingRange
  ResponseResult TextDocumentFoldingRange      = List FoldingRange
  ResponseResult TextDocumentSelectionRange    = List SelectionRange
  -- Custom can be either a notification or a message
-- Server
  -- Window
  ResponseResult WindowShowMessageRequest      = Maybe MessageActionItem
  ResponseResult WindowWorkDoneProgressCreate  = ()
  -- Capability
  ResponseResult ClientRegisterCapability      = Empty
  ResponseResult ClientUnregisterCapability    = Empty
  -- Workspace
  ResponseResult WorkspaceWorkspaceFolders     = Maybe (List WorkspaceFolder)
  ResponseResult WorkspaceConfiguration        = List Value
  ResponseResult WorkspaceApplyEdit            = ApplyWorkspaceEditResponseBody
-- Custom
  ResponseResult CustomMethod                  = Value


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

data NotificationMessage (m :: Method f Notification) =
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

data RequestMessage (m :: Method f Request) = RequestMessage
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

-- | A custom message data type is needed to distinguish between
-- notifications and requests, since a CustomMethod can be both!
data CustomMessage f t where
  ReqMess :: RequestMessage (CustomMethod :: Method f Request) -> CustomMessage f Request
  NotMess :: NotificationMessage (CustomMethod :: Method f Notification) -> CustomMessage f Notification

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

data ResponseError =
  ResponseError
    { _code    :: ErrorCode
    , _message :: Text
    , _xdata   :: Maybe Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ResponseError

-- | Either result or error must be Just.
data ResponseMessage (m :: Method f Request) =
  ResponseMessage
    { _jsonrpc :: Text
    , _id      :: Maybe (LspId m)
    , _result  :: Either ResponseError (ResponseResult m)
    } deriving Generic

deriving instance Eq   (ResponseResult m) => Eq (ResponseMessage m)
deriving instance Read (ResponseResult m) => Read (ResponseMessage m)
deriving instance Show (ResponseResult m) => Show (ResponseMessage m)

instance (ToJSON (ResponseResult m)) => ToJSON (ResponseMessage m) where
  toJSON (ResponseMessage { _jsonrpc = jsonrpc, _id = lspid, _result = result })
    = object
      [ "jsonrpc" .= jsonrpc
      , "id" .= lspid
      , case result of
        Left  err -> "error" .= err
        Right a   -> "result" .= a
      ]

instance FromJSON (ResponseResult a) => FromJSON (ResponseMessage a) where
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
type family Message (m :: Method f t) :: Type where
  Message (CustomMethod :: Method f t) = CustomMessage f t
  Message (m :: Method f Request) = RequestMessage m
  Message (m :: Method f Notification) = NotificationMessage m

-- Some helpful type synonyms
type ClientMessage (m :: Method FromClient t) = Message m
type ServerMessage (m :: Method FromServer t) = Message m

-- ---------------------------------------------------------------------
-- Working with arbritary messages
-- ---------------------------------------------------------------------

data FromServerMessage' a where
  FromServerMess :: forall t (m :: Method FromServer t) a. SMethod m -> Message m -> FromServerMessage' a
  FromServerRsp  :: forall (m :: Method FromClient Request) a. a m -> ResponseMessage m -> FromServerMessage' a

type FromServerMessage = FromServerMessage' SMethod

instance Eq FromServerMessage where
  (==) = (==) `on` toJSON
instance Show FromServerMessage where
  show = show . toJSON

instance ToJSON FromServerMessage where
  toJSON (FromServerMess m p) = serverMethodJSON m (toJSON p)
  toJSON (FromServerRsp m p) = clientResponseJSON m (toJSON p)

fromServerNot :: forall (m :: Method FromServer Notification).
  Message m ~ NotificationMessage m => NotificationMessage m -> FromServerMessage
fromServerNot m@NotificationMessage{_method=meth} = FromServerMess meth m

fromServerReq :: forall (m :: Method FromServer Request).
  Message m ~ RequestMessage m => RequestMessage m -> FromServerMessage
fromServerReq m@RequestMessage{_method=meth} = FromServerMess meth m

data FromClientMessage' a where
  FromClientMess :: forall t (m :: Method FromClient t) a. SMethod m -> Message m -> FromClientMessage' a
  FromClientRsp  :: forall (m :: Method FromServer Request) a. a m -> ResponseMessage m -> FromClientMessage' a

type FromClientMessage = FromClientMessage' SMethod

instance ToJSON FromClientMessage where
  toJSON (FromClientMess m p) = clientMethodJSON m (toJSON p)
  toJSON (FromClientRsp m p) = serverResponseJSON m (toJSON p)

fromClientNot :: forall (m :: Method FromClient Notification).
  Message m ~ NotificationMessage m => NotificationMessage m -> FromClientMessage
fromClientNot m@NotificationMessage{_method=meth} = FromClientMess meth m

fromClientReq :: forall (m :: Method FromClient Request).
  Message m ~ RequestMessage m => RequestMessage m -> FromClientMessage
fromClientReq m@RequestMessage{_method=meth} = FromClientMess meth m

type LookupFunc f a = forall (m :: Method f Request). LspId m -> Maybe (SMethod m, a m)

{-
Message Types we must handle are the following
 
Request      | jsonrpc | id | method | params?
Response     | jsonrpc | id |        |         | response? | error?
Notification | jsonrpc |    | method | params?
-}

parseServerMessage :: LookupFunc FromClient a -> Value -> Parser (FromServerMessage' a)
parseServerMessage lookupId v@(Object o) = do
  case HM.lookup "method" o of
    Just cmd -> do
      -- Request or Notification
      SomeServerMethod m <- parseJSON cmd
      case splitServerMethod m of
        IsServerNot -> FromServerMess m <$> parseJSON v
        IsServerReq -> FromServerMess m <$> parseJSON v
        IsServerEither
          | HM.member "id" o -- Request
          , SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Request))
                  in FromServerMess m' <$> parseJSON v
          | SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Notification))
                  in FromServerMess m' <$> parseJSON v
    Nothing -> do
      case HM.lookup "id" o of
        Just i' -> do
          i <- parseJSON i'
          case lookupId i of
            Just (m,res) -> clientResponseJSON m $ FromServerRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:",show v]

parseClientMessage :: LookupFunc FromServer a -> Value -> Parser (FromClientMessage' a)
parseClientMessage lookupId v@(Object o) = do
  case HM.lookup "method" o of
    Just cmd -> do
      -- Request or Notification
      SomeClientMethod m <- parseJSON cmd
      case splitClientMethod m of
        IsClientNot -> FromClientMess m <$> parseJSON v
        IsClientReq -> FromClientMess m <$> parseJSON v
        IsClientEither
          | HM.member "id" o -- Request
          , SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Request))
                  in FromClientMess m' <$> parseJSON v
          | SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Notification))
                  in FromClientMess m' <$> parseJSON v
    Nothing -> do
      case HM.lookup "id" o of
        Just i' -> do
          i <- parseJSON i'
          case lookupId i of
            Just (m,res) -> serverResponseJSON m $ FromClientRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:",show v]

-- ---------------------------------------------------------------------
-- Helper Utilities
-- ---------------------------------------------------------------------

clientResponseJSON :: SClientMethod m -> (HasJSON (ResponseMessage m) => x) -> x
clientResponseJSON m x = case splitClientMethod m of
  IsClientReq -> x
  IsClientEither -> x

serverResponseJSON :: SServerMethod m -> (HasJSON (ResponseMessage m) => x) -> x
serverResponseJSON m x = case splitServerMethod m of
  IsServerReq -> x
  IsServerEither -> x

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
       , Message m ~ NotificationMessage m)
    => ClientNotOrReq (m :: Method FromClient Notification)
  IsClientReq
    :: forall (m :: Method FromClient Request).
    ( HasJSON (ClientMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m)
    => ClientNotOrReq m
  IsClientEither
    :: ClientNotOrReq CustomMethod

data ServerNotOrReq (m :: Method FromServer t) where
  IsServerNot
    :: ( HasJSON (ServerMessage m)
       , Message m ~ NotificationMessage m)
    => ServerNotOrReq (m :: Method FromServer Notification)
  IsServerReq
    :: forall (m :: Method FromServer Request).
    ( HasJSON (ServerMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m)
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
splitClientMethod SWindowWorkDoneProgressCancel = IsClientNot
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
splitClientMethod STextDocumentDeclaration = IsClientReq
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
splitClientMethod STextDocumentSelectionRange = IsClientReq
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

-- | Heterogeneous equality on singleton server methods
mEqServer :: SServerMethod m1 -> SServerMethod m2 -> Maybe (m1 :~~: m2)
mEqServer m1 m2 = case (splitServerMethod m1, splitServerMethod m2) of
  (IsServerNot, IsServerNot) -> do
    Refl <- geq m1 m2
    pure HRefl
  (IsServerReq, IsServerReq) -> do
    Refl <- geq m1 m2
    pure HRefl
  _ -> Nothing

-- | Heterogeneous equality on singlton client methods
mEqClient :: SClientMethod m1 -> SClientMethod m2 -> Maybe (m1 :~~: m2)
mEqClient m1 m2 = case (splitClientMethod m1, splitClientMethod m2) of
  (IsClientNot, IsClientNot) -> do
    Refl <- geq m1 m2
    pure HRefl
  (IsClientReq, IsClientReq) -> do
    Refl <- geq m1 m2
    pure HRefl
  _ -> Nothing
