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

import           Language.LSP.Types.CallHierarchy
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
import           Language.LSP.Types.SemanticTokens
import           Language.LSP.Types.SignatureHelp
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.TypeDefinition
import           Language.LSP.Types.Utils
import           Language.LSP.Types.Window
import           Language.LSP.Types.WatchedFiles
import           Language.LSP.Types.WorkspaceEdit
import           Language.LSP.Types.WorkspaceFolders
import           Language.LSP.Types.WorkspaceSymbol

import Data.Kind
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Scientific
import Data.String
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
  -- Call hierarchy
  MessageParams TextDocumentPrepareCallHierarchy   = CallHierarchyPrepareParams
  MessageParams CallHierarchyIncomingCalls         = CallHierarchyIncomingCallsParams
  MessageParams CallHierarchyOutgoingCalls         = CallHierarchyOutgoingCallsParams
  -- Semantic tokens 
  MessageParams TextDocumentSemanticTokens         = Empty
  MessageParams TextDocumentSemanticTokensFull     = SemanticTokensParams
  MessageParams TextDocumentSemanticTokensFullDelta = SemanticTokensDeltaParams
  MessageParams TextDocumentSemanticTokensRange    = SemanticTokensRangeParams 
  MessageParams WorkspaceSemanticTokensRefresh     = Empty
-- Server
  -- Window
  MessageParams WindowShowMessage                  = ShowMessageParams
  MessageParams WindowShowMessageRequest           = ShowMessageRequestParams
  MessageParams WindowShowDocument                 = ShowDocumentParams
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
  -- Call hierarchy
  ResponseResult TextDocumentPrepareCallHierarchy = Maybe (List CallHierarchyItem)
  ResponseResult CallHierarchyIncomingCalls    = Maybe (List CallHierarchyIncomingCall)
  ResponseResult CallHierarchyOutgoingCalls    = Maybe (List CallHierarchyOutgoingCall)
  -- Semantic tokens 
  ResponseResult TextDocumentSemanticTokens          = Empty
  ResponseResult TextDocumentSemanticTokensFull      = Maybe SemanticTokens
  ResponseResult TextDocumentSemanticTokensFullDelta = Maybe (SemanticTokens |? SemanticTokensDelta)
  ResponseResult TextDocumentSemanticTokensRange     = Maybe SemanticTokens 
  ResponseResult WorkspaceSemanticTokensRefresh      = Empty
  -- Custom can be either a notification or a message
-- Server
  -- Window
  ResponseResult WindowShowMessageRequest      = Maybe MessageActionItem
  ResponseResult WindowShowDocument            = ShowDocumentResult
  ResponseResult WindowWorkDoneProgressCreate  = Empty
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
  parseJSON = genericParseJSON lspOptions . addNullField "params"
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

-- | Replace a missing field in an object with a null field, to simplify parsing
-- This is a hack to allow other types than Maybe to work like Maybe in allowing the field to be missing.
-- See also this issue: https://github.com/haskell/aeson/issues/646
addNullField :: String -> Value -> Value
addNullField s (Object o) = Object $ o <> fromString s .= Null
addNullField _ v = v

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (RequestMessage m) where
  parseJSON = genericParseJSON lspOptions . addNullField "params"
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
               | ServerCancelled
               | RequestFailed
               | ErrorCodeCustom Int32
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
  toJSON ServerCancelled      = Number (-32802)
  toJSON RequestFailed        = Number (-32803)
  toJSON (ErrorCodeCustom n)  = Number (fromIntegral n)

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
  parseJSON (Number (-32802)) = pure ServerCancelled
  parseJSON (Number (-32803)) = pure RequestFailed
  parseJSON (Number n       ) = case toBoundedInteger n of
    Just i -> pure (ErrorCodeCustom i)
    Nothing -> fail "Couldn't convert ErrorCode to bounded integer."
  parseJSON _                 = fail "Couldn't parse ErrorCode"

-- -------------------------------------

data ResponseError =
  ResponseError
    { _code    :: ErrorCode
    , _message :: Text
    , _xdata   :: Maybe Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ResponseError

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
  toJSON ResponseMessage { _jsonrpc = jsonrpc, _id = lspid, _result = result }
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
      (Just err, Nothing) -> pure $ Left err
      (Nothing, Just res) -> pure $ Right res
      (Just _err, Just _res) -> fail $ "both error and result cannot be present: " ++ show o
      (Nothing, Nothing) -> fail "both error and result cannot be Nothing"
    return $ ResponseMessage _jsonrpc _id result

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
