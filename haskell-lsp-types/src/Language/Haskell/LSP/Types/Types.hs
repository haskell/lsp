{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.Haskell.LSP.Types.Types where

import           Language.Haskell.LSP.Types.DataTypesJSON
import           Language.Haskell.LSP.Types.CodeAction
import           Language.Haskell.LSP.Types.Color
import           Language.Haskell.LSP.Types.Completion
import           Language.Haskell.LSP.Types.FoldingRange
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Message
import           Language.Haskell.LSP.Types.Symbol
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Window
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders

import Data.Kind
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.TypeLits

-- Proof that every ClientMessage has a To/FromJSON instance
clientMethodJSON :: SClientMethod m -> ((A.FromJSON (ClientMessage m),A.ToJSON (ClientMessage m)) => x) -> x
clientMethodJSON m x =
  case splitClientMethod m of
    IsClientNot -> x
    IsClientReq -> x
    IsClientEither -> x

type HasJSON a = (A.ToJSON a,A.FromJSON a)

-- Reify universal properties about Client/Server Messages

data ClientNotOrReq (m :: ClientMethod) where
  IsClientNot
    :: ( HasJSON (ClientMessage m)
       , ClientMessage m ~ NotificationMessage (SClientMethod m) req)
    => ClientNotOrReq m
  IsClientReq
    :: ( HasJSON (ClientMessage m)
       , ClientMessage m ~ RequestMessage (SClientMethod m) req rsp
       , HasJSON rsp)
    => ClientNotOrReq m
  IsClientEither
    :: ( HasJSON (ClientMessage m)
       , ClientMessage m ~ SomeMessage (SClientMethod m))
    => ClientNotOrReq m

data ServerNotOrReq (m :: ServerMethod) where
  IsServerNot
    :: ( HasJSON (ServerMessage m)
       , ServerMessage m ~ NotificationMessage (SServerMethod m) req)
    => ServerNotOrReq m
  IsServerReq
    :: ( HasJSON (ServerMessage m)
       , ServerMessage m ~ RequestMessage (SServerMethod m) req rsp
       , HasJSON rsp)
    => ServerNotOrReq m
  IsServerEither
    :: ( HasJSON (ServerMessage m)
       , ServerMessage m ~ SomeMessage (SServerMethod m))
    => ServerNotOrReq m

splitClientMethod :: SClientMethod m -> ClientNotOrReq m
splitClientMethod SInitialize = IsClientReq
splitClientMethod SInitialized = IsClientNot
splitClientMethod SShutdown = IsClientReq
splitClientMethod SExit = IsClientNot
splitClientMethod SCancelRequest = IsClientNot
splitClientMethod SWorkspaceDidChangeWorkspaceFolders = IsClientNot
splitClientMethod SWorkspaceDidChangeConfiguration = IsClientNot
splitClientMethod SWorkspaceDidChangeWatchedFiles = IsClientNot
splitClientMethod SWorkspaceSymbol = IsClientReq
splitClientMethod SWorkspaceExecuteCommand = IsClientReq
splitClientMethod SWindowProgressCancel = IsClientNot
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
splitClientMethod STextDocumentFoldingRange = IsClientReq
splitClientMethod SCustomClientMethod{} = IsClientEither

splitServerMethod :: SServerMethod m -> ServerNotOrReq m
splitServerMethod SWindowShowMessage = IsServerNot
splitServerMethod SWindowShowMessageRequest = IsServerReq
splitServerMethod SWindowLogMessage = IsServerNot
splitServerMethod SWindowProgressStart = IsServerNot
splitServerMethod SWindowProgressReport = IsServerNot
splitServerMethod SWindowProgressDone = IsServerNot
splitServerMethod STelemetryEvent = IsServerNot
splitServerMethod SClientRegisterCapability = IsServerReq
splitServerMethod SClientUnregisterCapability = IsServerReq
splitServerMethod SWorkspaceWorkspaceFolders = IsServerReq
splitServerMethod SWorkspaceConfiguration = IsServerReq
splitServerMethod SWorkspaceApplyEdit = IsServerReq
splitServerMethod STextDocumentPublishDiagnostics = IsServerNot
splitServerMethod SCancelRequestServer = IsServerNot
splitServerMethod SCustomServerMethod{} = IsServerEither


type family Response (t :: Type) :: Type where
  Response (RequestMessage m req rsp) = ResponseMessage rsp
  Response a = TypeError ('Text "Messages of type " :<>: ShowType a :<>:
                          'Text " cannot be responded too")

type family ClientMessage (m :: ClientMethod) :: Type where
  --   RequestMessage <method> <params> <response>
  -- | NotificationMessage <method> <params>
  -- General
  ClientMessage Initialize
    = RequestMessage (SClientMethod Initialize) InitializeParams InitializeResponseCapabilities
  ClientMessage Initialized
    = NotificationMessage (SClientMethod Initialized) (Maybe InitializedParams)
  ClientMessage Shutdown
    = RequestMessage (SClientMethod Shutdown) (Maybe A.Value) Text
  ClientMessage Exit
    = NotificationMessage (SClientMethod Exit) (Maybe ExitParams)
  ClientMessage CancelRequest
    = NotificationMessage (SClientMethod CancelRequest) CancelParams
  -- Workspace
  ClientMessage WorkspaceDidChangeWorkspaceFolders
    = NotificationMessage (SClientMethod WorkspaceDidChangeWorkspaceFolders) DidChangeWorkspaceFoldersParams
  ClientMessage WorkspaceDidChangeConfiguration
    = NotificationMessage (SClientMethod WorkspaceDidChangeConfiguration) DidChangeConfigurationParams
  ClientMessage WorkspaceDidChangeWatchedFiles
    = NotificationMessage (SClientMethod WorkspaceDidChangeWatchedFiles) DidChangeWatchedFilesParams
  ClientMessage WorkspaceSymbol
    = RequestMessage (SClientMethod WorkspaceSymbol) WorkspaceSymbolParams (List SymbolInformation)
  ClientMessage WorkspaceExecuteCommand
    = RequestMessage (SClientMethod WorkspaceExecuteCommand) ExecuteCommandParams A.Value
  -- Progress
  ClientMessage WindowProgressCancel
    = NotificationMessage (SClientMethod WindowProgressCancel) ProgressCancelParams
  -- Sync/Document state
  ClientMessage TextDocumentDidOpen
    = NotificationMessage (SClientMethod TextDocumentDidOpen) DidOpenTextDocumentParams
  ClientMessage TextDocumentDidChange
    = NotificationMessage (SClientMethod TextDocumentDidChange) DidChangeTextDocumentParams
  ClientMessage TextDocumentWillSave
    = NotificationMessage (SClientMethod TextDocumentWillSave) WillSaveTextDocumentParams
  ClientMessage TextDocumentWillSaveWaitUntil
    = RequestMessage (SClientMethod TextDocumentWillSaveWaitUntil) WillSaveTextDocumentParams (List TextEdit)
  ClientMessage TextDocumentDidSave
    = NotificationMessage (SClientMethod TextDocumentDidSave)   DidSaveTextDocumentParams
  ClientMessage TextDocumentDidClose
    = NotificationMessage (SClientMethod TextDocumentDidClose) DidCloseTextDocumentParams
  -- Completion
  ClientMessage TextDocumentCompletion
    = RequestMessage (SClientMethod TextDocumentCompletion) CompletionParams CompletionResponseResult
  ClientMessage CompletionItemResolve
    = RequestMessage (SClientMethod CompletionItemResolve) CompletionItem CompletionItem
  -- Language Queries
  ClientMessage TextDocumentHover
    = RequestMessage (SClientMethod TextDocumentHover) TextDocumentPositionParams (Maybe Hover)
  ClientMessage TextDocumentSignatureHelp
    = RequestMessage (SClientMethod TextDocumentSignatureHelp) TextDocumentPositionParams SignatureHelp
  ClientMessage TextDocumentDefinition
    = RequestMessage (SClientMethod TextDocumentDefinition) TextDocumentPositionParams LocationResponseParams
  ClientMessage TextDocumentTypeDefinition
    = RequestMessage (SClientMethod TextDocumentTypeDefinition) TextDocumentPositionParams LocationResponseParams
  ClientMessage TextDocumentImplementation
    = RequestMessage (SClientMethod TextDocumentImplementation) TextDocumentPositionParams LocationResponseParams
  ClientMessage TextDocumentReferences
    = RequestMessage (SClientMethod TextDocumentReferences) ReferenceParams (List Location)
  ClientMessage TextDocumentDocumentHighlight
    = RequestMessage (SClientMethod TextDocumentDocumentHighlight) TextDocumentPositionParams (List DocumentHighlight)
  ClientMessage TextDocumentDocumentSymbol
    = RequestMessage (SClientMethod TextDocumentDocumentSymbol) DocumentSymbolParams DSResult
  -- Code Action/Lens/Link
  ClientMessage TextDocumentCodeAction
    = RequestMessage (SClientMethod TextDocumentCodeAction) CodeActionParams (List CAResult)
  ClientMessage TextDocumentCodeLens
    = RequestMessage (SClientMethod TextDocumentCodeLens) CodeLensParams (List CodeLens)
  ClientMessage CodeLensResolve
    = RequestMessage (SClientMethod CodeLensResolve) CodeLens (List CodeLens)
  ClientMessage TextDocumentDocumentLink
    = RequestMessage (SClientMethod TextDocumentDocumentLink) DocumentLinkParams (List DocumentLink)
  ClientMessage DocumentLinkResolve
    = RequestMessage (SClientMethod DocumentLinkResolve) DocumentLink DocumentLink
  -- Syntax highlighting/coloring
  ClientMessage TextDocumentDocumentColor
    = RequestMessage (SClientMethod TextDocumentDocumentColor) DocumentColorParams (List ColorInformation)
  ClientMessage TextDocumentColorPresentation
    = RequestMessage (SClientMethod TextDocumentColorPresentation) ColorPresentationParams (List ColorPresentation)
  -- Formatting
  ClientMessage TextDocumentFormatting
    = RequestMessage (SClientMethod TextDocumentFormatting) DocumentFormattingParams (List TextEdit)
  ClientMessage TextDocumentRangeFormatting
    = RequestMessage (SClientMethod TextDocumentRangeFormatting) DocumentRangeFormattingParams (List TextEdit)
  ClientMessage TextDocumentOnTypeFormatting
    = RequestMessage (SClientMethod TextDocumentOnTypeFormatting) DocumentOnTypeFormattingParams (List TextEdit)
  -- Rename
  ClientMessage TextDocumentRename
    = RequestMessage (SClientMethod TextDocumentRename) RenameParams WorkspaceEdit
  -- FoldingRange
  ClientMessage TextDocumentFoldingRange
    = RequestMessage (SClientMethod TextDocumentFoldingRange) FoldingRangeParams (List FoldingRange)
  -- Custom can be either a notification or a message
  ClientMessage CustomClientMethod
    = SomeMessage (SClientMethod CustomClientMethod)

data Empty = Empty
instance A.ToJSON Empty where
  toJSON Empty = A.Null
instance A.FromJSON Empty where
  parseJSON A.Null = pure Empty
  parseJSON _ = mempty

type family ServerMessage (m :: ServerMethod) :: Type where
  -- Window
  ServerMessage WindowShowMessage
    = NotificationMessage (SServerMethod WindowShowMessage) ShowMessageParams
  ServerMessage WindowShowMessageRequest
    = RequestMessage (SServerMethod WindowShowMessageRequest) ShowMessageRequestParams (Maybe MessageActionItem)
  ServerMessage WindowLogMessage
    = NotificationMessage (SServerMethod WindowLogMessage) LogMessageParams
  ServerMessage WindowProgressStart
    = NotificationMessage (SServerMethod WindowProgressStart) ProgressStartParams
  ServerMessage WindowProgressReport
    = NotificationMessage (SServerMethod WindowProgressReport) ProgressReportParams
  ServerMessage WindowProgressDone
    = NotificationMessage (SServerMethod WindowProgressDone) ProgressDoneParams
  ServerMessage TelemetryEvent
    = NotificationMessage (SServerMethod TelemetryEvent) A.Value
  -- Capability
  ServerMessage ClientRegisterCapability
    = RequestMessage (SServerMethod ClientRegisterCapability) RegistrationParams Empty
  ServerMessage ClientUnregisterCapability
    = RequestMessage (SServerMethod ClientUnregisterCapability) UnregistrationParams Empty
  -- Workspace
  ServerMessage WorkspaceWorkspaceFolders
    = RequestMessage (SServerMethod WorkspaceWorkspaceFolders) Empty (Maybe (List WorkspaceFolder))
  ServerMessage WorkspaceConfiguration
    = RequestMessage (SServerMethod WorkspaceConfiguration) ConfigurationParams (List A.Value)
  ServerMessage WorkspaceApplyEdit
    = RequestMessage (SServerMethod WorkspaceApplyEdit) ApplyWorkspaceEditParams ApplyWorkspaceEditResponseBody
  -- Document/Diagnostic
  ServerMessage TextDocumentPublishDiagnostics
    = NotificationMessage (SServerMethod TextDocumentPublishDiagnostics) PublishDiagnosticsParams
  -- Cancel
  ServerMessage CancelRequestServer
    = NotificationMessage (SServerMethod CancelRequestServer) CancelParams
  -- Custom
  ServerMessage CustomServerMethod
    = SomeMessage (SServerMethod CustomServerMethod)

-- Server Messages

-- Window
type ShowMessageNotification = ServerMessage WindowShowMessage
type ShowMessageRequest = ServerMessage WindowShowMessageRequest
type ShowMessageResponse = Response ShowMessageRequest
type LogMessageNotification = ServerMessage WindowLogMessage

-- | The window/progress/start notification is sent from the server to the
-- client to ask the client to start progress.
--
-- @since 0.10.0.0
type ProgressStartNotification = ServerMessage WindowProgressStart

-- | The window/progress/report notification is sent from the server to the
-- client to report progress for a previously started progress.
--
-- @since 0.10.0.0
type ProgressReportNotification = ServerMessage WindowProgressReport

-- | The window/progress/done notification is sent from the server to the
-- client to stop a previously started progress.
--
-- @since 0.10.0.0
type ProgressDoneNotification = ServerMessage WindowProgressDone

type TelemetryNotification = ServerMessage TelemetryEvent

-- Capability
type RegisterCapabilityRequest = ServerMessage ClientRegisterCapability
type RegisterCapabilityResponse = Response RegisterCapabilityRequest
type UnregisterCapabilityRequest = ServerMessage ClientUnregisterCapability
type UnregisterCapabilityResponse = Response UnregisterCapabilityRequest

-- Workspace
type WorkspaceFoldersRequest = ServerMessage WorkspaceWorkspaceFolders
type WorkspaceFoldersResponse = Response WorkspaceFoldersRequest
type ConfigurationRequest = ServerMessage WorkspaceConfiguration
type ConfigurationResponse = Response ConfigurationRequest
type ApplyWorkspaceEditRequest  = ServerMessage WorkspaceApplyEdit
type ApplyWorkspaceEditResponse = Response ApplyWorkspaceEditRequest

-- Document/Diagnostic
type PublishDiagnosticsNotification = ServerMessage TextDocumentPublishDiagnostics

-- Cancel
type CancelNotificationServer = ServerMessage CancelRequestServer

-- Custom
type CustomServerNotification = ServerMessage CustomServerMethod
type CustomServerRequest = ServerMessage CustomServerMethod

-- Client Messages

-- General
type InitializeRequest       = ClientMessage Initialize
type InitializeResponse      = Response InitializeRequest
type InitializedNotification = ClientMessage Initialized
type ShutdownRequest         = ClientMessage Shutdown
type ShutdownResponse        = Response ShutdownRequest
type ExitNotification        = ClientMessage Exit
type CancelNotification      = ClientMessage CancelRequest

-- Workspace
type DidChangeWorkspaceFoldersNotification = ClientMessage WorkspaceDidChangeWorkspaceFolders
type DidChangeConfigurationNotification    = ClientMessage WorkspaceDidChangeConfiguration 
type DidChangeWatchedFilesNotification     = ClientMessage WorkspaceDidChangeWatchedFiles 
type WorkspaceSymbolRequest                = ClientMessage WorkspaceSymbol 
type WorkspaceSymbolsResponse              = Response WorkspaceSymbolRequest
type ExecuteCommandRequest                 = ClientMessage WorkspaceExecuteCommand 
type ExecuteCommandResponse                = Response ExecuteCommandRequest

-- Progress

-- | The window/progress/cancel notification is sent from the client to the server
-- to inform the server that the user has pressed the cancel button on the progress UX.
-- A server receiving a cancel request must still close a progress using the done notification.
--
-- @since 0.10.0.0
type ProgressCancelNotification = ClientMessage WindowProgressCancel

-- Document/Sync
type DidOpenTextDocumentNotification       = ClientMessage TextDocumentDidOpen
type DidChangeTextDocumentNotification     = ClientMessage TextDocumentDidChange
type WillSaveTextDocumentNotification      = ClientMessage TextDocumentWillSave
type WillSaveWaitUntilTextDocumentRequest  = ClientMessage TextDocumentWillSaveWaitUntil
type WillSaveWaitUntilTextDocumentResponse = Response WillSaveWaitUntilTextDocumentRequest
type DidSaveTextDocumentNotification       = ClientMessage TextDocumentDidSave
type DidCloseTextDocumentNotification      = ClientMessage TextDocumentDidClose

-- Completion

type CompletionRequest  = ClientMessage TextDocumentCompletion
type CompletionResponse = Response CompletionRequest

type CompletionItemResolveRequest  = ClientMessage CompletionItemResolve
type CompletionItemResolveResponse = Response CompletionItemResolveRequest

-- Queries
type HoverRequest               = ClientMessage TextDocumentHover
type HoverResponse              = Response HoverRequest
type SignatureHelpRequest       = ClientMessage TextDocumentSignatureHelp
type SignatureHelpResponse      = Response SignatureHelpRequest
type DefinitionRequest          = ClientMessage TextDocumentDefinition
type DefinitionResponse         = Response DefinitionRequest
type TypeDefinitionRequest      = ClientMessage TextDocumentTypeDefinition
type TypeDefinitionResponse     = Response TypeDefinitionRequest
type ImplementationRequest      = ClientMessage TextDocumentImplementation
type ImplementationResponse     = Response ImplementationRequest
type ReferencesRequest          = ClientMessage TextDocumentReferences
type ReferencesResponse         = Response ReferencesRequest
type DocumentHighlightRequest   = ClientMessage TextDocumentDocumentHighlight
type DocumentHighlightsResponse = Response DocumentHighlightRequest
type DocumentSymbolRequest      = ClientMessage TextDocumentDocumentSymbol
type DocumentSymbolsResponse    = Response DocumentSymbolRequest

-- Code Lens/Action/Link

type CodeActionRequest  = ClientMessage TextDocumentCodeAction
type CodeActionResponse = Response CodeActionRequest

type CodeLensRequest  = ClientMessage TextDocumentCodeLens
type CodeLensResponse = Response CodeLensRequest
type CodeLensResolveRequest  = ClientMessage CodeLensResolve
type CodeLensResolveResponse = Response CodeLensResolveRequest

type DocumentLinkRequest  = ClientMessage TextDocumentDocumentLink
type DocumentLinkResponse = Response DocumentLinkRequest
type DocumentLinkResolveRequest  = ClientMessage DocumentLinkResolve
type DocumentLinkResolveResponse = Response DocumentLinkResolveRequest

-- Color/Syntax

type DocumentColorRequest  = ClientMessage TextDocumentDocumentColor
type DocumentColorResponse = Response DocumentColorRequest

type ColorPresentationRequest  = ClientMessage TextDocumentColorPresentation
type ColorPresentationResponse = Response ColorPresentationRequest


-- Formatting
type DocumentFormattingRequest  = ClientMessage TextDocumentFormatting
type DocumentFormattingResponse = Response DocumentFormattingRequest

type DocumentRangeFormattingRequest  = ClientMessage TextDocumentRangeFormatting
type DocumentRangeFormattingResponse = Response DocumentRangeFormattingRequest

type DocumentOnTypeFormattingRequest  = ClientMessage TextDocumentOnTypeFormatting
type DocumentOnTypeFormattingResponse = Response DocumentOnTypeFormattingRequest

-- Rename
type RenameRequest  = ClientMessage TextDocumentRename
type RenameResponse = Response RenameRequest

-- Folding
type FoldingRangeRequest  = ClientMessage TextDocumentFoldingRange
type FoldingRangeResponse = Response FoldingRangeRequest

-- Custom
type CustomClientNotification = ClientMessage CustomClientMethod

type CustomClientRequest = ClientMessage CustomClientMethod

type CustomResponse = ResponseMessage A.Value

