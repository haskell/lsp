{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.LSP.Types.Synonyms where

import           Language.LSP.Types.Method
import           Language.LSP.Types.Message

-- Server Messages

-- Window
type ShowMessageNotification = Message WindowShowMessage

type ShowMessageRequest = Message WindowShowMessageRequest
type ShowMessageResponse = ResponseMessage WindowShowMessageRequest

type LogMessageNotification = Message WindowLogMessage

type WindowWorkDoneProgressCreateRequest = Message WindowWorkDoneProgressCreate
type WindowWorkDoneProgressCreateResponse = ResponseMessage WindowWorkDoneProgressCreate

type ProgressNotification = Message Progress

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

type WorkDoneProgressCancelNotification = Message WindowWorkDoneProgressCancel

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

type DeclarationRequest      = Message TextDocumentDeclaration
type DeclarationResponse     = ResponseMessage TextDocumentDeclaration

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
