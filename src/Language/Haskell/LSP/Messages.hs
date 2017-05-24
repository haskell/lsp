{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Messages (
  -- * General
    fmClientInitializeRequest
  , fmClientInitializedNotification
  , fmClientShutdownRequest
  , fmClientExitNotification
  , fmClientCancelNotification

  -- * Window
  , fmServerShowMessageNotification
  , fmServerShowMessageRequest
  , fmServerLogMessageNotification
  , fmServerTelemetryNotification

  -- * Client
  , fmServerRegisterCapabilityRequest
  , fmServerUnregisterCapabilityRequest

  -- * Workspace
  , fmServerDidChangeConfigurationNotification
  , fmServerDidChangeWatchedFilesNotification
  , fmClientWorkspaceSymbolRequest
  , fmClientExecuteCommandRequest
  , fmServerApplyWorkspaceEditRequest

  -- * Document
  , fmServerPublishDiagnosticsNotification
  , fmClientDidOpenTextDocumentNotification
  , fmClientDidChangeTextDocumentNotification
  , fmClientWillSaveTextDocumentNotification
  , fmClientWillSaveWaitUntilRequest
  , fmClientDidSaveTextDocumentNotification
  , fmClientDidCloseTextDocumentNotification
  , fmClientCompletionRequest
  , fmClientCompletionItemResolveRequest
  , fmClientHoverRequest
  , fmClientSignatureHelpRequest
  , fmClientReferencesRequest
  , fmClientDocumentHighlightRequest
  , fmClientDocumentSymbolRequest
  , fmClientDocumentFormattingRequest
  , fmClientDocumentRangeFormattingRequest
  , fmClientDocumentOnTypeFormattingRequest
  , fmClientDefinitionRequest
  , fmClientCodeActionRequest
  , fmClientCodeLensRequest
  , fmClientCodeLensResolveRequest
  , fmClientDocumentLinkRequest
  , fmClientDocumentLinkResolveRequest
  , fmClientRenameRequest
  ) where

import qualified Data.Aeson as J
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- General
-- ----------------------------------------------------------------------

-- * :leftwards_arrow_with_hook: [initialize](#initialize)

fmClientInitializeRequest :: J.LspId -> J.InitializeParams -> J.InitializeRequest
fmClientInitializeRequest rid params
  = J.RequestMessage  "2.0" rid "initialize" params

-- ----------------------------------------------------------------------
-- * **New** :arrow_right: [initialized](#initialized)

-- | From 3.0
fmClientInitializedNotification :: J.InitializedNotification
fmClientInitializedNotification = J.NotificationMessage "2.0" "initialized" Nothing

-- ----------------------------------------------------------------------
-- * :leftwards_arrow_with_hook: [shutdown](#shutdown)

fmClientShutdownRequest :: J.LspId -> Maybe J.Value -> J.ShutdownRequest
fmClientShutdownRequest rid params
  = J.RequestMessage  "2.0" rid "shutdown" params

-- ----------------------------------------------------------------------
-- * :arrow_right: [exit](#exit)

fmClientExitNotification :: J.ExitNotification
fmClientExitNotification = J.NotificationMessage "2.0" "exit" Nothing

-- ----------------------------------------------------------------------
-- * :arrow_right: [$/cancelRequest](#cancelRequest)

fmClientCancelNotification :: J.LspId -> J.CancelNotification
fmClientCancelNotification idToCancel
  = J.NotificationMessage "2.0" "$/cancelRequest" ( (J.CancelParams idToCancel))

-- ----------------------------------------------------------------------
-- Window
-- ----------------------------------------------------------------------

-- * :arrow_left: [window/showMessage](#window_showMessage)

fmServerShowMessageNotification :: J.MessageType -> String -> J.ShowMessageNotification
fmServerShowMessageNotification mt msg
  = J.NotificationMessage "2.0" "window/showMessage" (J.ShowMessageParams mt msg)

-- ----------------------------------------------------------------------
-- * :arrow_right_hook: [window/showMessageRequest](#window_showMessageRequest)

fmServerShowMessageRequest :: J.LspId -> J.ShowMessageRequestParams -> J.ShowMessageRequest
fmServerShowMessageRequest rid params
  = J.RequestMessage  "2.0" rid "window/showMessageRequest" ( params)

-- ----------------------------------------------------------------------
-- * :arrow_left: [window/logMessage](#window_logMessage)

fmServerLogMessageNotification :: J.MessageType -> String -> J.LogMessageNotification
fmServerLogMessageNotification mt msg
  = J.NotificationMessage "2.0" "window/logMessage" (J.LogMessageParams mt msg)

-- ----------------------------------------------------------------------
-- * :arrow_left: [telemetry/event](#telemetry_event)

fmServerTelemetryNotification :: J.Value  -> J.TelemetryNotification
fmServerTelemetryNotification params
  = J.NotificationMessage "2.0" "telemetry/event" params

-- ----------------------------------------------------------------------
--  Client
-- ----------------------------------------------------------------------

-- * :arrow_right_hook: [client/registerCapability](#client_registerCapability)
-- | from 3.0
fmServerRegisterCapabilityRequest :: J.LspId -> J.RegistrationParams -> J.RegisterCapabilityRequest
fmServerRegisterCapabilityRequest rid params
  = J.RequestMessage  "2.0" rid "client/registerCapability" (params)

-- * :arrow_right_hook: [client/unregisterCapability](#client_unregisterCapability)
-- | from 3.0
fmServerUnregisterCapabilityRequest :: J.LspId -> J.UnregistrationParams -> J.UnregisterCapabilityRequest
fmServerUnregisterCapabilityRequest rid params
  = J.RequestMessage  "2.0" rid "client/unregisterCapability" ( params)

-- ----------------------------------------------------------------------
-- Workspace
-- ----------------------------------------------------------------------

-- * :arrow_right: [workspace/didChangeConfiguration](#workspace_didChangeConfiguration)
fmServerDidChangeConfigurationNotification :: J.DidChangeConfigurationParams -> J.DidChangeConfigurationNotification
fmServerDidChangeConfigurationNotification params
  = J.NotificationMessage "2.0" "workspace/didChangeConfiguration" ( params)

-- * :arrow_right: [workspace/didChangeWatchedFiles](#workspace_didChangeWatchedFiles)
fmServerDidChangeWatchedFilesNotification :: J.DidChangeWatchedFilesParams -> J.DidChangeWatchedFilesNotification
fmServerDidChangeWatchedFilesNotification params
  = J.NotificationMessage "2.0" "workspace/didChangeWatchedFiles" ( params)

-- * :leftwards_arrow_with_hook: [workspace/symbol](#workspace_symbol)
fmClientWorkspaceSymbolRequest :: J.LspId -> J.WorkspaceSymbolParams -> J.WorkspaceSymbolRequest
fmClientWorkspaceSymbolRequest rid params
  = J.RequestMessage  "2.0" rid "workspace/symbol" ( params)

-- * **New** :leftwards_arrow_with_hook: [workspace/executeCommand](#workspace_executeCommand)
-- | From 3.0
fmClientExecuteCommandRequest :: J.LspId -> J.ExecuteCommandParams -> J.ExecuteCommandRequest
fmClientExecuteCommandRequest rid params
  = J.RequestMessage  "2.0" rid "workspace/executeCommand" ( params)

-- * **New** :arrow_right_hook: [workspace/applyEdit](#workspace_applyEdit)
-- | From 3.0
fmServerApplyWorkspaceEditRequest :: J.LspId -> J.ApplyWorkspaceEditParams -> J.ApplyWorkspaceEditRequest
fmServerApplyWorkspaceEditRequest rid params
  = J.RequestMessage  "2.0" rid "workspace/applyEdit" ( params)

-- ----------------------------------------------------------------------
 -- Document
-- ----------------------------------------------------------------------

-- * :arrow_left: [textDocument/publishDiagnostics](#textDocument_publishDiagnostics)
fmServerPublishDiagnosticsNotification :: J.PublishDiagnosticsParams -> J.PublishDiagnosticsNotification
fmServerPublishDiagnosticsNotification params
  = J.NotificationMessage "2.0" "textDocument/publishDiagnostics" ( params)

-- * :arrow_right: [textDocument/didOpen](#textDocument_didOpen)
fmClientDidOpenTextDocumentNotification :: J.DidOpenTextDocumentParams -> J.DidOpenTextDocumentNotification
fmClientDidOpenTextDocumentNotification params
  = J.NotificationMessage "2.0" "textDocument/didOpen" ( params)

-- * :arrow_right: [textDocument/didChange](#textDocument_didChange)
fmClientDidChangeTextDocumentNotification :: J.DidChangeTextDocumentParams -> J.DidChangeTextDocumentNotification
fmClientDidChangeTextDocumentNotification params
  = J.NotificationMessage "2.0" "textDocument/didChange" ( params)

-- * :arrow_right: [textDocument/willSave](#textDocument_willSave)
fmClientWillSaveTextDocumentNotification :: J.WillSaveTextDocumentParams -> J.WillSaveTextDocumentNotification
fmClientWillSaveTextDocumentNotification params
  = J.NotificationMessage "2.0" "textDocument/willSave" ( params)

-- * **New** :leftwards_arrow_with_hook: [textDocument/willSaveWaitUntil](#textDocument_willSaveWaitUntil)
-- | From 3.0
fmClientWillSaveWaitUntilRequest :: J.LspId -> J.WillSaveTextDocumentParams -> J.WillSaveWaitUntilTextDocumentRequest
fmClientWillSaveWaitUntilRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/willSaveWaitUntil" ( params)

-- * **New** :arrow_right: [textDocument/didSave](#textDocument_didSave)
-- | 3.0
fmClientDidSaveTextDocumentNotification :: J.DidSaveTextDocumentParams -> J.DidSaveTextDocumentNotification
fmClientDidSaveTextDocumentNotification params
  = J.NotificationMessage "2.0" "textDocument/didSave" ( params)

-- * :arrow_right: [textDocument/didClose](#textDocument_didClose)
fmClientDidCloseTextDocumentNotification :: J.DidCloseTextDocumentParams -> J.DidCloseTextDocumentNotification
fmClientDidCloseTextDocumentNotification params
  = J.NotificationMessage "2.0" "textDocument/didClose" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/completion](#textDocument_completion)
fmClientCompletionRequest :: J.LspId -> J.TextDocumentPositionParams -> J.CompletionRequest
fmClientCompletionRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/completion" ( params)

-- * :leftwards_arrow_with_hook: [completionItem/resolve](#completionItem_resolve)
fmClientCompletionItemResolveRequest :: J.LspId -> J.CompletionItem -> J.CompletionItemResolveRequest
fmClientCompletionItemResolveRequest rid params
  = J.RequestMessage "2.0" rid "completionItem/resolve" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/hover](#textDocument_hover)
fmClientHoverRequest :: J.LspId -> J.TextDocumentPositionParams -> J.HoverRequest
fmClientHoverRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/hover" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/signatureHelp](#textDocument_signatureHelp)
fmClientSignatureHelpRequest :: J.LspId -> J.TextDocumentPositionParams -> J.SignatureHelpRequest
fmClientSignatureHelpRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/signatureHelp" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/references](#textDocument_references)
fmClientReferencesRequest :: J.LspId -> J.ReferenceParams -> J.ReferencesRequest
fmClientReferencesRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/references" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/documentHighlight](#textDocument_documentHighlight)
fmClientDocumentHighlightRequest :: J.LspId -> J.TextDocumentPositionParams -> J.DocumentHighlightRequest
fmClientDocumentHighlightRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/documentHighlight" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/documentSymbol](#textDocument_documentSymbol)
fmClientDocumentSymbolRequest :: J.LspId -> J.DocumentSymbolParams -> J.DocumentSymbolRequest
fmClientDocumentSymbolRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/documentSymbol" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/formatting](#textDocument_formatting)
fmClientDocumentFormattingRequest :: J.LspId -> J.DocumentFormattingParams -> J.DocumentFormattingRequest
fmClientDocumentFormattingRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/formatting" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/rangeFormatting](#textDocument_rangeFormatting)
fmClientDocumentRangeFormattingRequest :: J.LspId -> J.DocumentRangeFormattingParams -> J.DocumentRangeFormattingRequest
fmClientDocumentRangeFormattingRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/rangeFormatting" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/onTypeFormatting](#textDocument_onTypeFormatting)
fmClientDocumentOnTypeFormattingRequest :: J.LspId -> J.DocumentOnTypeFormattingParams -> J.DocumentOnTypeFormattingRequest
fmClientDocumentOnTypeFormattingRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/onTypeFormatting" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/definition](#textDocument_definition)
fmClientDefinitionRequest :: J.LspId -> J.TextDocumentPositionParams -> J.DefinitionRequest
fmClientDefinitionRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/definition" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/codeAction](#textDocument_codeAction)
fmClientCodeActionRequest :: J.LspId -> J.CodeActionParams -> J.CodeActionRequest
fmClientCodeActionRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/codeAction" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/codeLens](#textDocument_codeLens)
fmClientCodeLensRequest :: J.LspId -> J.CodeLensParams -> J.CodeLensRequest
fmClientCodeLensRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/codeLens" ( params)

-- * :leftwards_arrow_with_hook: [codeLens/resolve](#codeLens_resolve)
fmClientCodeLensResolveRequest :: J.LspId -> J.CodeLens -> J.CodeLensResolveRequest
fmClientCodeLensResolveRequest rid params
  = J.RequestMessage "2.0" rid "codeLens/resolve" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/documentLink](#textDocument_documentLink)
fmClientDocumentLinkRequest :: J.LspId -> J.DocumentLinkParams -> J.DocumentLinkRequest
fmClientDocumentLinkRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/documentLink" ( params)

-- * :leftwards_arrow_with_hook: [documentLink/resolve](#documentLink_resolve)
fmClientDocumentLinkResolveRequest :: J.LspId -> J.DocumentLink -> J.DocumentLinkResolveRequest
fmClientDocumentLinkResolveRequest rid params
  = J.RequestMessage "2.0" rid "documentLink/resolve" ( params)

-- * :leftwards_arrow_with_hook: [textDocument/rename](#textDocument_rename)
fmClientRenameRequest :: J.LspId -> J.RenameParams -> J.RenameRequest
fmClientRenameRequest rid params
  = J.RequestMessage "2.0" rid "textDocument/rename" ( params)

