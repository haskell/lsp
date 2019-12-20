{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Types.MessageFuncs (
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
  , fmServerWorkDoneProgressBeginNotification
  , fmServerWorkDoneProgressReportNotification
  , fmServerWorkDoneProgressEndNotification
  , fmServerWorkDoneProgressCreateRequest
  , fmServerTelemetryNotification

  -- * Client
  , fmServerRegisterCapabilityRequest
  , fmServerUnregisterCapabilityRequest

  -- * Workspace
  , fmClientDidChangeConfigurationNotification
  , fmClientDidChangeWatchedFilesNotification
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
  , fmClientPrepareRenameRequest
  ) where

import qualified Data.Aeson as J
import           Data.Text ( Text )
import qualified Language.Haskell.LSP.Types      as J

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
  = J.RequestMessage  "2.0" rid J.SInitialize params

-- ----------------------------------------------------------------------
-- * **New** :arrow_right: [initialized](#initialized)

-- | From 3.0
fmClientInitializedNotification :: J.InitializedNotification
fmClientInitializedNotification = J.NotificationMessage "2.0" J.SInitialized Nothing

-- ----------------------------------------------------------------------
-- * :leftwards_arrow_with_hook: [shutdown](#shutdown)

fmClientShutdownRequest :: J.LspId -> Maybe J.Value -> J.ShutdownRequest
fmClientShutdownRequest rid params
  = J.RequestMessage  "2.0" rid J.SShutdown params

-- ----------------------------------------------------------------------
-- * :arrow_right: [exit](#exit)

fmClientExitNotification :: J.ExitNotification
fmClientExitNotification = J.NotificationMessage "2.0" J.SExit Nothing

-- ----------------------------------------------------------------------
-- * :arrow_right: [$/cancelRequest](#cancelRequest)

fmClientCancelNotification :: J.LspId -> J.CancelNotification
fmClientCancelNotification idToCancel
  = J.NotificationMessage "2.0" J.SCancelRequest  (J.CancelParams idToCancel)

-- ----------------------------------------------------------------------
-- Window
-- ----------------------------------------------------------------------

-- * :arrow_left: [window/showMessage](#window_showMessage)

fmServerShowMessageNotification :: J.MessageType -> Text -> J.ShowMessageNotification
fmServerShowMessageNotification mt msg
  = J.NotificationMessage "2.0" J.SWindowShowMessage (J.ShowMessageParams mt msg)

-- ----------------------------------------------------------------------
-- * :arrow_right_hook: [window/showMessageRequest](#window_showMessageRequest)

fmServerShowMessageRequest :: J.LspId -> J.ShowMessageRequestParams -> J.ShowMessageRequest
fmServerShowMessageRequest rid params
  = J.RequestMessage  "2.0" rid J.SWindowShowMessageRequest params

-- ----------------------------------------------------------------------
-- * :arrow_left: [window/logMessage](#window_logMessage)

fmServerLogMessageNotification :: J.MessageType -> Text -> J.LogMessageNotification
fmServerLogMessageNotification mt msg
  = J.NotificationMessage "2.0" J.SWindowLogMessage (J.LogMessageParams mt msg)

-- ----------------------------------------------------------------------

fmServerWorkDoneProgressBeginNotification :: J.ProgressParams J.WorkDoneProgressBeginParams -> J.WorkDoneProgressBeginNotification
fmServerWorkDoneProgressBeginNotification params
  = J.NotificationMessage "2.0" J.SProgress (J.Begin <$> params)

-- ----------------------------------------------------------------------

fmServerWorkDoneProgressReportNotification :: J.ProgressParams J.WorkDoneProgressReportParams -> J.WorkDoneProgressReportNotification
fmServerWorkDoneProgressReportNotification params
  = J.NotificationMessage "2.0" J.SProgress (J.Report <$> params)

-- ----------------------------------------------------------------------

fmServerWorkDoneProgressEndNotification :: J.ProgressParams J.WorkDoneProgressEndParams -> J.WorkDoneProgressEndNotification
fmServerWorkDoneProgressEndNotification params
  = J.NotificationMessage "2.0" J.SProgress (J.End <$> params)

fmServerWorkDoneProgressCreateRequest :: J.LspId -> J.WorkDoneProgressCreateParams -> J.WorkDoneProgressCreateRequest
fmServerWorkDoneProgressCreateRequest rid params
  = J.RequestMessage "2.0" rid J.SWindowWorkDoneProgressCreate params

-- ----------------------------------------------------------------------
-- * :arrow_left: [telemetry/event](#telemetry_event)

fmServerTelemetryNotification :: J.Value  -> J.TelemetryNotification
fmServerTelemetryNotification params
  = J.NotificationMessage "2.0" J.STelemetryEvent params

-- ----------------------------------------------------------------------
--  Client
-- ----------------------------------------------------------------------

-- * :arrow_right_hook: [client/registerCapability](#client_registerCapability)
-- | from 3.0
fmServerRegisterCapabilityRequest :: J.LspId -> J.RegistrationParams -> J.RegisterCapabilityRequest
fmServerRegisterCapabilityRequest rid params
  = J.RequestMessage  "2.0" rid J.SClientRegisterCapability params

-- * :arrow_right_hook: [client/unregisterCapability](#client_unregisterCapability)
-- | from 3.0
fmServerUnregisterCapabilityRequest :: J.LspId -> J.UnregistrationParams -> J.UnregisterCapabilityRequest
fmServerUnregisterCapabilityRequest rid params
  = J.RequestMessage  "2.0" rid J.SClientUnregisterCapability params

-- ----------------------------------------------------------------------
-- Workspace
-- ----------------------------------------------------------------------

-- * :arrow_right: [workspace/didChangeConfiguration](#workspace_didChangeConfiguration)
fmClientDidChangeConfigurationNotification :: J.DidChangeConfigurationParams -> J.DidChangeConfigurationNotification
fmClientDidChangeConfigurationNotification params
  = J.NotificationMessage "2.0" J.SWorkspaceDidChangeConfiguration params

-- * :arrow_right: [workspace/didChangeWatchedFiles](#workspace_didChangeWatchedFiles)
fmClientDidChangeWatchedFilesNotification :: J.DidChangeWatchedFilesParams -> J.DidChangeWatchedFilesNotification
fmClientDidChangeWatchedFilesNotification params
  = J.NotificationMessage "2.0" J.SWorkspaceDidChangeWatchedFiles params

-- * :leftwards_arrow_with_hook: [workspace/symbol](#workspace_symbol)
fmClientWorkspaceSymbolRequest :: J.LspId -> J.WorkspaceSymbolParams -> J.WorkspaceSymbolRequest
fmClientWorkspaceSymbolRequest rid params
  = J.RequestMessage  "2.0" rid J.SWorkspaceSymbol params

-- * **New** :leftwards_arrow_with_hook: [workspace/executeCommand](#workspace_executeCommand)
-- | From 3.0
fmClientExecuteCommandRequest :: J.LspId -> J.ExecuteCommandParams -> J.ExecuteCommandRequest
fmClientExecuteCommandRequest rid params
  = J.RequestMessage  "2.0" rid J.SWorkspaceExecuteCommand params

-- * **New** :arrow_right_hook: [workspace/applyEdit](#workspace_applyEdit)
-- | From 3.0
fmServerApplyWorkspaceEditRequest :: J.LspId -> J.ApplyWorkspaceEditParams -> J.ApplyWorkspaceEditRequest
fmServerApplyWorkspaceEditRequest rid params
  = J.RequestMessage  "2.0" rid J.SWorkspaceApplyEdit params

-- ----------------------------------------------------------------------
 -- Document
-- ----------------------------------------------------------------------

-- * :arrow_left: [textDocument/publishDiagnostics](#textDocument_publishDiagnostics)
fmServerPublishDiagnosticsNotification :: J.PublishDiagnosticsParams -> J.PublishDiagnosticsNotification
fmServerPublishDiagnosticsNotification params
  = J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params

-- * :arrow_right: [textDocument/didOpen](#textDocument_didOpen)
fmClientDidOpenTextDocumentNotification :: J.DidOpenTextDocumentParams -> J.DidOpenTextDocumentNotification
fmClientDidOpenTextDocumentNotification params
  = J.NotificationMessage "2.0" J.STextDocumentDidOpen params

-- * :arrow_right: [textDocument/didChange](#textDocument_didChange)
fmClientDidChangeTextDocumentNotification :: J.DidChangeTextDocumentParams -> J.DidChangeTextDocumentNotification
fmClientDidChangeTextDocumentNotification params
  = J.NotificationMessage "2.0" J.STextDocumentDidChange params

-- * :arrow_right: [textDocument/willSave](#textDocument_willSave)
fmClientWillSaveTextDocumentNotification :: J.WillSaveTextDocumentParams -> J.WillSaveTextDocumentNotification
fmClientWillSaveTextDocumentNotification params
  = J.NotificationMessage "2.0" J.STextDocumentWillSave params

-- * **New** :leftwards_arrow_with_hook: [textDocument/willSaveWaitUntil](#textDocument_willSaveWaitUntil)
-- | From 3.0
fmClientWillSaveWaitUntilRequest :: J.LspId -> J.WillSaveTextDocumentParams -> J.WillSaveWaitUntilTextDocumentRequest
fmClientWillSaveWaitUntilRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentWillSaveWaitUntil params

-- * **New** :arrow_right: [textDocument/didSave](#textDocument_didSave)
-- | 3.0
fmClientDidSaveTextDocumentNotification :: J.DidSaveTextDocumentParams -> J.DidSaveTextDocumentNotification
fmClientDidSaveTextDocumentNotification params
  = J.NotificationMessage "2.0" J.STextDocumentDidSave params

-- * :arrow_right: [textDocument/didClose](#textDocument_didClose)
fmClientDidCloseTextDocumentNotification :: J.DidCloseTextDocumentParams -> J.DidCloseTextDocumentNotification
fmClientDidCloseTextDocumentNotification params
  = J.NotificationMessage "2.0" J.STextDocumentDidClose params

-- * :leftwards_arrow_with_hook: [textDocument/completion](#textDocument_completion)
fmClientCompletionRequest :: J.LspId -> J.CompletionParams -> J.CompletionRequest
fmClientCompletionRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentCompletion params

-- * :leftwards_arrow_with_hook: [completionItem/resolve](#completionItem_resolve)
fmClientCompletionItemResolveRequest :: J.LspId -> J.CompletionItem -> J.CompletionItemResolveRequest
fmClientCompletionItemResolveRequest rid params
  = J.RequestMessage "2.0" rid J.SCompletionItemResolve params

-- * :leftwards_arrow_with_hook: [textDocument/hover](#textDocument_hover)
fmClientHoverRequest :: J.LspId -> J.TextDocumentPositionParams -> J.HoverRequest
fmClientHoverRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentHover params

-- * :leftwards_arrow_with_hook: [textDocument/signatureHelp](#textDocument_signatureHelp)
fmClientSignatureHelpRequest :: J.LspId -> J.TextDocumentPositionParams -> J.SignatureHelpRequest
fmClientSignatureHelpRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentSignatureHelp params

-- * :leftwards_arrow_with_hook: [textDocument/references](#textDocument_references)
fmClientReferencesRequest :: J.LspId -> J.ReferenceParams -> J.ReferencesRequest
fmClientReferencesRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentReferences params

-- * :leftwards_arrow_with_hook: [textDocument/documentHighlight](#textDocument_documentHighlight)
fmClientDocumentHighlightRequest :: J.LspId -> J.TextDocumentPositionParams -> J.DocumentHighlightRequest
fmClientDocumentHighlightRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentDocumentHighlight params

-- * :leftwards_arrow_with_hook: [textDocument/documentSymbol](#textDocument_documentSymbol)
fmClientDocumentSymbolRequest :: J.LspId -> J.DocumentSymbolParams -> J.DocumentSymbolRequest
fmClientDocumentSymbolRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentDocumentSymbol params

-- * :leftwards_arrow_with_hook: [textDocument/formatting](#textDocument_formatting)
fmClientDocumentFormattingRequest :: J.LspId -> J.DocumentFormattingParams -> J.DocumentFormattingRequest
fmClientDocumentFormattingRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentFormatting params

-- * :leftwards_arrow_with_hook: [textDocument/rangeFormatting](#textDocument_rangeFormatting)
fmClientDocumentRangeFormattingRequest :: J.LspId -> J.DocumentRangeFormattingParams -> J.DocumentRangeFormattingRequest
fmClientDocumentRangeFormattingRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentRangeFormatting params

-- * :leftwards_arrow_with_hook: [textDocument/onTypeFormatting](#textDocument_onTypeFormatting)
fmClientDocumentOnTypeFormattingRequest :: J.LspId -> J.DocumentOnTypeFormattingParams -> J.DocumentOnTypeFormattingRequest
fmClientDocumentOnTypeFormattingRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentOnTypeFormatting params

-- * :leftwards_arrow_with_hook: [textDocument/definition](#textDocument_definition)
fmClientDefinitionRequest :: J.LspId -> J.TextDocumentPositionParams -> J.DefinitionRequest
fmClientDefinitionRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentDefinition params

-- * :leftwards_arrow_with_hook: [textDocument/codeAction](#textDocument_codeAction)
fmClientCodeActionRequest :: J.LspId -> J.CodeActionParams -> J.CodeActionRequest
fmClientCodeActionRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentCodeAction params

-- * :leftwards_arrow_with_hook: [textDocument/codeLens](#textDocument_codeLens)
fmClientCodeLensRequest :: J.LspId -> J.CodeLensParams -> J.CodeLensRequest
fmClientCodeLensRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentCodeLens params

-- * :leftwards_arrow_with_hook: [codeLens/resolve](#codeLens_resolve)
fmClientCodeLensResolveRequest :: J.LspId -> J.CodeLens -> J.CodeLensResolveRequest
fmClientCodeLensResolveRequest rid params
  = J.RequestMessage "2.0" rid J.SCodeLensResolve params

-- * :leftwards_arrow_with_hook: [textDocument/documentLink](#textDocument_documentLink)
fmClientDocumentLinkRequest :: J.LspId -> J.DocumentLinkParams -> J.DocumentLinkRequest
fmClientDocumentLinkRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentDocumentLink params

-- * :leftwards_arrow_with_hook: [documentLink/resolve](#documentLink_resolve)
fmClientDocumentLinkResolveRequest :: J.LspId -> J.DocumentLink -> J.DocumentLinkResolveRequest
fmClientDocumentLinkResolveRequest rid params
  = J.RequestMessage "2.0" rid J.SDocumentLinkResolve params

-- * :leftwards_arrow_with_hook: [textDocument/rename](#textDocument_rename)
fmClientRenameRequest :: J.LspId -> J.RenameParams -> J.RenameRequest
fmClientRenameRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentRename params

-- * :leftwards_arrow_with_hook: [textDocument/prepareRename](#textDocument_prepareRename)
fmClientPrepareRenameRequest :: J.LspId -> J.TextDocumentPositionParams -> J.PrepareRenameRequest
fmClientPrepareRenameRequest rid params
  = J.RequestMessage "2.0" rid J.STextDocumentPrepareRename params
