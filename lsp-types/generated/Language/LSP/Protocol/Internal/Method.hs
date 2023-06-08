-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Method where

import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Kind as Kind
import qualified Data.Proxy
import qualified Data.Row as Row
import qualified Data.Void
import qualified GHC.TypeLits
import qualified Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams
import qualified Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditResult
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCall
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCallsParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCall
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCallsParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyPrepareParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CancelParams
import qualified Language.LSP.Protocol.Internal.Types.CodeAction
import qualified Language.LSP.Protocol.Internal.Types.CodeActionParams
import qualified Language.LSP.Protocol.Internal.Types.CodeActionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeLens
import qualified Language.LSP.Protocol.Internal.Types.CodeLensParams
import qualified Language.LSP.Protocol.Internal.Types.CodeLensRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ColorInformation
import qualified Language.LSP.Protocol.Internal.Types.ColorPresentation
import qualified Language.LSP.Protocol.Internal.Types.ColorPresentationParams
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.CompletionItem
import qualified Language.LSP.Protocol.Internal.Types.CompletionList
import qualified Language.LSP.Protocol.Internal.Types.CompletionParams
import qualified Language.LSP.Protocol.Internal.Types.CompletionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ConfigurationParams
import qualified Language.LSP.Protocol.Internal.Types.CreateFilesParams
import qualified Language.LSP.Protocol.Internal.Types.Declaration
import qualified Language.LSP.Protocol.Internal.Types.DeclarationLink
import qualified Language.LSP.Protocol.Internal.Types.DeclarationParams
import qualified Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.Definition
import qualified Language.LSP.Protocol.Internal.Types.DefinitionLink
import qualified Language.LSP.Protocol.Internal.Types.DefinitionParams
import qualified Language.LSP.Protocol.Internal.Types.DefinitionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DeleteFilesParams
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DidChangeNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWorkspaceFoldersParams
import qualified Language.LSP.Protocol.Internal.Types.DidCloseNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidCloseTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidOpenNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidOpenTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidSaveNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidSaveTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlight
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentLink
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbol
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandParams
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.FoldingRange
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeParams
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.Hover
import qualified Language.LSP.Protocol.Internal.Types.HoverParams
import qualified Language.LSP.Protocol.Internal.Types.HoverRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ImplementationParams
import qualified Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InitializeError
import qualified Language.LSP.Protocol.Internal.Types.InitializeParams
import qualified Language.LSP.Protocol.Internal.Types.InitializeResult
import qualified Language.LSP.Protocol.Internal.Types.InitializedParams
import qualified Language.LSP.Protocol.Internal.Types.InlayHint
import qualified Language.LSP.Protocol.Internal.Types.InlayHintParams
import qualified Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InlineValue
import qualified Language.LSP.Protocol.Internal.Types.InlineValueParams
import qualified Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeParams
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRanges
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Internal.Types.LogMessageParams
import qualified Language.LSP.Protocol.Internal.Types.LogTraceParams
import qualified Language.LSP.Protocol.Internal.Types.MessageActionItem
import qualified Language.LSP.Protocol.Internal.Types.Moniker
import qualified Language.LSP.Protocol.Internal.Types.MonikerParams
import qualified Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.PrepareRenameParams
import qualified Language.LSP.Protocol.Internal.Types.PrepareRenameResult
import qualified Language.LSP.Protocol.Internal.Types.ProgressParams
import qualified Language.LSP.Protocol.Internal.Types.PublishDiagnosticsParams
import qualified Language.LSP.Protocol.Internal.Types.ReferenceParams
import qualified Language.LSP.Protocol.Internal.Types.ReferenceRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.RegistrationParams
import qualified Language.LSP.Protocol.Internal.Types.RenameFilesParams
import qualified Language.LSP.Protocol.Internal.Types.RenameParams
import qualified Language.LSP.Protocol.Internal.Types.RenameRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SelectionRange
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeParams
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokens
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensDelta
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensRangeParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SetTraceParams
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentResult
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageParams
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageRequestParams
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelp
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpParams
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SymbolInformation
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSaveRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionParams
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyPrepareParams
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchySubtypesParams
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchySupertypesParams
import qualified Language.LSP.Protocol.Internal.Types.UnregistrationParams
import qualified Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressCancelParams
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressCreateParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEdit
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbol
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolRegistrationOptions
import qualified Language.LSP.Protocol.Message.Meta as MM
import qualified Language.LSP.Protocol.Types.Common

-- | A type representing a LSP method (or class of methods), intended to be used mostly at the type level.
type Method :: MM.MessageDirection -> MM.MessageKind -> Kind.Type
data Method f t where 
  Method_TextDocumentImplementation :: Method MM.ClientToServer MM.Request
  Method_TextDocumentTypeDefinition :: Method MM.ClientToServer MM.Request
  Method_WorkspaceWorkspaceFolders :: Method MM.ServerToClient MM.Request
  Method_WorkspaceConfiguration :: Method MM.ServerToClient MM.Request
  Method_TextDocumentDocumentColor :: Method MM.ClientToServer MM.Request
  Method_TextDocumentColorPresentation :: Method MM.ClientToServer MM.Request
  Method_TextDocumentFoldingRange :: Method MM.ClientToServer MM.Request
  Method_TextDocumentDeclaration :: Method MM.ClientToServer MM.Request
  Method_TextDocumentSelectionRange :: Method MM.ClientToServer MM.Request
  Method_WindowWorkDoneProgressCreate :: Method MM.ServerToClient MM.Request
  Method_TextDocumentPrepareCallHierarchy :: Method MM.ClientToServer MM.Request
  Method_CallHierarchyIncomingCalls :: Method MM.ClientToServer MM.Request
  Method_CallHierarchyOutgoingCalls :: Method MM.ClientToServer MM.Request
  Method_TextDocumentSemanticTokensFull :: Method MM.ClientToServer MM.Request
  Method_TextDocumentSemanticTokensFullDelta :: Method MM.ClientToServer MM.Request
  Method_TextDocumentSemanticTokensRange :: Method MM.ClientToServer MM.Request
  Method_WorkspaceSemanticTokensRefresh :: Method MM.ServerToClient MM.Request
  Method_WindowShowDocument :: Method MM.ServerToClient MM.Request
  Method_TextDocumentLinkedEditingRange :: Method MM.ClientToServer MM.Request
  Method_WorkspaceWillCreateFiles :: Method MM.ClientToServer MM.Request
  Method_WorkspaceWillRenameFiles :: Method MM.ClientToServer MM.Request
  Method_WorkspaceWillDeleteFiles :: Method MM.ClientToServer MM.Request
  Method_TextDocumentMoniker :: Method MM.ClientToServer MM.Request
  Method_TextDocumentPrepareTypeHierarchy :: Method MM.ClientToServer MM.Request
  Method_TypeHierarchySupertypes :: Method MM.ClientToServer MM.Request
  Method_TypeHierarchySubtypes :: Method MM.ClientToServer MM.Request
  Method_TextDocumentInlineValue :: Method MM.ClientToServer MM.Request
  Method_WorkspaceInlineValueRefresh :: Method MM.ServerToClient MM.Request
  Method_TextDocumentInlayHint :: Method MM.ClientToServer MM.Request
  Method_InlayHintResolve :: Method MM.ClientToServer MM.Request
  Method_WorkspaceInlayHintRefresh :: Method MM.ServerToClient MM.Request
  Method_TextDocumentDiagnostic :: Method MM.ClientToServer MM.Request
  Method_WorkspaceDiagnostic :: Method MM.ClientToServer MM.Request
  Method_WorkspaceDiagnosticRefresh :: Method MM.ServerToClient MM.Request
  Method_ClientRegisterCapability :: Method MM.ServerToClient MM.Request
  Method_ClientUnregisterCapability :: Method MM.ServerToClient MM.Request
  Method_Initialize :: Method MM.ClientToServer MM.Request
  Method_Shutdown :: Method MM.ClientToServer MM.Request
  Method_WindowShowMessageRequest :: Method MM.ServerToClient MM.Request
  Method_TextDocumentWillSaveWaitUntil :: Method MM.ClientToServer MM.Request
  Method_TextDocumentCompletion :: Method MM.ClientToServer MM.Request
  Method_CompletionItemResolve :: Method MM.ClientToServer MM.Request
  Method_TextDocumentHover :: Method MM.ClientToServer MM.Request
  Method_TextDocumentSignatureHelp :: Method MM.ClientToServer MM.Request
  Method_TextDocumentDefinition :: Method MM.ClientToServer MM.Request
  Method_TextDocumentReferences :: Method MM.ClientToServer MM.Request
  Method_TextDocumentDocumentHighlight :: Method MM.ClientToServer MM.Request
  Method_TextDocumentDocumentSymbol :: Method MM.ClientToServer MM.Request
  Method_TextDocumentCodeAction :: Method MM.ClientToServer MM.Request
  Method_CodeActionResolve :: Method MM.ClientToServer MM.Request
  Method_WorkspaceSymbol :: Method MM.ClientToServer MM.Request
  Method_WorkspaceSymbolResolve :: Method MM.ClientToServer MM.Request
  Method_TextDocumentCodeLens :: Method MM.ClientToServer MM.Request
  Method_CodeLensResolve :: Method MM.ClientToServer MM.Request
  Method_WorkspaceCodeLensRefresh :: Method MM.ServerToClient MM.Request
  Method_TextDocumentDocumentLink :: Method MM.ClientToServer MM.Request
  Method_DocumentLinkResolve :: Method MM.ClientToServer MM.Request
  Method_TextDocumentFormatting :: Method MM.ClientToServer MM.Request
  Method_TextDocumentRangeFormatting :: Method MM.ClientToServer MM.Request
  Method_TextDocumentOnTypeFormatting :: Method MM.ClientToServer MM.Request
  Method_TextDocumentRename :: Method MM.ClientToServer MM.Request
  Method_TextDocumentPrepareRename :: Method MM.ClientToServer MM.Request
  Method_WorkspaceExecuteCommand :: Method MM.ClientToServer MM.Request
  Method_WorkspaceApplyEdit :: Method MM.ServerToClient MM.Request
  Method_WorkspaceDidChangeWorkspaceFolders :: Method MM.ClientToServer MM.Notification
  Method_WindowWorkDoneProgressCancel :: Method MM.ClientToServer MM.Notification
  Method_WorkspaceDidCreateFiles :: Method MM.ClientToServer MM.Notification
  Method_WorkspaceDidRenameFiles :: Method MM.ClientToServer MM.Notification
  Method_WorkspaceDidDeleteFiles :: Method MM.ClientToServer MM.Notification
  Method_NotebookDocumentDidOpen :: Method MM.ClientToServer MM.Notification
  Method_NotebookDocumentDidChange :: Method MM.ClientToServer MM.Notification
  Method_NotebookDocumentDidSave :: Method MM.ClientToServer MM.Notification
  Method_NotebookDocumentDidClose :: Method MM.ClientToServer MM.Notification
  Method_Initialized :: Method MM.ClientToServer MM.Notification
  Method_Exit :: Method MM.ClientToServer MM.Notification
  Method_WorkspaceDidChangeConfiguration :: Method MM.ClientToServer MM.Notification
  Method_WindowShowMessage :: Method MM.ServerToClient MM.Notification
  Method_WindowLogMessage :: Method MM.ServerToClient MM.Notification
  Method_TelemetryEvent :: Method MM.ServerToClient MM.Notification
  Method_TextDocumentDidOpen :: Method MM.ClientToServer MM.Notification
  Method_TextDocumentDidChange :: Method MM.ClientToServer MM.Notification
  Method_TextDocumentDidClose :: Method MM.ClientToServer MM.Notification
  Method_TextDocumentDidSave :: Method MM.ClientToServer MM.Notification
  Method_TextDocumentWillSave :: Method MM.ClientToServer MM.Notification
  Method_WorkspaceDidChangeWatchedFiles :: Method MM.ClientToServer MM.Notification
  Method_TextDocumentPublishDiagnostics :: Method MM.ServerToClient MM.Notification
  Method_SetTrace :: Method MM.ClientToServer MM.Notification
  Method_LogTrace :: Method MM.ServerToClient MM.Notification
  Method_CancelRequest :: Method f MM.Notification
  Method_Progress :: Method f MM.Notification
  Method_CustomMethod :: GHC.TypeLits.Symbol -> Method f t

-- | Maps a LSP method to its parameter type.
type MessageParams :: forall f t . Method f t -> Kind.Type
type family MessageParams (m ::  Method f t) where 
  MessageParams Method_TextDocumentImplementation = Language.LSP.Protocol.Internal.Types.ImplementationParams.ImplementationParams
  MessageParams Method_TextDocumentTypeDefinition = Language.LSP.Protocol.Internal.Types.TypeDefinitionParams.TypeDefinitionParams
  MessageParams Method_WorkspaceWorkspaceFolders = Maybe Data.Void.Void
  MessageParams Method_WorkspaceConfiguration = Language.LSP.Protocol.Internal.Types.ConfigurationParams.ConfigurationParams
  MessageParams Method_TextDocumentDocumentColor = Language.LSP.Protocol.Internal.Types.DocumentColorParams.DocumentColorParams
  MessageParams Method_TextDocumentColorPresentation = Language.LSP.Protocol.Internal.Types.ColorPresentationParams.ColorPresentationParams
  MessageParams Method_TextDocumentFoldingRange = Language.LSP.Protocol.Internal.Types.FoldingRangeParams.FoldingRangeParams
  MessageParams Method_TextDocumentDeclaration = Language.LSP.Protocol.Internal.Types.DeclarationParams.DeclarationParams
  MessageParams Method_TextDocumentSelectionRange = Language.LSP.Protocol.Internal.Types.SelectionRangeParams.SelectionRangeParams
  MessageParams Method_WindowWorkDoneProgressCreate = Language.LSP.Protocol.Internal.Types.WorkDoneProgressCreateParams.WorkDoneProgressCreateParams
  MessageParams Method_TextDocumentPrepareCallHierarchy = Language.LSP.Protocol.Internal.Types.CallHierarchyPrepareParams.CallHierarchyPrepareParams
  MessageParams Method_CallHierarchyIncomingCalls = Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCallsParams.CallHierarchyIncomingCallsParams
  MessageParams Method_CallHierarchyOutgoingCalls = Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCallsParams.CallHierarchyOutgoingCallsParams
  MessageParams Method_TextDocumentSemanticTokensFull = Language.LSP.Protocol.Internal.Types.SemanticTokensParams.SemanticTokensParams
  MessageParams Method_TextDocumentSemanticTokensFullDelta = Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaParams.SemanticTokensDeltaParams
  MessageParams Method_TextDocumentSemanticTokensRange = Language.LSP.Protocol.Internal.Types.SemanticTokensRangeParams.SemanticTokensRangeParams
  MessageParams Method_WorkspaceSemanticTokensRefresh = Maybe Data.Void.Void
  MessageParams Method_WindowShowDocument = Language.LSP.Protocol.Internal.Types.ShowDocumentParams.ShowDocumentParams
  MessageParams Method_TextDocumentLinkedEditingRange = Language.LSP.Protocol.Internal.Types.LinkedEditingRangeParams.LinkedEditingRangeParams
  MessageParams Method_WorkspaceWillCreateFiles = Language.LSP.Protocol.Internal.Types.CreateFilesParams.CreateFilesParams
  MessageParams Method_WorkspaceWillRenameFiles = Language.LSP.Protocol.Internal.Types.RenameFilesParams.RenameFilesParams
  MessageParams Method_WorkspaceWillDeleteFiles = Language.LSP.Protocol.Internal.Types.DeleteFilesParams.DeleteFilesParams
  MessageParams Method_TextDocumentMoniker = Language.LSP.Protocol.Internal.Types.MonikerParams.MonikerParams
  MessageParams Method_TextDocumentPrepareTypeHierarchy = Language.LSP.Protocol.Internal.Types.TypeHierarchyPrepareParams.TypeHierarchyPrepareParams
  MessageParams Method_TypeHierarchySupertypes = Language.LSP.Protocol.Internal.Types.TypeHierarchySupertypesParams.TypeHierarchySupertypesParams
  MessageParams Method_TypeHierarchySubtypes = Language.LSP.Protocol.Internal.Types.TypeHierarchySubtypesParams.TypeHierarchySubtypesParams
  MessageParams Method_TextDocumentInlineValue = Language.LSP.Protocol.Internal.Types.InlineValueParams.InlineValueParams
  MessageParams Method_WorkspaceInlineValueRefresh = Maybe Data.Void.Void
  MessageParams Method_TextDocumentInlayHint = Language.LSP.Protocol.Internal.Types.InlayHintParams.InlayHintParams
  MessageParams Method_InlayHintResolve = Language.LSP.Protocol.Internal.Types.InlayHint.InlayHint
  MessageParams Method_WorkspaceInlayHintRefresh = Maybe Data.Void.Void
  MessageParams Method_TextDocumentDiagnostic = Language.LSP.Protocol.Internal.Types.DocumentDiagnosticParams.DocumentDiagnosticParams
  MessageParams Method_WorkspaceDiagnostic = Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticParams.WorkspaceDiagnosticParams
  MessageParams Method_WorkspaceDiagnosticRefresh = Maybe Data.Void.Void
  MessageParams Method_ClientRegisterCapability = Language.LSP.Protocol.Internal.Types.RegistrationParams.RegistrationParams
  MessageParams Method_ClientUnregisterCapability = Language.LSP.Protocol.Internal.Types.UnregistrationParams.UnregistrationParams
  MessageParams Method_Initialize = Language.LSP.Protocol.Internal.Types.InitializeParams.InitializeParams
  MessageParams Method_Shutdown = Maybe Data.Void.Void
  MessageParams Method_WindowShowMessageRequest = Language.LSP.Protocol.Internal.Types.ShowMessageRequestParams.ShowMessageRequestParams
  MessageParams Method_TextDocumentWillSaveWaitUntil = Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams.WillSaveTextDocumentParams
  MessageParams Method_TextDocumentCompletion = Language.LSP.Protocol.Internal.Types.CompletionParams.CompletionParams
  MessageParams Method_CompletionItemResolve = Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem
  MessageParams Method_TextDocumentHover = Language.LSP.Protocol.Internal.Types.HoverParams.HoverParams
  MessageParams Method_TextDocumentSignatureHelp = Language.LSP.Protocol.Internal.Types.SignatureHelpParams.SignatureHelpParams
  MessageParams Method_TextDocumentDefinition = Language.LSP.Protocol.Internal.Types.DefinitionParams.DefinitionParams
  MessageParams Method_TextDocumentReferences = Language.LSP.Protocol.Internal.Types.ReferenceParams.ReferenceParams
  MessageParams Method_TextDocumentDocumentHighlight = Language.LSP.Protocol.Internal.Types.DocumentHighlightParams.DocumentHighlightParams
  MessageParams Method_TextDocumentDocumentSymbol = Language.LSP.Protocol.Internal.Types.DocumentSymbolParams.DocumentSymbolParams
  MessageParams Method_TextDocumentCodeAction = Language.LSP.Protocol.Internal.Types.CodeActionParams.CodeActionParams
  MessageParams Method_CodeActionResolve = Language.LSP.Protocol.Internal.Types.CodeAction.CodeAction
  MessageParams Method_WorkspaceSymbol = Language.LSP.Protocol.Internal.Types.WorkspaceSymbolParams.WorkspaceSymbolParams
  MessageParams Method_WorkspaceSymbolResolve = Language.LSP.Protocol.Internal.Types.WorkspaceSymbol.WorkspaceSymbol
  MessageParams Method_TextDocumentCodeLens = Language.LSP.Protocol.Internal.Types.CodeLensParams.CodeLensParams
  MessageParams Method_CodeLensResolve = Language.LSP.Protocol.Internal.Types.CodeLens.CodeLens
  MessageParams Method_WorkspaceCodeLensRefresh = Maybe Data.Void.Void
  MessageParams Method_TextDocumentDocumentLink = Language.LSP.Protocol.Internal.Types.DocumentLinkParams.DocumentLinkParams
  MessageParams Method_DocumentLinkResolve = Language.LSP.Protocol.Internal.Types.DocumentLink.DocumentLink
  MessageParams Method_TextDocumentFormatting = Language.LSP.Protocol.Internal.Types.DocumentFormattingParams.DocumentFormattingParams
  MessageParams Method_TextDocumentRangeFormatting = Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams.DocumentRangeFormattingParams
  MessageParams Method_TextDocumentOnTypeFormatting = Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingParams.DocumentOnTypeFormattingParams
  MessageParams Method_TextDocumentRename = Language.LSP.Protocol.Internal.Types.RenameParams.RenameParams
  MessageParams Method_TextDocumentPrepareRename = Language.LSP.Protocol.Internal.Types.PrepareRenameParams.PrepareRenameParams
  MessageParams Method_WorkspaceExecuteCommand = Language.LSP.Protocol.Internal.Types.ExecuteCommandParams.ExecuteCommandParams
  MessageParams Method_WorkspaceApplyEdit = Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams.ApplyWorkspaceEditParams
  MessageParams Method_WorkspaceDidChangeWorkspaceFolders = Language.LSP.Protocol.Internal.Types.DidChangeWorkspaceFoldersParams.DidChangeWorkspaceFoldersParams
  MessageParams Method_WindowWorkDoneProgressCancel = Language.LSP.Protocol.Internal.Types.WorkDoneProgressCancelParams.WorkDoneProgressCancelParams
  MessageParams Method_WorkspaceDidCreateFiles = Language.LSP.Protocol.Internal.Types.CreateFilesParams.CreateFilesParams
  MessageParams Method_WorkspaceDidRenameFiles = Language.LSP.Protocol.Internal.Types.RenameFilesParams.RenameFilesParams
  MessageParams Method_WorkspaceDidDeleteFiles = Language.LSP.Protocol.Internal.Types.DeleteFilesParams.DeleteFilesParams
  MessageParams Method_NotebookDocumentDidOpen = Language.LSP.Protocol.Internal.Types.DidOpenNotebookDocumentParams.DidOpenNotebookDocumentParams
  MessageParams Method_NotebookDocumentDidChange = Language.LSP.Protocol.Internal.Types.DidChangeNotebookDocumentParams.DidChangeNotebookDocumentParams
  MessageParams Method_NotebookDocumentDidSave = Language.LSP.Protocol.Internal.Types.DidSaveNotebookDocumentParams.DidSaveNotebookDocumentParams
  MessageParams Method_NotebookDocumentDidClose = Language.LSP.Protocol.Internal.Types.DidCloseNotebookDocumentParams.DidCloseNotebookDocumentParams
  MessageParams Method_Initialized = Language.LSP.Protocol.Internal.Types.InitializedParams.InitializedParams
  MessageParams Method_Exit = Maybe Data.Void.Void
  MessageParams Method_WorkspaceDidChangeConfiguration = Language.LSP.Protocol.Internal.Types.DidChangeConfigurationParams.DidChangeConfigurationParams
  MessageParams Method_WindowShowMessage = Language.LSP.Protocol.Internal.Types.ShowMessageParams.ShowMessageParams
  MessageParams Method_WindowLogMessage = Language.LSP.Protocol.Internal.Types.LogMessageParams.LogMessageParams
  MessageParams Method_TelemetryEvent = Data.Aeson.Value
  MessageParams Method_TextDocumentDidOpen = Language.LSP.Protocol.Internal.Types.DidOpenTextDocumentParams.DidOpenTextDocumentParams
  MessageParams Method_TextDocumentDidChange = Language.LSP.Protocol.Internal.Types.DidChangeTextDocumentParams.DidChangeTextDocumentParams
  MessageParams Method_TextDocumentDidClose = Language.LSP.Protocol.Internal.Types.DidCloseTextDocumentParams.DidCloseTextDocumentParams
  MessageParams Method_TextDocumentDidSave = Language.LSP.Protocol.Internal.Types.DidSaveTextDocumentParams.DidSaveTextDocumentParams
  MessageParams Method_TextDocumentWillSave = Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams.WillSaveTextDocumentParams
  MessageParams Method_WorkspaceDidChangeWatchedFiles = Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesParams.DidChangeWatchedFilesParams
  MessageParams Method_TextDocumentPublishDiagnostics = Language.LSP.Protocol.Internal.Types.PublishDiagnosticsParams.PublishDiagnosticsParams
  MessageParams Method_SetTrace = Language.LSP.Protocol.Internal.Types.SetTraceParams.SetTraceParams
  MessageParams Method_LogTrace = Language.LSP.Protocol.Internal.Types.LogTraceParams.LogTraceParams
  MessageParams Method_CancelRequest = Language.LSP.Protocol.Internal.Types.CancelParams.CancelParams
  MessageParams Method_Progress = Language.LSP.Protocol.Internal.Types.ProgressParams.ProgressParams
  MessageParams (Method_CustomMethod s) = Aeson.Value

-- | Maps a LSP method to its result type.
type MessageResult :: forall f t . Method f t -> Kind.Type
type family MessageResult (m ::  Method f t) where 
  MessageResult Method_TextDocumentImplementation = (Language.LSP.Protocol.Internal.Types.Definition.Definition Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.DefinitionLink.DefinitionLink] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_TextDocumentTypeDefinition = (Language.LSP.Protocol.Internal.Types.Definition.Definition Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.DefinitionLink.DefinitionLink] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_WorkspaceWorkspaceFolders = ([Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceConfiguration = [Data.Aeson.Value]
  MessageResult Method_TextDocumentDocumentColor = [Language.LSP.Protocol.Internal.Types.ColorInformation.ColorInformation]
  MessageResult Method_TextDocumentColorPresentation = [Language.LSP.Protocol.Internal.Types.ColorPresentation.ColorPresentation]
  MessageResult Method_TextDocumentFoldingRange = ([Language.LSP.Protocol.Internal.Types.FoldingRange.FoldingRange] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentDeclaration = (Language.LSP.Protocol.Internal.Types.Declaration.Declaration Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.DeclarationLink.DeclarationLink] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_TextDocumentSelectionRange = ([Language.LSP.Protocol.Internal.Types.SelectionRange.SelectionRange] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WindowWorkDoneProgressCreate = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_TextDocumentPrepareCallHierarchy = ([Language.LSP.Protocol.Internal.Types.CallHierarchyItem.CallHierarchyItem] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_CallHierarchyIncomingCalls = ([Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCall.CallHierarchyIncomingCall] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_CallHierarchyOutgoingCalls = ([Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCall.CallHierarchyOutgoingCall] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentSemanticTokensFull = (Language.LSP.Protocol.Internal.Types.SemanticTokens.SemanticTokens Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentSemanticTokensFullDelta = (Language.LSP.Protocol.Internal.Types.SemanticTokens.SemanticTokens Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.SemanticTokensDelta.SemanticTokensDelta Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_TextDocumentSemanticTokensRange = (Language.LSP.Protocol.Internal.Types.SemanticTokens.SemanticTokens Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceSemanticTokensRefresh = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_WindowShowDocument = Language.LSP.Protocol.Internal.Types.ShowDocumentResult.ShowDocumentResult
  MessageResult Method_TextDocumentLinkedEditingRange = (Language.LSP.Protocol.Internal.Types.LinkedEditingRanges.LinkedEditingRanges Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceWillCreateFiles = (Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceWillRenameFiles = (Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceWillDeleteFiles = (Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentMoniker = ([Language.LSP.Protocol.Internal.Types.Moniker.Moniker] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentPrepareTypeHierarchy = ([Language.LSP.Protocol.Internal.Types.TypeHierarchyItem.TypeHierarchyItem] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TypeHierarchySupertypes = ([Language.LSP.Protocol.Internal.Types.TypeHierarchyItem.TypeHierarchyItem] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TypeHierarchySubtypes = ([Language.LSP.Protocol.Internal.Types.TypeHierarchyItem.TypeHierarchyItem] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentInlineValue = ([Language.LSP.Protocol.Internal.Types.InlineValue.InlineValue] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceInlineValueRefresh = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_TextDocumentInlayHint = ([Language.LSP.Protocol.Internal.Types.InlayHint.InlayHint] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_InlayHintResolve = Language.LSP.Protocol.Internal.Types.InlayHint.InlayHint
  MessageResult Method_WorkspaceInlayHintRefresh = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_TextDocumentDiagnostic = Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReport.DocumentDiagnosticReport
  MessageResult Method_WorkspaceDiagnostic = Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReport.WorkspaceDiagnosticReport
  MessageResult Method_WorkspaceDiagnosticRefresh = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_ClientRegisterCapability = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_ClientUnregisterCapability = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_Initialize = Language.LSP.Protocol.Internal.Types.InitializeResult.InitializeResult
  MessageResult Method_Shutdown = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_WindowShowMessageRequest = (Language.LSP.Protocol.Internal.Types.MessageActionItem.MessageActionItem Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentWillSaveWaitUntil = ([Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentCompletion = ([Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem] Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.CompletionList.CompletionList Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_CompletionItemResolve = Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem
  MessageResult Method_TextDocumentHover = (Language.LSP.Protocol.Internal.Types.Hover.Hover Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentSignatureHelp = (Language.LSP.Protocol.Internal.Types.SignatureHelp.SignatureHelp Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentDefinition = (Language.LSP.Protocol.Internal.Types.Definition.Definition Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.DefinitionLink.DefinitionLink] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_TextDocumentReferences = ([Language.LSP.Protocol.Internal.Types.Location.Location] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentDocumentHighlight = ([Language.LSP.Protocol.Internal.Types.DocumentHighlight.DocumentHighlight] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentDocumentSymbol = ([Language.LSP.Protocol.Internal.Types.SymbolInformation.SymbolInformation] Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.DocumentSymbol.DocumentSymbol] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_TextDocumentCodeAction = ([(Language.LSP.Protocol.Internal.Types.Command.Command Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.CodeAction.CodeAction)] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_CodeActionResolve = Language.LSP.Protocol.Internal.Types.CodeAction.CodeAction
  MessageResult Method_WorkspaceSymbol = ([Language.LSP.Protocol.Internal.Types.SymbolInformation.SymbolInformation] Language.LSP.Protocol.Types.Common.|? ([Language.LSP.Protocol.Internal.Types.WorkspaceSymbol.WorkspaceSymbol] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  MessageResult Method_WorkspaceSymbolResolve = Language.LSP.Protocol.Internal.Types.WorkspaceSymbol.WorkspaceSymbol
  MessageResult Method_TextDocumentCodeLens = ([Language.LSP.Protocol.Internal.Types.CodeLens.CodeLens] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_CodeLensResolve = Language.LSP.Protocol.Internal.Types.CodeLens.CodeLens
  MessageResult Method_WorkspaceCodeLensRefresh = Language.LSP.Protocol.Types.Common.Null
  MessageResult Method_TextDocumentDocumentLink = ([Language.LSP.Protocol.Internal.Types.DocumentLink.DocumentLink] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_DocumentLinkResolve = Language.LSP.Protocol.Internal.Types.DocumentLink.DocumentLink
  MessageResult Method_TextDocumentFormatting = ([Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentRangeFormatting = ([Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentOnTypeFormatting = ([Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentRename = (Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_TextDocumentPrepareRename = (Language.LSP.Protocol.Internal.Types.PrepareRenameResult.PrepareRenameResult Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceExecuteCommand = (Data.Aeson.Value Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  MessageResult Method_WorkspaceApplyEdit = Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditResult.ApplyWorkspaceEditResult
  MessageResult (Method_CustomMethod s) = Aeson.Value

-- | Maps a LSP method to its error data type.
type ErrorData :: forall f t . Method f t -> Kind.Type
type family ErrorData (m ::  Method f t) where 
  ErrorData Method_TextDocumentImplementation = Maybe Data.Void.Void
  ErrorData Method_TextDocumentTypeDefinition = Maybe Data.Void.Void
  ErrorData Method_WorkspaceWorkspaceFolders = Maybe Data.Void.Void
  ErrorData Method_WorkspaceConfiguration = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDocumentColor = Maybe Data.Void.Void
  ErrorData Method_TextDocumentColorPresentation = Maybe Data.Void.Void
  ErrorData Method_TextDocumentFoldingRange = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDeclaration = Maybe Data.Void.Void
  ErrorData Method_TextDocumentSelectionRange = Maybe Data.Void.Void
  ErrorData Method_WindowWorkDoneProgressCreate = Maybe Data.Void.Void
  ErrorData Method_TextDocumentPrepareCallHierarchy = Maybe Data.Void.Void
  ErrorData Method_CallHierarchyIncomingCalls = Maybe Data.Void.Void
  ErrorData Method_CallHierarchyOutgoingCalls = Maybe Data.Void.Void
  ErrorData Method_TextDocumentSemanticTokensFull = Maybe Data.Void.Void
  ErrorData Method_TextDocumentSemanticTokensFullDelta = Maybe Data.Void.Void
  ErrorData Method_TextDocumentSemanticTokensRange = Maybe Data.Void.Void
  ErrorData Method_WorkspaceSemanticTokensRefresh = Maybe Data.Void.Void
  ErrorData Method_WindowShowDocument = Maybe Data.Void.Void
  ErrorData Method_TextDocumentLinkedEditingRange = Maybe Data.Void.Void
  ErrorData Method_WorkspaceWillCreateFiles = Maybe Data.Void.Void
  ErrorData Method_WorkspaceWillRenameFiles = Maybe Data.Void.Void
  ErrorData Method_WorkspaceWillDeleteFiles = Maybe Data.Void.Void
  ErrorData Method_TextDocumentMoniker = Maybe Data.Void.Void
  ErrorData Method_TextDocumentPrepareTypeHierarchy = Maybe Data.Void.Void
  ErrorData Method_TypeHierarchySupertypes = Maybe Data.Void.Void
  ErrorData Method_TypeHierarchySubtypes = Maybe Data.Void.Void
  ErrorData Method_TextDocumentInlineValue = Maybe Data.Void.Void
  ErrorData Method_WorkspaceInlineValueRefresh = Maybe Data.Void.Void
  ErrorData Method_TextDocumentInlayHint = Maybe Data.Void.Void
  ErrorData Method_InlayHintResolve = Maybe Data.Void.Void
  ErrorData Method_WorkspaceInlayHintRefresh = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDiagnostic = Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData.DiagnosticServerCancellationData
  ErrorData Method_WorkspaceDiagnostic = Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData.DiagnosticServerCancellationData
  ErrorData Method_WorkspaceDiagnosticRefresh = Maybe Data.Void.Void
  ErrorData Method_ClientRegisterCapability = Maybe Data.Void.Void
  ErrorData Method_ClientUnregisterCapability = Maybe Data.Void.Void
  ErrorData Method_Initialize = Language.LSP.Protocol.Internal.Types.InitializeError.InitializeError
  ErrorData Method_Shutdown = Maybe Data.Void.Void
  ErrorData Method_WindowShowMessageRequest = Maybe Data.Void.Void
  ErrorData Method_TextDocumentWillSaveWaitUntil = Maybe Data.Void.Void
  ErrorData Method_TextDocumentCompletion = Maybe Data.Void.Void
  ErrorData Method_CompletionItemResolve = Maybe Data.Void.Void
  ErrorData Method_TextDocumentHover = Maybe Data.Void.Void
  ErrorData Method_TextDocumentSignatureHelp = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDefinition = Maybe Data.Void.Void
  ErrorData Method_TextDocumentReferences = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDocumentHighlight = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDocumentSymbol = Maybe Data.Void.Void
  ErrorData Method_TextDocumentCodeAction = Maybe Data.Void.Void
  ErrorData Method_CodeActionResolve = Maybe Data.Void.Void
  ErrorData Method_WorkspaceSymbol = Maybe Data.Void.Void
  ErrorData Method_WorkspaceSymbolResolve = Maybe Data.Void.Void
  ErrorData Method_TextDocumentCodeLens = Maybe Data.Void.Void
  ErrorData Method_CodeLensResolve = Maybe Data.Void.Void
  ErrorData Method_WorkspaceCodeLensRefresh = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDocumentLink = Maybe Data.Void.Void
  ErrorData Method_DocumentLinkResolve = Maybe Data.Void.Void
  ErrorData Method_TextDocumentFormatting = Maybe Data.Void.Void
  ErrorData Method_TextDocumentRangeFormatting = Maybe Data.Void.Void
  ErrorData Method_TextDocumentOnTypeFormatting = Maybe Data.Void.Void
  ErrorData Method_TextDocumentRename = Maybe Data.Void.Void
  ErrorData Method_TextDocumentPrepareRename = Maybe Data.Void.Void
  ErrorData Method_WorkspaceExecuteCommand = Maybe Data.Void.Void
  ErrorData Method_WorkspaceApplyEdit = Maybe Data.Void.Void
  ErrorData (Method_CustomMethod s) = Aeson.Value

-- | Maps a LSP method to its registration options type.
type RegistrationOptions :: forall f t . Method f t -> Kind.Type
type family RegistrationOptions (m ::  Method f t) where 
  RegistrationOptions Method_TextDocumentImplementation = Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions.ImplementationRegistrationOptions
  RegistrationOptions Method_TextDocumentTypeDefinition = Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions.TypeDefinitionRegistrationOptions
  RegistrationOptions Method_WorkspaceWorkspaceFolders = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceConfiguration = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentDocumentColor = Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions.DocumentColorRegistrationOptions
  RegistrationOptions Method_TextDocumentColorPresentation = (Row.Rec ("workDoneProgress" Row..== (Maybe Bool) Row..+ ("documentSelector" Row..== (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null) Row..+ Row.Empty)))
  RegistrationOptions Method_TextDocumentFoldingRange = Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions.FoldingRangeRegistrationOptions
  RegistrationOptions Method_TextDocumentDeclaration = Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions.DeclarationRegistrationOptions
  RegistrationOptions Method_TextDocumentSelectionRange = Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions.SelectionRangeRegistrationOptions
  RegistrationOptions Method_WindowWorkDoneProgressCreate = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentPrepareCallHierarchy = Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions.CallHierarchyRegistrationOptions
  RegistrationOptions Method_CallHierarchyIncomingCalls = Maybe Data.Void.Void
  RegistrationOptions Method_CallHierarchyOutgoingCalls = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentSemanticTokensFull = Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions.SemanticTokensRegistrationOptions
  RegistrationOptions Method_TextDocumentSemanticTokensFullDelta = Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions.SemanticTokensRegistrationOptions
  RegistrationOptions Method_TextDocumentSemanticTokensRange = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceSemanticTokensRefresh = Maybe Data.Void.Void
  RegistrationOptions Method_WindowShowDocument = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentLinkedEditingRange = Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions.LinkedEditingRangeRegistrationOptions
  RegistrationOptions Method_WorkspaceWillCreateFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_WorkspaceWillRenameFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_WorkspaceWillDeleteFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_TextDocumentMoniker = Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions.MonikerRegistrationOptions
  RegistrationOptions Method_TextDocumentPrepareTypeHierarchy = Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions.TypeHierarchyRegistrationOptions
  RegistrationOptions Method_TypeHierarchySupertypes = Maybe Data.Void.Void
  RegistrationOptions Method_TypeHierarchySubtypes = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentInlineValue = Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions.InlineValueRegistrationOptions
  RegistrationOptions Method_WorkspaceInlineValueRefresh = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentInlayHint = Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions.InlayHintRegistrationOptions
  RegistrationOptions Method_InlayHintResolve = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceInlayHintRefresh = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentDiagnostic = Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions.DiagnosticRegistrationOptions
  RegistrationOptions Method_WorkspaceDiagnostic = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceDiagnosticRefresh = Maybe Data.Void.Void
  RegistrationOptions Method_ClientRegisterCapability = Maybe Data.Void.Void
  RegistrationOptions Method_ClientUnregisterCapability = Maybe Data.Void.Void
  RegistrationOptions Method_Initialize = Maybe Data.Void.Void
  RegistrationOptions Method_Shutdown = Maybe Data.Void.Void
  RegistrationOptions Method_WindowShowMessageRequest = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentWillSaveWaitUntil = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  RegistrationOptions Method_TextDocumentCompletion = Language.LSP.Protocol.Internal.Types.CompletionRegistrationOptions.CompletionRegistrationOptions
  RegistrationOptions Method_CompletionItemResolve = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentHover = Language.LSP.Protocol.Internal.Types.HoverRegistrationOptions.HoverRegistrationOptions
  RegistrationOptions Method_TextDocumentSignatureHelp = Language.LSP.Protocol.Internal.Types.SignatureHelpRegistrationOptions.SignatureHelpRegistrationOptions
  RegistrationOptions Method_TextDocumentDefinition = Language.LSP.Protocol.Internal.Types.DefinitionRegistrationOptions.DefinitionRegistrationOptions
  RegistrationOptions Method_TextDocumentReferences = Language.LSP.Protocol.Internal.Types.ReferenceRegistrationOptions.ReferenceRegistrationOptions
  RegistrationOptions Method_TextDocumentDocumentHighlight = Language.LSP.Protocol.Internal.Types.DocumentHighlightRegistrationOptions.DocumentHighlightRegistrationOptions
  RegistrationOptions Method_TextDocumentDocumentSymbol = Language.LSP.Protocol.Internal.Types.DocumentSymbolRegistrationOptions.DocumentSymbolRegistrationOptions
  RegistrationOptions Method_TextDocumentCodeAction = Language.LSP.Protocol.Internal.Types.CodeActionRegistrationOptions.CodeActionRegistrationOptions
  RegistrationOptions Method_CodeActionResolve = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceSymbol = Language.LSP.Protocol.Internal.Types.WorkspaceSymbolRegistrationOptions.WorkspaceSymbolRegistrationOptions
  RegistrationOptions Method_WorkspaceSymbolResolve = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentCodeLens = Language.LSP.Protocol.Internal.Types.CodeLensRegistrationOptions.CodeLensRegistrationOptions
  RegistrationOptions Method_CodeLensResolve = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceCodeLensRefresh = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentDocumentLink = Language.LSP.Protocol.Internal.Types.DocumentLinkRegistrationOptions.DocumentLinkRegistrationOptions
  RegistrationOptions Method_DocumentLinkResolve = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentFormatting = Language.LSP.Protocol.Internal.Types.DocumentFormattingRegistrationOptions.DocumentFormattingRegistrationOptions
  RegistrationOptions Method_TextDocumentRangeFormatting = Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingRegistrationOptions.DocumentRangeFormattingRegistrationOptions
  RegistrationOptions Method_TextDocumentOnTypeFormatting = Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingRegistrationOptions.DocumentOnTypeFormattingRegistrationOptions
  RegistrationOptions Method_TextDocumentRename = Language.LSP.Protocol.Internal.Types.RenameRegistrationOptions.RenameRegistrationOptions
  RegistrationOptions Method_TextDocumentPrepareRename = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceExecuteCommand = Language.LSP.Protocol.Internal.Types.ExecuteCommandRegistrationOptions.ExecuteCommandRegistrationOptions
  RegistrationOptions Method_WorkspaceApplyEdit = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceDidChangeWorkspaceFolders = Maybe Data.Void.Void
  RegistrationOptions Method_WindowWorkDoneProgressCancel = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceDidCreateFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_WorkspaceDidRenameFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_WorkspaceDidDeleteFiles = Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
  RegistrationOptions Method_NotebookDocumentDidOpen = Maybe Data.Void.Void
  RegistrationOptions Method_NotebookDocumentDidChange = Maybe Data.Void.Void
  RegistrationOptions Method_NotebookDocumentDidSave = Maybe Data.Void.Void
  RegistrationOptions Method_NotebookDocumentDidClose = Maybe Data.Void.Void
  RegistrationOptions Method_Initialized = Maybe Data.Void.Void
  RegistrationOptions Method_Exit = Maybe Data.Void.Void
  RegistrationOptions Method_WorkspaceDidChangeConfiguration = Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions.DidChangeConfigurationRegistrationOptions
  RegistrationOptions Method_WindowShowMessage = Maybe Data.Void.Void
  RegistrationOptions Method_WindowLogMessage = Maybe Data.Void.Void
  RegistrationOptions Method_TelemetryEvent = Maybe Data.Void.Void
  RegistrationOptions Method_TextDocumentDidOpen = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  RegistrationOptions Method_TextDocumentDidChange = Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions.TextDocumentChangeRegistrationOptions
  RegistrationOptions Method_TextDocumentDidClose = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  RegistrationOptions Method_TextDocumentDidSave = Language.LSP.Protocol.Internal.Types.TextDocumentSaveRegistrationOptions.TextDocumentSaveRegistrationOptions
  RegistrationOptions Method_TextDocumentWillSave = Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
  RegistrationOptions Method_WorkspaceDidChangeWatchedFiles = Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions.DidChangeWatchedFilesRegistrationOptions
  RegistrationOptions Method_TextDocumentPublishDiagnostics = Maybe Data.Void.Void
  RegistrationOptions Method_SetTrace = Maybe Data.Void.Void
  RegistrationOptions Method_LogTrace = Maybe Data.Void.Void
  RegistrationOptions Method_CancelRequest = Maybe Data.Void.Void
  RegistrationOptions Method_Progress = Maybe Data.Void.Void
  RegistrationOptions (Method_CustomMethod s) = Data.Void.Void

-- | A singleton type for 'Method'.
type SMethod :: forall f t . Method f t -> Kind.Type
data SMethod m where 
  SMethod_TextDocumentImplementation :: SMethod Method_TextDocumentImplementation
  SMethod_TextDocumentTypeDefinition :: SMethod Method_TextDocumentTypeDefinition
  SMethod_WorkspaceWorkspaceFolders :: SMethod Method_WorkspaceWorkspaceFolders
  SMethod_WorkspaceConfiguration :: SMethod Method_WorkspaceConfiguration
  SMethod_TextDocumentDocumentColor :: SMethod Method_TextDocumentDocumentColor
  SMethod_TextDocumentColorPresentation :: SMethod Method_TextDocumentColorPresentation
  SMethod_TextDocumentFoldingRange :: SMethod Method_TextDocumentFoldingRange
  SMethod_TextDocumentDeclaration :: SMethod Method_TextDocumentDeclaration
  SMethod_TextDocumentSelectionRange :: SMethod Method_TextDocumentSelectionRange
  SMethod_WindowWorkDoneProgressCreate :: SMethod Method_WindowWorkDoneProgressCreate
  SMethod_TextDocumentPrepareCallHierarchy :: SMethod Method_TextDocumentPrepareCallHierarchy
  SMethod_CallHierarchyIncomingCalls :: SMethod Method_CallHierarchyIncomingCalls
  SMethod_CallHierarchyOutgoingCalls :: SMethod Method_CallHierarchyOutgoingCalls
  SMethod_TextDocumentSemanticTokensFull :: SMethod Method_TextDocumentSemanticTokensFull
  SMethod_TextDocumentSemanticTokensFullDelta :: SMethod Method_TextDocumentSemanticTokensFullDelta
  SMethod_TextDocumentSemanticTokensRange :: SMethod Method_TextDocumentSemanticTokensRange
  SMethod_WorkspaceSemanticTokensRefresh :: SMethod Method_WorkspaceSemanticTokensRefresh
  SMethod_WindowShowDocument :: SMethod Method_WindowShowDocument
  SMethod_TextDocumentLinkedEditingRange :: SMethod Method_TextDocumentLinkedEditingRange
  SMethod_WorkspaceWillCreateFiles :: SMethod Method_WorkspaceWillCreateFiles
  SMethod_WorkspaceWillRenameFiles :: SMethod Method_WorkspaceWillRenameFiles
  SMethod_WorkspaceWillDeleteFiles :: SMethod Method_WorkspaceWillDeleteFiles
  SMethod_TextDocumentMoniker :: SMethod Method_TextDocumentMoniker
  SMethod_TextDocumentPrepareTypeHierarchy :: SMethod Method_TextDocumentPrepareTypeHierarchy
  SMethod_TypeHierarchySupertypes :: SMethod Method_TypeHierarchySupertypes
  SMethod_TypeHierarchySubtypes :: SMethod Method_TypeHierarchySubtypes
  SMethod_TextDocumentInlineValue :: SMethod Method_TextDocumentInlineValue
  SMethod_WorkspaceInlineValueRefresh :: SMethod Method_WorkspaceInlineValueRefresh
  SMethod_TextDocumentInlayHint :: SMethod Method_TextDocumentInlayHint
  SMethod_InlayHintResolve :: SMethod Method_InlayHintResolve
  SMethod_WorkspaceInlayHintRefresh :: SMethod Method_WorkspaceInlayHintRefresh
  SMethod_TextDocumentDiagnostic :: SMethod Method_TextDocumentDiagnostic
  SMethod_WorkspaceDiagnostic :: SMethod Method_WorkspaceDiagnostic
  SMethod_WorkspaceDiagnosticRefresh :: SMethod Method_WorkspaceDiagnosticRefresh
  SMethod_ClientRegisterCapability :: SMethod Method_ClientRegisterCapability
  SMethod_ClientUnregisterCapability :: SMethod Method_ClientUnregisterCapability
  SMethod_Initialize :: SMethod Method_Initialize
  SMethod_Shutdown :: SMethod Method_Shutdown
  SMethod_WindowShowMessageRequest :: SMethod Method_WindowShowMessageRequest
  SMethod_TextDocumentWillSaveWaitUntil :: SMethod Method_TextDocumentWillSaveWaitUntil
  SMethod_TextDocumentCompletion :: SMethod Method_TextDocumentCompletion
  SMethod_CompletionItemResolve :: SMethod Method_CompletionItemResolve
  SMethod_TextDocumentHover :: SMethod Method_TextDocumentHover
  SMethod_TextDocumentSignatureHelp :: SMethod Method_TextDocumentSignatureHelp
  SMethod_TextDocumentDefinition :: SMethod Method_TextDocumentDefinition
  SMethod_TextDocumentReferences :: SMethod Method_TextDocumentReferences
  SMethod_TextDocumentDocumentHighlight :: SMethod Method_TextDocumentDocumentHighlight
  SMethod_TextDocumentDocumentSymbol :: SMethod Method_TextDocumentDocumentSymbol
  SMethod_TextDocumentCodeAction :: SMethod Method_TextDocumentCodeAction
  SMethod_CodeActionResolve :: SMethod Method_CodeActionResolve
  SMethod_WorkspaceSymbol :: SMethod Method_WorkspaceSymbol
  SMethod_WorkspaceSymbolResolve :: SMethod Method_WorkspaceSymbolResolve
  SMethod_TextDocumentCodeLens :: SMethod Method_TextDocumentCodeLens
  SMethod_CodeLensResolve :: SMethod Method_CodeLensResolve
  SMethod_WorkspaceCodeLensRefresh :: SMethod Method_WorkspaceCodeLensRefresh
  SMethod_TextDocumentDocumentLink :: SMethod Method_TextDocumentDocumentLink
  SMethod_DocumentLinkResolve :: SMethod Method_DocumentLinkResolve
  SMethod_TextDocumentFormatting :: SMethod Method_TextDocumentFormatting
  SMethod_TextDocumentRangeFormatting :: SMethod Method_TextDocumentRangeFormatting
  SMethod_TextDocumentOnTypeFormatting :: SMethod Method_TextDocumentOnTypeFormatting
  SMethod_TextDocumentRename :: SMethod Method_TextDocumentRename
  SMethod_TextDocumentPrepareRename :: SMethod Method_TextDocumentPrepareRename
  SMethod_WorkspaceExecuteCommand :: SMethod Method_WorkspaceExecuteCommand
  SMethod_WorkspaceApplyEdit :: SMethod Method_WorkspaceApplyEdit
  SMethod_WorkspaceDidChangeWorkspaceFolders :: SMethod Method_WorkspaceDidChangeWorkspaceFolders
  SMethod_WindowWorkDoneProgressCancel :: SMethod Method_WindowWorkDoneProgressCancel
  SMethod_WorkspaceDidCreateFiles :: SMethod Method_WorkspaceDidCreateFiles
  SMethod_WorkspaceDidRenameFiles :: SMethod Method_WorkspaceDidRenameFiles
  SMethod_WorkspaceDidDeleteFiles :: SMethod Method_WorkspaceDidDeleteFiles
  SMethod_NotebookDocumentDidOpen :: SMethod Method_NotebookDocumentDidOpen
  SMethod_NotebookDocumentDidChange :: SMethod Method_NotebookDocumentDidChange
  SMethod_NotebookDocumentDidSave :: SMethod Method_NotebookDocumentDidSave
  SMethod_NotebookDocumentDidClose :: SMethod Method_NotebookDocumentDidClose
  SMethod_Initialized :: SMethod Method_Initialized
  SMethod_Exit :: SMethod Method_Exit
  SMethod_WorkspaceDidChangeConfiguration :: SMethod Method_WorkspaceDidChangeConfiguration
  SMethod_WindowShowMessage :: SMethod Method_WindowShowMessage
  SMethod_WindowLogMessage :: SMethod Method_WindowLogMessage
  SMethod_TelemetryEvent :: SMethod Method_TelemetryEvent
  SMethod_TextDocumentDidOpen :: SMethod Method_TextDocumentDidOpen
  SMethod_TextDocumentDidChange :: SMethod Method_TextDocumentDidChange
  SMethod_TextDocumentDidClose :: SMethod Method_TextDocumentDidClose
  SMethod_TextDocumentDidSave :: SMethod Method_TextDocumentDidSave
  SMethod_TextDocumentWillSave :: SMethod Method_TextDocumentWillSave
  SMethod_WorkspaceDidChangeWatchedFiles :: SMethod Method_WorkspaceDidChangeWatchedFiles
  SMethod_TextDocumentPublishDiagnostics :: SMethod Method_TextDocumentPublishDiagnostics
  SMethod_SetTrace :: SMethod Method_SetTrace
  SMethod_LogTrace :: SMethod Method_LogTrace
  SMethod_CancelRequest :: SMethod Method_CancelRequest
  SMethod_Progress :: SMethod Method_Progress
  SMethod_CustomMethod :: forall s . GHC.TypeLits.KnownSymbol s => Data.Proxy.Proxy s -> SMethod (Method_CustomMethod s)

-- | A method which isn't statically known.
data SomeMethod where 
  SomeMethod :: forall m . SMethod m -> SomeMethod

-- | Turn a 'SomeMethod' into its LSP method string.
someMethodToMethodString :: SomeMethod -> String
someMethodToMethodString (SomeMethod SMethod_TextDocumentImplementation) = "textDocument/implementation"
someMethodToMethodString (SomeMethod SMethod_TextDocumentTypeDefinition) = "textDocument/typeDefinition"
someMethodToMethodString (SomeMethod SMethod_WorkspaceWorkspaceFolders) = "workspace/workspaceFolders"
someMethodToMethodString (SomeMethod SMethod_WorkspaceConfiguration) = "workspace/configuration"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDocumentColor) = "textDocument/documentColor"
someMethodToMethodString (SomeMethod SMethod_TextDocumentColorPresentation) = "textDocument/colorPresentation"
someMethodToMethodString (SomeMethod SMethod_TextDocumentFoldingRange) = "textDocument/foldingRange"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDeclaration) = "textDocument/declaration"
someMethodToMethodString (SomeMethod SMethod_TextDocumentSelectionRange) = "textDocument/selectionRange"
someMethodToMethodString (SomeMethod SMethod_WindowWorkDoneProgressCreate) = "window/workDoneProgress/create"
someMethodToMethodString (SomeMethod SMethod_TextDocumentPrepareCallHierarchy) = "textDocument/prepareCallHierarchy"
someMethodToMethodString (SomeMethod SMethod_CallHierarchyIncomingCalls) = "callHierarchy/incomingCalls"
someMethodToMethodString (SomeMethod SMethod_CallHierarchyOutgoingCalls) = "callHierarchy/outgoingCalls"
someMethodToMethodString (SomeMethod SMethod_TextDocumentSemanticTokensFull) = "textDocument/semanticTokens/full"
someMethodToMethodString (SomeMethod SMethod_TextDocumentSemanticTokensFullDelta) = "textDocument/semanticTokens/full/delta"
someMethodToMethodString (SomeMethod SMethod_TextDocumentSemanticTokensRange) = "textDocument/semanticTokens/range"
someMethodToMethodString (SomeMethod SMethod_WorkspaceSemanticTokensRefresh) = "workspace/semanticTokens/refresh"
someMethodToMethodString (SomeMethod SMethod_WindowShowDocument) = "window/showDocument"
someMethodToMethodString (SomeMethod SMethod_TextDocumentLinkedEditingRange) = "textDocument/linkedEditingRange"
someMethodToMethodString (SomeMethod SMethod_WorkspaceWillCreateFiles) = "workspace/willCreateFiles"
someMethodToMethodString (SomeMethod SMethod_WorkspaceWillRenameFiles) = "workspace/willRenameFiles"
someMethodToMethodString (SomeMethod SMethod_WorkspaceWillDeleteFiles) = "workspace/willDeleteFiles"
someMethodToMethodString (SomeMethod SMethod_TextDocumentMoniker) = "textDocument/moniker"
someMethodToMethodString (SomeMethod SMethod_TextDocumentPrepareTypeHierarchy) = "textDocument/prepareTypeHierarchy"
someMethodToMethodString (SomeMethod SMethod_TypeHierarchySupertypes) = "typeHierarchy/supertypes"
someMethodToMethodString (SomeMethod SMethod_TypeHierarchySubtypes) = "typeHierarchy/subtypes"
someMethodToMethodString (SomeMethod SMethod_TextDocumentInlineValue) = "textDocument/inlineValue"
someMethodToMethodString (SomeMethod SMethod_WorkspaceInlineValueRefresh) = "workspace/inlineValue/refresh"
someMethodToMethodString (SomeMethod SMethod_TextDocumentInlayHint) = "textDocument/inlayHint"
someMethodToMethodString (SomeMethod SMethod_InlayHintResolve) = "inlayHint/resolve"
someMethodToMethodString (SomeMethod SMethod_WorkspaceInlayHintRefresh) = "workspace/inlayHint/refresh"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDiagnostic) = "textDocument/diagnostic"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDiagnostic) = "workspace/diagnostic"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDiagnosticRefresh) = "workspace/diagnostic/refresh"
someMethodToMethodString (SomeMethod SMethod_ClientRegisterCapability) = "client/registerCapability"
someMethodToMethodString (SomeMethod SMethod_ClientUnregisterCapability) = "client/unregisterCapability"
someMethodToMethodString (SomeMethod SMethod_Initialize) = "initialize"
someMethodToMethodString (SomeMethod SMethod_Shutdown) = "shutdown"
someMethodToMethodString (SomeMethod SMethod_WindowShowMessageRequest) = "window/showMessageRequest"
someMethodToMethodString (SomeMethod SMethod_TextDocumentWillSaveWaitUntil) = "textDocument/willSaveWaitUntil"
someMethodToMethodString (SomeMethod SMethod_TextDocumentCompletion) = "textDocument/completion"
someMethodToMethodString (SomeMethod SMethod_CompletionItemResolve) = "completionItem/resolve"
someMethodToMethodString (SomeMethod SMethod_TextDocumentHover) = "textDocument/hover"
someMethodToMethodString (SomeMethod SMethod_TextDocumentSignatureHelp) = "textDocument/signatureHelp"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDefinition) = "textDocument/definition"
someMethodToMethodString (SomeMethod SMethod_TextDocumentReferences) = "textDocument/references"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDocumentHighlight) = "textDocument/documentHighlight"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDocumentSymbol) = "textDocument/documentSymbol"
someMethodToMethodString (SomeMethod SMethod_TextDocumentCodeAction) = "textDocument/codeAction"
someMethodToMethodString (SomeMethod SMethod_CodeActionResolve) = "codeAction/resolve"
someMethodToMethodString (SomeMethod SMethod_WorkspaceSymbol) = "workspace/symbol"
someMethodToMethodString (SomeMethod SMethod_WorkspaceSymbolResolve) = "workspaceSymbol/resolve"
someMethodToMethodString (SomeMethod SMethod_TextDocumentCodeLens) = "textDocument/codeLens"
someMethodToMethodString (SomeMethod SMethod_CodeLensResolve) = "codeLens/resolve"
someMethodToMethodString (SomeMethod SMethod_WorkspaceCodeLensRefresh) = "workspace/codeLens/refresh"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDocumentLink) = "textDocument/documentLink"
someMethodToMethodString (SomeMethod SMethod_DocumentLinkResolve) = "documentLink/resolve"
someMethodToMethodString (SomeMethod SMethod_TextDocumentFormatting) = "textDocument/formatting"
someMethodToMethodString (SomeMethod SMethod_TextDocumentRangeFormatting) = "textDocument/rangeFormatting"
someMethodToMethodString (SomeMethod SMethod_TextDocumentOnTypeFormatting) = "textDocument/onTypeFormatting"
someMethodToMethodString (SomeMethod SMethod_TextDocumentRename) = "textDocument/rename"
someMethodToMethodString (SomeMethod SMethod_TextDocumentPrepareRename) = "textDocument/prepareRename"
someMethodToMethodString (SomeMethod SMethod_WorkspaceExecuteCommand) = "workspace/executeCommand"
someMethodToMethodString (SomeMethod SMethod_WorkspaceApplyEdit) = "workspace/applyEdit"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidChangeWorkspaceFolders) = "workspace/didChangeWorkspaceFolders"
someMethodToMethodString (SomeMethod SMethod_WindowWorkDoneProgressCancel) = "window/workDoneProgress/cancel"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidCreateFiles) = "workspace/didCreateFiles"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidRenameFiles) = "workspace/didRenameFiles"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidDeleteFiles) = "workspace/didDeleteFiles"
someMethodToMethodString (SomeMethod SMethod_NotebookDocumentDidOpen) = "notebookDocument/didOpen"
someMethodToMethodString (SomeMethod SMethod_NotebookDocumentDidChange) = "notebookDocument/didChange"
someMethodToMethodString (SomeMethod SMethod_NotebookDocumentDidSave) = "notebookDocument/didSave"
someMethodToMethodString (SomeMethod SMethod_NotebookDocumentDidClose) = "notebookDocument/didClose"
someMethodToMethodString (SomeMethod SMethod_Initialized) = "initialized"
someMethodToMethodString (SomeMethod SMethod_Exit) = "exit"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidChangeConfiguration) = "workspace/didChangeConfiguration"
someMethodToMethodString (SomeMethod SMethod_WindowShowMessage) = "window/showMessage"
someMethodToMethodString (SomeMethod SMethod_WindowLogMessage) = "window/logMessage"
someMethodToMethodString (SomeMethod SMethod_TelemetryEvent) = "telemetry/event"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDidOpen) = "textDocument/didOpen"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDidChange) = "textDocument/didChange"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDidClose) = "textDocument/didClose"
someMethodToMethodString (SomeMethod SMethod_TextDocumentDidSave) = "textDocument/didSave"
someMethodToMethodString (SomeMethod SMethod_TextDocumentWillSave) = "textDocument/willSave"
someMethodToMethodString (SomeMethod SMethod_WorkspaceDidChangeWatchedFiles) = "workspace/didChangeWatchedFiles"
someMethodToMethodString (SomeMethod SMethod_TextDocumentPublishDiagnostics) = "textDocument/publishDiagnostics"
someMethodToMethodString (SomeMethod SMethod_SetTrace) = "$/setTrace"
someMethodToMethodString (SomeMethod SMethod_LogTrace) = "$/logTrace"
someMethodToMethodString (SomeMethod SMethod_CancelRequest) = "$/cancelRequest"
someMethodToMethodString (SomeMethod SMethod_Progress) = "$/progress"
someMethodToMethodString (SomeMethod (SMethod_CustomMethod v)) = GHC.TypeLits.symbolVal v

-- | Turn a LSP method string into a 'SomeMethod'.
methodStringToSomeMethod :: String -> SomeMethod
methodStringToSomeMethod "textDocument/implementation" = SomeMethod SMethod_TextDocumentImplementation
methodStringToSomeMethod "textDocument/typeDefinition" = SomeMethod SMethod_TextDocumentTypeDefinition
methodStringToSomeMethod "workspace/workspaceFolders" = SomeMethod SMethod_WorkspaceWorkspaceFolders
methodStringToSomeMethod "workspace/configuration" = SomeMethod SMethod_WorkspaceConfiguration
methodStringToSomeMethod "textDocument/documentColor" = SomeMethod SMethod_TextDocumentDocumentColor
methodStringToSomeMethod "textDocument/colorPresentation" = SomeMethod SMethod_TextDocumentColorPresentation
methodStringToSomeMethod "textDocument/foldingRange" = SomeMethod SMethod_TextDocumentFoldingRange
methodStringToSomeMethod "textDocument/declaration" = SomeMethod SMethod_TextDocumentDeclaration
methodStringToSomeMethod "textDocument/selectionRange" = SomeMethod SMethod_TextDocumentSelectionRange
methodStringToSomeMethod "window/workDoneProgress/create" = SomeMethod SMethod_WindowWorkDoneProgressCreate
methodStringToSomeMethod "textDocument/prepareCallHierarchy" = SomeMethod SMethod_TextDocumentPrepareCallHierarchy
methodStringToSomeMethod "callHierarchy/incomingCalls" = SomeMethod SMethod_CallHierarchyIncomingCalls
methodStringToSomeMethod "callHierarchy/outgoingCalls" = SomeMethod SMethod_CallHierarchyOutgoingCalls
methodStringToSomeMethod "textDocument/semanticTokens/full" = SomeMethod SMethod_TextDocumentSemanticTokensFull
methodStringToSomeMethod "textDocument/semanticTokens/full/delta" = SomeMethod SMethod_TextDocumentSemanticTokensFullDelta
methodStringToSomeMethod "textDocument/semanticTokens/range" = SomeMethod SMethod_TextDocumentSemanticTokensRange
methodStringToSomeMethod "workspace/semanticTokens/refresh" = SomeMethod SMethod_WorkspaceSemanticTokensRefresh
methodStringToSomeMethod "window/showDocument" = SomeMethod SMethod_WindowShowDocument
methodStringToSomeMethod "textDocument/linkedEditingRange" = SomeMethod SMethod_TextDocumentLinkedEditingRange
methodStringToSomeMethod "workspace/willCreateFiles" = SomeMethod SMethod_WorkspaceWillCreateFiles
methodStringToSomeMethod "workspace/willRenameFiles" = SomeMethod SMethod_WorkspaceWillRenameFiles
methodStringToSomeMethod "workspace/willDeleteFiles" = SomeMethod SMethod_WorkspaceWillDeleteFiles
methodStringToSomeMethod "textDocument/moniker" = SomeMethod SMethod_TextDocumentMoniker
methodStringToSomeMethod "textDocument/prepareTypeHierarchy" = SomeMethod SMethod_TextDocumentPrepareTypeHierarchy
methodStringToSomeMethod "typeHierarchy/supertypes" = SomeMethod SMethod_TypeHierarchySupertypes
methodStringToSomeMethod "typeHierarchy/subtypes" = SomeMethod SMethod_TypeHierarchySubtypes
methodStringToSomeMethod "textDocument/inlineValue" = SomeMethod SMethod_TextDocumentInlineValue
methodStringToSomeMethod "workspace/inlineValue/refresh" = SomeMethod SMethod_WorkspaceInlineValueRefresh
methodStringToSomeMethod "textDocument/inlayHint" = SomeMethod SMethod_TextDocumentInlayHint
methodStringToSomeMethod "inlayHint/resolve" = SomeMethod SMethod_InlayHintResolve
methodStringToSomeMethod "workspace/inlayHint/refresh" = SomeMethod SMethod_WorkspaceInlayHintRefresh
methodStringToSomeMethod "textDocument/diagnostic" = SomeMethod SMethod_TextDocumentDiagnostic
methodStringToSomeMethod "workspace/diagnostic" = SomeMethod SMethod_WorkspaceDiagnostic
methodStringToSomeMethod "workspace/diagnostic/refresh" = SomeMethod SMethod_WorkspaceDiagnosticRefresh
methodStringToSomeMethod "client/registerCapability" = SomeMethod SMethod_ClientRegisterCapability
methodStringToSomeMethod "client/unregisterCapability" = SomeMethod SMethod_ClientUnregisterCapability
methodStringToSomeMethod "initialize" = SomeMethod SMethod_Initialize
methodStringToSomeMethod "shutdown" = SomeMethod SMethod_Shutdown
methodStringToSomeMethod "window/showMessageRequest" = SomeMethod SMethod_WindowShowMessageRequest
methodStringToSomeMethod "textDocument/willSaveWaitUntil" = SomeMethod SMethod_TextDocumentWillSaveWaitUntil
methodStringToSomeMethod "textDocument/completion" = SomeMethod SMethod_TextDocumentCompletion
methodStringToSomeMethod "completionItem/resolve" = SomeMethod SMethod_CompletionItemResolve
methodStringToSomeMethod "textDocument/hover" = SomeMethod SMethod_TextDocumentHover
methodStringToSomeMethod "textDocument/signatureHelp" = SomeMethod SMethod_TextDocumentSignatureHelp
methodStringToSomeMethod "textDocument/definition" = SomeMethod SMethod_TextDocumentDefinition
methodStringToSomeMethod "textDocument/references" = SomeMethod SMethod_TextDocumentReferences
methodStringToSomeMethod "textDocument/documentHighlight" = SomeMethod SMethod_TextDocumentDocumentHighlight
methodStringToSomeMethod "textDocument/documentSymbol" = SomeMethod SMethod_TextDocumentDocumentSymbol
methodStringToSomeMethod "textDocument/codeAction" = SomeMethod SMethod_TextDocumentCodeAction
methodStringToSomeMethod "codeAction/resolve" = SomeMethod SMethod_CodeActionResolve
methodStringToSomeMethod "workspace/symbol" = SomeMethod SMethod_WorkspaceSymbol
methodStringToSomeMethod "workspaceSymbol/resolve" = SomeMethod SMethod_WorkspaceSymbolResolve
methodStringToSomeMethod "textDocument/codeLens" = SomeMethod SMethod_TextDocumentCodeLens
methodStringToSomeMethod "codeLens/resolve" = SomeMethod SMethod_CodeLensResolve
methodStringToSomeMethod "workspace/codeLens/refresh" = SomeMethod SMethod_WorkspaceCodeLensRefresh
methodStringToSomeMethod "textDocument/documentLink" = SomeMethod SMethod_TextDocumentDocumentLink
methodStringToSomeMethod "documentLink/resolve" = SomeMethod SMethod_DocumentLinkResolve
methodStringToSomeMethod "textDocument/formatting" = SomeMethod SMethod_TextDocumentFormatting
methodStringToSomeMethod "textDocument/rangeFormatting" = SomeMethod SMethod_TextDocumentRangeFormatting
methodStringToSomeMethod "textDocument/onTypeFormatting" = SomeMethod SMethod_TextDocumentOnTypeFormatting
methodStringToSomeMethod "textDocument/rename" = SomeMethod SMethod_TextDocumentRename
methodStringToSomeMethod "textDocument/prepareRename" = SomeMethod SMethod_TextDocumentPrepareRename
methodStringToSomeMethod "workspace/executeCommand" = SomeMethod SMethod_WorkspaceExecuteCommand
methodStringToSomeMethod "workspace/applyEdit" = SomeMethod SMethod_WorkspaceApplyEdit
methodStringToSomeMethod "workspace/didChangeWorkspaceFolders" = SomeMethod SMethod_WorkspaceDidChangeWorkspaceFolders
methodStringToSomeMethod "window/workDoneProgress/cancel" = SomeMethod SMethod_WindowWorkDoneProgressCancel
methodStringToSomeMethod "workspace/didCreateFiles" = SomeMethod SMethod_WorkspaceDidCreateFiles
methodStringToSomeMethod "workspace/didRenameFiles" = SomeMethod SMethod_WorkspaceDidRenameFiles
methodStringToSomeMethod "workspace/didDeleteFiles" = SomeMethod SMethod_WorkspaceDidDeleteFiles
methodStringToSomeMethod "notebookDocument/didOpen" = SomeMethod SMethod_NotebookDocumentDidOpen
methodStringToSomeMethod "notebookDocument/didChange" = SomeMethod SMethod_NotebookDocumentDidChange
methodStringToSomeMethod "notebookDocument/didSave" = SomeMethod SMethod_NotebookDocumentDidSave
methodStringToSomeMethod "notebookDocument/didClose" = SomeMethod SMethod_NotebookDocumentDidClose
methodStringToSomeMethod "initialized" = SomeMethod SMethod_Initialized
methodStringToSomeMethod "exit" = SomeMethod SMethod_Exit
methodStringToSomeMethod "workspace/didChangeConfiguration" = SomeMethod SMethod_WorkspaceDidChangeConfiguration
methodStringToSomeMethod "window/showMessage" = SomeMethod SMethod_WindowShowMessage
methodStringToSomeMethod "window/logMessage" = SomeMethod SMethod_WindowLogMessage
methodStringToSomeMethod "telemetry/event" = SomeMethod SMethod_TelemetryEvent
methodStringToSomeMethod "textDocument/didOpen" = SomeMethod SMethod_TextDocumentDidOpen
methodStringToSomeMethod "textDocument/didChange" = SomeMethod SMethod_TextDocumentDidChange
methodStringToSomeMethod "textDocument/didClose" = SomeMethod SMethod_TextDocumentDidClose
methodStringToSomeMethod "textDocument/didSave" = SomeMethod SMethod_TextDocumentDidSave
methodStringToSomeMethod "textDocument/willSave" = SomeMethod SMethod_TextDocumentWillSave
methodStringToSomeMethod "workspace/didChangeWatchedFiles" = SomeMethod SMethod_WorkspaceDidChangeWatchedFiles
methodStringToSomeMethod "textDocument/publishDiagnostics" = SomeMethod SMethod_TextDocumentPublishDiagnostics
methodStringToSomeMethod "$/setTrace" = SomeMethod SMethod_SetTrace
methodStringToSomeMethod "$/logTrace" = SomeMethod SMethod_LogTrace
methodStringToSomeMethod "$/cancelRequest" = SomeMethod SMethod_CancelRequest
methodStringToSomeMethod "$/progress" = SomeMethod SMethod_Progress
methodStringToSomeMethod v = case GHC.TypeLits.someSymbolVal v of { GHC.TypeLits.SomeSymbol p -> SomeMethod (SMethod_CustomMethod p) ; }

-- | Get a singleton witness for the message direction of a 'SMethod'.
messageDirection :: forall f t (m :: Method f t) . SMethod m -> MM.SMessageDirection f
messageDirection SMethod_TextDocumentImplementation = MM.SClientToServer
messageDirection SMethod_TextDocumentTypeDefinition = MM.SClientToServer
messageDirection SMethod_WorkspaceWorkspaceFolders = MM.SServerToClient
messageDirection SMethod_WorkspaceConfiguration = MM.SServerToClient
messageDirection SMethod_TextDocumentDocumentColor = MM.SClientToServer
messageDirection SMethod_TextDocumentColorPresentation = MM.SClientToServer
messageDirection SMethod_TextDocumentFoldingRange = MM.SClientToServer
messageDirection SMethod_TextDocumentDeclaration = MM.SClientToServer
messageDirection SMethod_TextDocumentSelectionRange = MM.SClientToServer
messageDirection SMethod_WindowWorkDoneProgressCreate = MM.SServerToClient
messageDirection SMethod_TextDocumentPrepareCallHierarchy = MM.SClientToServer
messageDirection SMethod_CallHierarchyIncomingCalls = MM.SClientToServer
messageDirection SMethod_CallHierarchyOutgoingCalls = MM.SClientToServer
messageDirection SMethod_TextDocumentSemanticTokensFull = MM.SClientToServer
messageDirection SMethod_TextDocumentSemanticTokensFullDelta = MM.SClientToServer
messageDirection SMethod_TextDocumentSemanticTokensRange = MM.SClientToServer
messageDirection SMethod_WorkspaceSemanticTokensRefresh = MM.SServerToClient
messageDirection SMethod_WindowShowDocument = MM.SServerToClient
messageDirection SMethod_TextDocumentLinkedEditingRange = MM.SClientToServer
messageDirection SMethod_WorkspaceWillCreateFiles = MM.SClientToServer
messageDirection SMethod_WorkspaceWillRenameFiles = MM.SClientToServer
messageDirection SMethod_WorkspaceWillDeleteFiles = MM.SClientToServer
messageDirection SMethod_TextDocumentMoniker = MM.SClientToServer
messageDirection SMethod_TextDocumentPrepareTypeHierarchy = MM.SClientToServer
messageDirection SMethod_TypeHierarchySupertypes = MM.SClientToServer
messageDirection SMethod_TypeHierarchySubtypes = MM.SClientToServer
messageDirection SMethod_TextDocumentInlineValue = MM.SClientToServer
messageDirection SMethod_WorkspaceInlineValueRefresh = MM.SServerToClient
messageDirection SMethod_TextDocumentInlayHint = MM.SClientToServer
messageDirection SMethod_InlayHintResolve = MM.SClientToServer
messageDirection SMethod_WorkspaceInlayHintRefresh = MM.SServerToClient
messageDirection SMethod_TextDocumentDiagnostic = MM.SClientToServer
messageDirection SMethod_WorkspaceDiagnostic = MM.SClientToServer
messageDirection SMethod_WorkspaceDiagnosticRefresh = MM.SServerToClient
messageDirection SMethod_ClientRegisterCapability = MM.SServerToClient
messageDirection SMethod_ClientUnregisterCapability = MM.SServerToClient
messageDirection SMethod_Initialize = MM.SClientToServer
messageDirection SMethod_Shutdown = MM.SClientToServer
messageDirection SMethod_WindowShowMessageRequest = MM.SServerToClient
messageDirection SMethod_TextDocumentWillSaveWaitUntil = MM.SClientToServer
messageDirection SMethod_TextDocumentCompletion = MM.SClientToServer
messageDirection SMethod_CompletionItemResolve = MM.SClientToServer
messageDirection SMethod_TextDocumentHover = MM.SClientToServer
messageDirection SMethod_TextDocumentSignatureHelp = MM.SClientToServer
messageDirection SMethod_TextDocumentDefinition = MM.SClientToServer
messageDirection SMethod_TextDocumentReferences = MM.SClientToServer
messageDirection SMethod_TextDocumentDocumentHighlight = MM.SClientToServer
messageDirection SMethod_TextDocumentDocumentSymbol = MM.SClientToServer
messageDirection SMethod_TextDocumentCodeAction = MM.SClientToServer
messageDirection SMethod_CodeActionResolve = MM.SClientToServer
messageDirection SMethod_WorkspaceSymbol = MM.SClientToServer
messageDirection SMethod_WorkspaceSymbolResolve = MM.SClientToServer
messageDirection SMethod_TextDocumentCodeLens = MM.SClientToServer
messageDirection SMethod_CodeLensResolve = MM.SClientToServer
messageDirection SMethod_WorkspaceCodeLensRefresh = MM.SServerToClient
messageDirection SMethod_TextDocumentDocumentLink = MM.SClientToServer
messageDirection SMethod_DocumentLinkResolve = MM.SClientToServer
messageDirection SMethod_TextDocumentFormatting = MM.SClientToServer
messageDirection SMethod_TextDocumentRangeFormatting = MM.SClientToServer
messageDirection SMethod_TextDocumentOnTypeFormatting = MM.SClientToServer
messageDirection SMethod_TextDocumentRename = MM.SClientToServer
messageDirection SMethod_TextDocumentPrepareRename = MM.SClientToServer
messageDirection SMethod_WorkspaceExecuteCommand = MM.SClientToServer
messageDirection SMethod_WorkspaceApplyEdit = MM.SServerToClient
messageDirection SMethod_WorkspaceDidChangeWorkspaceFolders = MM.SClientToServer
messageDirection SMethod_WindowWorkDoneProgressCancel = MM.SClientToServer
messageDirection SMethod_WorkspaceDidCreateFiles = MM.SClientToServer
messageDirection SMethod_WorkspaceDidRenameFiles = MM.SClientToServer
messageDirection SMethod_WorkspaceDidDeleteFiles = MM.SClientToServer
messageDirection SMethod_NotebookDocumentDidOpen = MM.SClientToServer
messageDirection SMethod_NotebookDocumentDidChange = MM.SClientToServer
messageDirection SMethod_NotebookDocumentDidSave = MM.SClientToServer
messageDirection SMethod_NotebookDocumentDidClose = MM.SClientToServer
messageDirection SMethod_Initialized = MM.SClientToServer
messageDirection SMethod_Exit = MM.SClientToServer
messageDirection SMethod_WorkspaceDidChangeConfiguration = MM.SClientToServer
messageDirection SMethod_WindowShowMessage = MM.SServerToClient
messageDirection SMethod_WindowLogMessage = MM.SServerToClient
messageDirection SMethod_TelemetryEvent = MM.SServerToClient
messageDirection SMethod_TextDocumentDidOpen = MM.SClientToServer
messageDirection SMethod_TextDocumentDidChange = MM.SClientToServer
messageDirection SMethod_TextDocumentDidClose = MM.SClientToServer
messageDirection SMethod_TextDocumentDidSave = MM.SClientToServer
messageDirection SMethod_TextDocumentWillSave = MM.SClientToServer
messageDirection SMethod_WorkspaceDidChangeWatchedFiles = MM.SClientToServer
messageDirection SMethod_TextDocumentPublishDiagnostics = MM.SServerToClient
messageDirection SMethod_SetTrace = MM.SClientToServer
messageDirection SMethod_LogTrace = MM.SServerToClient
messageDirection SMethod_CancelRequest = MM.SBothDirections
messageDirection SMethod_Progress = MM.SBothDirections
messageDirection (SMethod_CustomMethod _) = MM.SBothDirections

-- | Get a singleton witness for the message kind of a 'SMethod'.
messageKind :: forall f t (m :: Method f t) . SMethod m -> MM.SMessageKind t
messageKind SMethod_TextDocumentImplementation = MM.SRequest
messageKind SMethod_TextDocumentTypeDefinition = MM.SRequest
messageKind SMethod_WorkspaceWorkspaceFolders = MM.SRequest
messageKind SMethod_WorkspaceConfiguration = MM.SRequest
messageKind SMethod_TextDocumentDocumentColor = MM.SRequest
messageKind SMethod_TextDocumentColorPresentation = MM.SRequest
messageKind SMethod_TextDocumentFoldingRange = MM.SRequest
messageKind SMethod_TextDocumentDeclaration = MM.SRequest
messageKind SMethod_TextDocumentSelectionRange = MM.SRequest
messageKind SMethod_WindowWorkDoneProgressCreate = MM.SRequest
messageKind SMethod_TextDocumentPrepareCallHierarchy = MM.SRequest
messageKind SMethod_CallHierarchyIncomingCalls = MM.SRequest
messageKind SMethod_CallHierarchyOutgoingCalls = MM.SRequest
messageKind SMethod_TextDocumentSemanticTokensFull = MM.SRequest
messageKind SMethod_TextDocumentSemanticTokensFullDelta = MM.SRequest
messageKind SMethod_TextDocumentSemanticTokensRange = MM.SRequest
messageKind SMethod_WorkspaceSemanticTokensRefresh = MM.SRequest
messageKind SMethod_WindowShowDocument = MM.SRequest
messageKind SMethod_TextDocumentLinkedEditingRange = MM.SRequest
messageKind SMethod_WorkspaceWillCreateFiles = MM.SRequest
messageKind SMethod_WorkspaceWillRenameFiles = MM.SRequest
messageKind SMethod_WorkspaceWillDeleteFiles = MM.SRequest
messageKind SMethod_TextDocumentMoniker = MM.SRequest
messageKind SMethod_TextDocumentPrepareTypeHierarchy = MM.SRequest
messageKind SMethod_TypeHierarchySupertypes = MM.SRequest
messageKind SMethod_TypeHierarchySubtypes = MM.SRequest
messageKind SMethod_TextDocumentInlineValue = MM.SRequest
messageKind SMethod_WorkspaceInlineValueRefresh = MM.SRequest
messageKind SMethod_TextDocumentInlayHint = MM.SRequest
messageKind SMethod_InlayHintResolve = MM.SRequest
messageKind SMethod_WorkspaceInlayHintRefresh = MM.SRequest
messageKind SMethod_TextDocumentDiagnostic = MM.SRequest
messageKind SMethod_WorkspaceDiagnostic = MM.SRequest
messageKind SMethod_WorkspaceDiagnosticRefresh = MM.SRequest
messageKind SMethod_ClientRegisterCapability = MM.SRequest
messageKind SMethod_ClientUnregisterCapability = MM.SRequest
messageKind SMethod_Initialize = MM.SRequest
messageKind SMethod_Shutdown = MM.SRequest
messageKind SMethod_WindowShowMessageRequest = MM.SRequest
messageKind SMethod_TextDocumentWillSaveWaitUntil = MM.SRequest
messageKind SMethod_TextDocumentCompletion = MM.SRequest
messageKind SMethod_CompletionItemResolve = MM.SRequest
messageKind SMethod_TextDocumentHover = MM.SRequest
messageKind SMethod_TextDocumentSignatureHelp = MM.SRequest
messageKind SMethod_TextDocumentDefinition = MM.SRequest
messageKind SMethod_TextDocumentReferences = MM.SRequest
messageKind SMethod_TextDocumentDocumentHighlight = MM.SRequest
messageKind SMethod_TextDocumentDocumentSymbol = MM.SRequest
messageKind SMethod_TextDocumentCodeAction = MM.SRequest
messageKind SMethod_CodeActionResolve = MM.SRequest
messageKind SMethod_WorkspaceSymbol = MM.SRequest
messageKind SMethod_WorkspaceSymbolResolve = MM.SRequest
messageKind SMethod_TextDocumentCodeLens = MM.SRequest
messageKind SMethod_CodeLensResolve = MM.SRequest
messageKind SMethod_WorkspaceCodeLensRefresh = MM.SRequest
messageKind SMethod_TextDocumentDocumentLink = MM.SRequest
messageKind SMethod_DocumentLinkResolve = MM.SRequest
messageKind SMethod_TextDocumentFormatting = MM.SRequest
messageKind SMethod_TextDocumentRangeFormatting = MM.SRequest
messageKind SMethod_TextDocumentOnTypeFormatting = MM.SRequest
messageKind SMethod_TextDocumentRename = MM.SRequest
messageKind SMethod_TextDocumentPrepareRename = MM.SRequest
messageKind SMethod_WorkspaceExecuteCommand = MM.SRequest
messageKind SMethod_WorkspaceApplyEdit = MM.SRequest
messageKind SMethod_WorkspaceDidChangeWorkspaceFolders = MM.SNotification
messageKind SMethod_WindowWorkDoneProgressCancel = MM.SNotification
messageKind SMethod_WorkspaceDidCreateFiles = MM.SNotification
messageKind SMethod_WorkspaceDidRenameFiles = MM.SNotification
messageKind SMethod_WorkspaceDidDeleteFiles = MM.SNotification
messageKind SMethod_NotebookDocumentDidOpen = MM.SNotification
messageKind SMethod_NotebookDocumentDidChange = MM.SNotification
messageKind SMethod_NotebookDocumentDidSave = MM.SNotification
messageKind SMethod_NotebookDocumentDidClose = MM.SNotification
messageKind SMethod_Initialized = MM.SNotification
messageKind SMethod_Exit = MM.SNotification
messageKind SMethod_WorkspaceDidChangeConfiguration = MM.SNotification
messageKind SMethod_WindowShowMessage = MM.SNotification
messageKind SMethod_WindowLogMessage = MM.SNotification
messageKind SMethod_TelemetryEvent = MM.SNotification
messageKind SMethod_TextDocumentDidOpen = MM.SNotification
messageKind SMethod_TextDocumentDidChange = MM.SNotification
messageKind SMethod_TextDocumentDidClose = MM.SNotification
messageKind SMethod_TextDocumentDidSave = MM.SNotification
messageKind SMethod_TextDocumentWillSave = MM.SNotification
messageKind SMethod_WorkspaceDidChangeWatchedFiles = MM.SNotification
messageKind SMethod_TextDocumentPublishDiagnostics = MM.SNotification
messageKind SMethod_SetTrace = MM.SNotification
messageKind SMethod_LogTrace = MM.SNotification
messageKind SMethod_CancelRequest = MM.SNotification
messageKind SMethod_Progress = MM.SNotification
messageKind (SMethod_CustomMethod _) = MM.SBothTypes
