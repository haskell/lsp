{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Method where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Kind as Kind
import qualified Data.Row as Row
import qualified Data.Singletons as S
import qualified Data.Void
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
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions
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
data Method where 
  Method_TextDocumentImplementation :: Method
  Method_TextDocumentTypeDefinition :: Method
  Method_WorkspaceWorkspaceFolders :: Method
  Method_WorkspaceConfiguration :: Method
  Method_TextDocumentDocumentColor :: Method
  Method_TextDocumentColorPresentation :: Method
  Method_TextDocumentFoldingRange :: Method
  Method_TextDocumentDeclaration :: Method
  Method_TextDocumentSelectionRange :: Method
  Method_WindowWorkDoneProgressCreate :: Method
  Method_TextDocumentPrepareCallHierarchy :: Method
  Method_CallHierarchyIncomingCalls :: Method
  Method_CallHierarchyOutgoingCalls :: Method
  Method_TextDocumentSemanticTokensFull :: Method
  Method_TextDocumentSemanticTokensFullDelta :: Method
  Method_TextDocumentSemanticTokensRange :: Method
  Method_WorkspaceSemanticTokensRefresh :: Method
  Method_WindowShowDocument :: Method
  Method_TextDocumentLinkedEditingRange :: Method
  Method_WorkspaceWillCreateFiles :: Method
  Method_WorkspaceWillRenameFiles :: Method
  Method_WorkspaceWillDeleteFiles :: Method
  Method_TextDocumentMoniker :: Method
  Method_TextDocumentPrepareTypeHierarchy :: Method
  Method_TypeHierarchySupertypes :: Method
  Method_TypeHierarchySubtypes :: Method
  Method_TextDocumentInlineValue :: Method
  Method_WorkspaceInlineValueRefresh :: Method
  Method_TextDocumentInlayHint :: Method
  Method_InlayHintResolve :: Method
  Method_WorkspaceInlayHintRefresh :: Method
  Method_TextDocumentDiagnostic :: Method
  Method_WorkspaceDiagnostic :: Method
  Method_WorkspaceDiagnosticRefresh :: Method
  Method_ClientRegisterCapability :: Method
  Method_ClientUnregisterCapability :: Method
  Method_Initialize :: Method
  Method_Shutdown :: Method
  Method_WindowShowMessageRequest :: Method
  Method_TextDocumentWillSaveWaitUntil :: Method
  Method_TextDocumentCompletion :: Method
  Method_CompletionItemResolve :: Method
  Method_TextDocumentHover :: Method
  Method_TextDocumentSignatureHelp :: Method
  Method_TextDocumentDefinition :: Method
  Method_TextDocumentReferences :: Method
  Method_TextDocumentDocumentHighlight :: Method
  Method_TextDocumentDocumentSymbol :: Method
  Method_TextDocumentCodeAction :: Method
  Method_CodeActionResolve :: Method
  Method_WorkspaceSymbol :: Method
  Method_WorkspaceSymbolResolve :: Method
  Method_TextDocumentCodeLens :: Method
  Method_CodeLensResolve :: Method
  Method_WorkspaceCodeLensRefresh :: Method
  Method_TextDocumentDocumentLink :: Method
  Method_DocumentLinkResolve :: Method
  Method_TextDocumentFormatting :: Method
  Method_TextDocumentRangeFormatting :: Method
  Method_TextDocumentOnTypeFormatting :: Method
  Method_TextDocumentRename :: Method
  Method_TextDocumentPrepareRename :: Method
  Method_WorkspaceExecuteCommand :: Method
  Method_WorkspaceApplyEdit :: Method
  Method_WorkspaceDidChangeWorkspaceFolders :: Method
  Method_WindowWorkDoneProgressCancel :: Method
  Method_WorkspaceDidCreateFiles :: Method
  Method_WorkspaceDidRenameFiles :: Method
  Method_WorkspaceDidDeleteFiles :: Method
  Method_NotebookDocumentDidOpen :: Method
  Method_NotebookDocumentDidChange :: Method
  Method_NotebookDocumentDidSave :: Method
  Method_NotebookDocumentDidClose :: Method
  Method_Initialized :: Method
  Method_Exit :: Method
  Method_WorkspaceDidChangeConfiguration :: Method
  Method_WindowShowMessage :: Method
  Method_WindowLogMessage :: Method
  Method_TelemetryEvent :: Method
  Method_TextDocumentDidOpen :: Method
  Method_TextDocumentDidChange :: Method
  Method_TextDocumentDidClose :: Method
  Method_TextDocumentDidSave :: Method
  Method_TextDocumentWillSave :: Method
  Method_WorkspaceDidChangeWatchedFiles :: Method
  Method_TextDocumentPublishDiagnostics :: Method
  Method_SetTrace :: Method
  Method_LogTrace :: Method
  Method_CancelRequest :: Method
  Method_Progress :: Method
  deriving stock (Show, Eq, Ord, Generic)

-- | Maps a LSP method to its parameter type.
type MessageParams :: Method -> Kind.Type
type family MessageParams (m ::  Method) where 
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

-- | Maps a LSP method to its result type.
type MessageResult :: Method -> Kind.Type
type family MessageResult (m ::  Method) where 
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
  MessageResult Method_WorkspaceDidChangeWorkspaceFolders = Maybe Data.Void.Void
  MessageResult Method_WindowWorkDoneProgressCancel = Maybe Data.Void.Void
  MessageResult Method_WorkspaceDidCreateFiles = Maybe Data.Void.Void
  MessageResult Method_WorkspaceDidRenameFiles = Maybe Data.Void.Void
  MessageResult Method_WorkspaceDidDeleteFiles = Maybe Data.Void.Void
  MessageResult Method_NotebookDocumentDidOpen = Maybe Data.Void.Void
  MessageResult Method_NotebookDocumentDidChange = Maybe Data.Void.Void
  MessageResult Method_NotebookDocumentDidSave = Maybe Data.Void.Void
  MessageResult Method_NotebookDocumentDidClose = Maybe Data.Void.Void
  MessageResult Method_Initialized = Maybe Data.Void.Void
  MessageResult Method_Exit = Maybe Data.Void.Void
  MessageResult Method_WorkspaceDidChangeConfiguration = Maybe Data.Void.Void
  MessageResult Method_WindowShowMessage = Maybe Data.Void.Void
  MessageResult Method_WindowLogMessage = Maybe Data.Void.Void
  MessageResult Method_TelemetryEvent = Maybe Data.Void.Void
  MessageResult Method_TextDocumentDidOpen = Maybe Data.Void.Void
  MessageResult Method_TextDocumentDidChange = Maybe Data.Void.Void
  MessageResult Method_TextDocumentDidClose = Maybe Data.Void.Void
  MessageResult Method_TextDocumentDidSave = Maybe Data.Void.Void
  MessageResult Method_TextDocumentWillSave = Maybe Data.Void.Void
  MessageResult Method_WorkspaceDidChangeWatchedFiles = Maybe Data.Void.Void
  MessageResult Method_TextDocumentPublishDiagnostics = Maybe Data.Void.Void
  MessageResult Method_SetTrace = Maybe Data.Void.Void
  MessageResult Method_LogTrace = Maybe Data.Void.Void
  MessageResult Method_CancelRequest = Maybe Data.Void.Void
  MessageResult Method_Progress = Maybe Data.Void.Void

-- | Maps a LSP method to its error data type.
type ErrorData :: Method -> Kind.Type
type family ErrorData (m ::  Method) where 
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
  ErrorData Method_WorkspaceDidChangeWorkspaceFolders = Maybe Data.Void.Void
  ErrorData Method_WindowWorkDoneProgressCancel = Maybe Data.Void.Void
  ErrorData Method_WorkspaceDidCreateFiles = Maybe Data.Void.Void
  ErrorData Method_WorkspaceDidRenameFiles = Maybe Data.Void.Void
  ErrorData Method_WorkspaceDidDeleteFiles = Maybe Data.Void.Void
  ErrorData Method_NotebookDocumentDidOpen = Maybe Data.Void.Void
  ErrorData Method_NotebookDocumentDidChange = Maybe Data.Void.Void
  ErrorData Method_NotebookDocumentDidSave = Maybe Data.Void.Void
  ErrorData Method_NotebookDocumentDidClose = Maybe Data.Void.Void
  ErrorData Method_Initialized = Maybe Data.Void.Void
  ErrorData Method_Exit = Maybe Data.Void.Void
  ErrorData Method_WorkspaceDidChangeConfiguration = Maybe Data.Void.Void
  ErrorData Method_WindowShowMessage = Maybe Data.Void.Void
  ErrorData Method_WindowLogMessage = Maybe Data.Void.Void
  ErrorData Method_TelemetryEvent = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDidOpen = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDidChange = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDidClose = Maybe Data.Void.Void
  ErrorData Method_TextDocumentDidSave = Maybe Data.Void.Void
  ErrorData Method_TextDocumentWillSave = Maybe Data.Void.Void
  ErrorData Method_WorkspaceDidChangeWatchedFiles = Maybe Data.Void.Void
  ErrorData Method_TextDocumentPublishDiagnostics = Maybe Data.Void.Void
  ErrorData Method_SetTrace = Maybe Data.Void.Void
  ErrorData Method_LogTrace = Maybe Data.Void.Void
  ErrorData Method_CancelRequest = Maybe Data.Void.Void
  ErrorData Method_Progress = Maybe Data.Void.Void

-- | Maps a LSP method to its registration options type.
type RegistrationOptions :: Method -> Kind.Type
type family RegistrationOptions (m ::  Method) where 
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
  RegistrationOptions Method_NotebookDocumentDidOpen = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  RegistrationOptions Method_NotebookDocumentDidChange = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  RegistrationOptions Method_NotebookDocumentDidSave = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
  RegistrationOptions Method_NotebookDocumentDidClose = Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
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

-- | A singleton type for 'Method'.
type SMethod :: Method -> Kind.Type
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

-- | Turn a 'Method' into its LSP method string.
methodToMethodString :: Method -> String
methodToMethodString Method_TextDocumentImplementation = "textDocument/implementation"
methodToMethodString Method_TextDocumentTypeDefinition = "textDocument/typeDefinition"
methodToMethodString Method_WorkspaceWorkspaceFolders = "workspace/workspaceFolders"
methodToMethodString Method_WorkspaceConfiguration = "workspace/configuration"
methodToMethodString Method_TextDocumentDocumentColor = "textDocument/documentColor"
methodToMethodString Method_TextDocumentColorPresentation = "textDocument/colorPresentation"
methodToMethodString Method_TextDocumentFoldingRange = "textDocument/foldingRange"
methodToMethodString Method_TextDocumentDeclaration = "textDocument/declaration"
methodToMethodString Method_TextDocumentSelectionRange = "textDocument/selectionRange"
methodToMethodString Method_WindowWorkDoneProgressCreate = "window/workDoneProgress/create"
methodToMethodString Method_TextDocumentPrepareCallHierarchy = "textDocument/prepareCallHierarchy"
methodToMethodString Method_CallHierarchyIncomingCalls = "callHierarchy/incomingCalls"
methodToMethodString Method_CallHierarchyOutgoingCalls = "callHierarchy/outgoingCalls"
methodToMethodString Method_TextDocumentSemanticTokensFull = "textDocument/semanticTokens/full"
methodToMethodString Method_TextDocumentSemanticTokensFullDelta = "textDocument/semanticTokens/full/delta"
methodToMethodString Method_TextDocumentSemanticTokensRange = "textDocument/semanticTokens/range"
methodToMethodString Method_WorkspaceSemanticTokensRefresh = "workspace/semanticTokens/refresh"
methodToMethodString Method_WindowShowDocument = "window/showDocument"
methodToMethodString Method_TextDocumentLinkedEditingRange = "textDocument/linkedEditingRange"
methodToMethodString Method_WorkspaceWillCreateFiles = "workspace/willCreateFiles"
methodToMethodString Method_WorkspaceWillRenameFiles = "workspace/willRenameFiles"
methodToMethodString Method_WorkspaceWillDeleteFiles = "workspace/willDeleteFiles"
methodToMethodString Method_TextDocumentMoniker = "textDocument/moniker"
methodToMethodString Method_TextDocumentPrepareTypeHierarchy = "textDocument/prepareTypeHierarchy"
methodToMethodString Method_TypeHierarchySupertypes = "typeHierarchy/supertypes"
methodToMethodString Method_TypeHierarchySubtypes = "typeHierarchy/subtypes"
methodToMethodString Method_TextDocumentInlineValue = "textDocument/inlineValue"
methodToMethodString Method_WorkspaceInlineValueRefresh = "workspace/inlineValue/refresh"
methodToMethodString Method_TextDocumentInlayHint = "textDocument/inlayHint"
methodToMethodString Method_InlayHintResolve = "inlayHint/resolve"
methodToMethodString Method_WorkspaceInlayHintRefresh = "workspace/inlayHint/refresh"
methodToMethodString Method_TextDocumentDiagnostic = "textDocument/diagnostic"
methodToMethodString Method_WorkspaceDiagnostic = "workspace/diagnostic"
methodToMethodString Method_WorkspaceDiagnosticRefresh = "workspace/diagnostic/refresh"
methodToMethodString Method_ClientRegisterCapability = "client/registerCapability"
methodToMethodString Method_ClientUnregisterCapability = "client/unregisterCapability"
methodToMethodString Method_Initialize = "initialize"
methodToMethodString Method_Shutdown = "shutdown"
methodToMethodString Method_WindowShowMessageRequest = "window/showMessageRequest"
methodToMethodString Method_TextDocumentWillSaveWaitUntil = "textDocument/willSaveWaitUntil"
methodToMethodString Method_TextDocumentCompletion = "textDocument/completion"
methodToMethodString Method_CompletionItemResolve = "completionItem/resolve"
methodToMethodString Method_TextDocumentHover = "textDocument/hover"
methodToMethodString Method_TextDocumentSignatureHelp = "textDocument/signatureHelp"
methodToMethodString Method_TextDocumentDefinition = "textDocument/definition"
methodToMethodString Method_TextDocumentReferences = "textDocument/references"
methodToMethodString Method_TextDocumentDocumentHighlight = "textDocument/documentHighlight"
methodToMethodString Method_TextDocumentDocumentSymbol = "textDocument/documentSymbol"
methodToMethodString Method_TextDocumentCodeAction = "textDocument/codeAction"
methodToMethodString Method_CodeActionResolve = "codeAction/resolve"
methodToMethodString Method_WorkspaceSymbol = "workspace/symbol"
methodToMethodString Method_WorkspaceSymbolResolve = "workspaceSymbol/resolve"
methodToMethodString Method_TextDocumentCodeLens = "textDocument/codeLens"
methodToMethodString Method_CodeLensResolve = "codeLens/resolve"
methodToMethodString Method_WorkspaceCodeLensRefresh = "workspace/codeLens/refresh"
methodToMethodString Method_TextDocumentDocumentLink = "textDocument/documentLink"
methodToMethodString Method_DocumentLinkResolve = "documentLink/resolve"
methodToMethodString Method_TextDocumentFormatting = "textDocument/formatting"
methodToMethodString Method_TextDocumentRangeFormatting = "textDocument/rangeFormatting"
methodToMethodString Method_TextDocumentOnTypeFormatting = "textDocument/onTypeFormatting"
methodToMethodString Method_TextDocumentRename = "textDocument/rename"
methodToMethodString Method_TextDocumentPrepareRename = "textDocument/prepareRename"
methodToMethodString Method_WorkspaceExecuteCommand = "workspace/executeCommand"
methodToMethodString Method_WorkspaceApplyEdit = "workspace/applyEdit"
methodToMethodString Method_WorkspaceDidChangeWorkspaceFolders = "workspace/didChangeWorkspaceFolders"
methodToMethodString Method_WindowWorkDoneProgressCancel = "window/workDoneProgress/cancel"
methodToMethodString Method_WorkspaceDidCreateFiles = "workspace/didCreateFiles"
methodToMethodString Method_WorkspaceDidRenameFiles = "workspace/didRenameFiles"
methodToMethodString Method_WorkspaceDidDeleteFiles = "workspace/didDeleteFiles"
methodToMethodString Method_NotebookDocumentDidOpen = "notebookDocument/didOpen"
methodToMethodString Method_NotebookDocumentDidChange = "notebookDocument/didChange"
methodToMethodString Method_NotebookDocumentDidSave = "notebookDocument/didSave"
methodToMethodString Method_NotebookDocumentDidClose = "notebookDocument/didClose"
methodToMethodString Method_Initialized = "initialized"
methodToMethodString Method_Exit = "exit"
methodToMethodString Method_WorkspaceDidChangeConfiguration = "workspace/didChangeConfiguration"
methodToMethodString Method_WindowShowMessage = "window/showMessage"
methodToMethodString Method_WindowLogMessage = "window/logMessage"
methodToMethodString Method_TelemetryEvent = "telemetry/event"
methodToMethodString Method_TextDocumentDidOpen = "textDocument/didOpen"
methodToMethodString Method_TextDocumentDidChange = "textDocument/didChange"
methodToMethodString Method_TextDocumentDidClose = "textDocument/didClose"
methodToMethodString Method_TextDocumentDidSave = "textDocument/didSave"
methodToMethodString Method_TextDocumentWillSave = "textDocument/willSave"
methodToMethodString Method_WorkspaceDidChangeWatchedFiles = "workspace/didChangeWatchedFiles"
methodToMethodString Method_TextDocumentPublishDiagnostics = "textDocument/publishDiagnostics"
methodToMethodString Method_SetTrace = "$/setTrace"
methodToMethodString Method_LogTrace = "$/logTrace"
methodToMethodString Method_CancelRequest = "$/cancelRequest"
methodToMethodString Method_Progress = "$/progress"

-- | Turn a LSP method string into a 'SomeMethod'.
methodStringToMethod :: String -> Maybe Method
methodStringToMethod "textDocument/implementation" = Just Method_TextDocumentImplementation
methodStringToMethod "textDocument/typeDefinition" = Just Method_TextDocumentTypeDefinition
methodStringToMethod "workspace/workspaceFolders" = Just Method_WorkspaceWorkspaceFolders
methodStringToMethod "workspace/configuration" = Just Method_WorkspaceConfiguration
methodStringToMethod "textDocument/documentColor" = Just Method_TextDocumentDocumentColor
methodStringToMethod "textDocument/colorPresentation" = Just Method_TextDocumentColorPresentation
methodStringToMethod "textDocument/foldingRange" = Just Method_TextDocumentFoldingRange
methodStringToMethod "textDocument/declaration" = Just Method_TextDocumentDeclaration
methodStringToMethod "textDocument/selectionRange" = Just Method_TextDocumentSelectionRange
methodStringToMethod "window/workDoneProgress/create" = Just Method_WindowWorkDoneProgressCreate
methodStringToMethod "textDocument/prepareCallHierarchy" = Just Method_TextDocumentPrepareCallHierarchy
methodStringToMethod "callHierarchy/incomingCalls" = Just Method_CallHierarchyIncomingCalls
methodStringToMethod "callHierarchy/outgoingCalls" = Just Method_CallHierarchyOutgoingCalls
methodStringToMethod "textDocument/semanticTokens/full" = Just Method_TextDocumentSemanticTokensFull
methodStringToMethod "textDocument/semanticTokens/full/delta" = Just Method_TextDocumentSemanticTokensFullDelta
methodStringToMethod "textDocument/semanticTokens/range" = Just Method_TextDocumentSemanticTokensRange
methodStringToMethod "workspace/semanticTokens/refresh" = Just Method_WorkspaceSemanticTokensRefresh
methodStringToMethod "window/showDocument" = Just Method_WindowShowDocument
methodStringToMethod "textDocument/linkedEditingRange" = Just Method_TextDocumentLinkedEditingRange
methodStringToMethod "workspace/willCreateFiles" = Just Method_WorkspaceWillCreateFiles
methodStringToMethod "workspace/willRenameFiles" = Just Method_WorkspaceWillRenameFiles
methodStringToMethod "workspace/willDeleteFiles" = Just Method_WorkspaceWillDeleteFiles
methodStringToMethod "textDocument/moniker" = Just Method_TextDocumentMoniker
methodStringToMethod "textDocument/prepareTypeHierarchy" = Just Method_TextDocumentPrepareTypeHierarchy
methodStringToMethod "typeHierarchy/supertypes" = Just Method_TypeHierarchySupertypes
methodStringToMethod "typeHierarchy/subtypes" = Just Method_TypeHierarchySubtypes
methodStringToMethod "textDocument/inlineValue" = Just Method_TextDocumentInlineValue
methodStringToMethod "workspace/inlineValue/refresh" = Just Method_WorkspaceInlineValueRefresh
methodStringToMethod "textDocument/inlayHint" = Just Method_TextDocumentInlayHint
methodStringToMethod "inlayHint/resolve" = Just Method_InlayHintResolve
methodStringToMethod "workspace/inlayHint/refresh" = Just Method_WorkspaceInlayHintRefresh
methodStringToMethod "textDocument/diagnostic" = Just Method_TextDocumentDiagnostic
methodStringToMethod "workspace/diagnostic" = Just Method_WorkspaceDiagnostic
methodStringToMethod "workspace/diagnostic/refresh" = Just Method_WorkspaceDiagnosticRefresh
methodStringToMethod "client/registerCapability" = Just Method_ClientRegisterCapability
methodStringToMethod "client/unregisterCapability" = Just Method_ClientUnregisterCapability
methodStringToMethod "initialize" = Just Method_Initialize
methodStringToMethod "shutdown" = Just Method_Shutdown
methodStringToMethod "window/showMessageRequest" = Just Method_WindowShowMessageRequest
methodStringToMethod "textDocument/willSaveWaitUntil" = Just Method_TextDocumentWillSaveWaitUntil
methodStringToMethod "textDocument/completion" = Just Method_TextDocumentCompletion
methodStringToMethod "completionItem/resolve" = Just Method_CompletionItemResolve
methodStringToMethod "textDocument/hover" = Just Method_TextDocumentHover
methodStringToMethod "textDocument/signatureHelp" = Just Method_TextDocumentSignatureHelp
methodStringToMethod "textDocument/definition" = Just Method_TextDocumentDefinition
methodStringToMethod "textDocument/references" = Just Method_TextDocumentReferences
methodStringToMethod "textDocument/documentHighlight" = Just Method_TextDocumentDocumentHighlight
methodStringToMethod "textDocument/documentSymbol" = Just Method_TextDocumentDocumentSymbol
methodStringToMethod "textDocument/codeAction" = Just Method_TextDocumentCodeAction
methodStringToMethod "codeAction/resolve" = Just Method_CodeActionResolve
methodStringToMethod "workspace/symbol" = Just Method_WorkspaceSymbol
methodStringToMethod "workspaceSymbol/resolve" = Just Method_WorkspaceSymbolResolve
methodStringToMethod "textDocument/codeLens" = Just Method_TextDocumentCodeLens
methodStringToMethod "codeLens/resolve" = Just Method_CodeLensResolve
methodStringToMethod "workspace/codeLens/refresh" = Just Method_WorkspaceCodeLensRefresh
methodStringToMethod "textDocument/documentLink" = Just Method_TextDocumentDocumentLink
methodStringToMethod "documentLink/resolve" = Just Method_DocumentLinkResolve
methodStringToMethod "textDocument/formatting" = Just Method_TextDocumentFormatting
methodStringToMethod "textDocument/rangeFormatting" = Just Method_TextDocumentRangeFormatting
methodStringToMethod "textDocument/onTypeFormatting" = Just Method_TextDocumentOnTypeFormatting
methodStringToMethod "textDocument/rename" = Just Method_TextDocumentRename
methodStringToMethod "textDocument/prepareRename" = Just Method_TextDocumentPrepareRename
methodStringToMethod "workspace/executeCommand" = Just Method_WorkspaceExecuteCommand
methodStringToMethod "workspace/applyEdit" = Just Method_WorkspaceApplyEdit
methodStringToMethod "workspace/didChangeWorkspaceFolders" = Just Method_WorkspaceDidChangeWorkspaceFolders
methodStringToMethod "window/workDoneProgress/cancel" = Just Method_WindowWorkDoneProgressCancel
methodStringToMethod "workspace/didCreateFiles" = Just Method_WorkspaceDidCreateFiles
methodStringToMethod "workspace/didRenameFiles" = Just Method_WorkspaceDidRenameFiles
methodStringToMethod "workspace/didDeleteFiles" = Just Method_WorkspaceDidDeleteFiles
methodStringToMethod "notebookDocument/didOpen" = Just Method_NotebookDocumentDidOpen
methodStringToMethod "notebookDocument/didChange" = Just Method_NotebookDocumentDidChange
methodStringToMethod "notebookDocument/didSave" = Just Method_NotebookDocumentDidSave
methodStringToMethod "notebookDocument/didClose" = Just Method_NotebookDocumentDidClose
methodStringToMethod "initialized" = Just Method_Initialized
methodStringToMethod "exit" = Just Method_Exit
methodStringToMethod "workspace/didChangeConfiguration" = Just Method_WorkspaceDidChangeConfiguration
methodStringToMethod "window/showMessage" = Just Method_WindowShowMessage
methodStringToMethod "window/logMessage" = Just Method_WindowLogMessage
methodStringToMethod "telemetry/event" = Just Method_TelemetryEvent
methodStringToMethod "textDocument/didOpen" = Just Method_TextDocumentDidOpen
methodStringToMethod "textDocument/didChange" = Just Method_TextDocumentDidChange
methodStringToMethod "textDocument/didClose" = Just Method_TextDocumentDidClose
methodStringToMethod "textDocument/didSave" = Just Method_TextDocumentDidSave
methodStringToMethod "textDocument/willSave" = Just Method_TextDocumentWillSave
methodStringToMethod "workspace/didChangeWatchedFiles" = Just Method_WorkspaceDidChangeWatchedFiles
methodStringToMethod "textDocument/publishDiagnostics" = Just Method_TextDocumentPublishDiagnostics
methodStringToMethod "$/setTrace" = Just Method_SetTrace
methodStringToMethod "$/logTrace" = Just Method_LogTrace
methodStringToMethod "$/cancelRequest" = Just Method_CancelRequest
methodStringToMethod "$/progress" = Just Method_Progress
methodStringToMethod _ = Nothing

-- | Maps a LSP method to its message direction.
type MethodDirection :: Method -> MM.Initiator
type family MethodDirection (m ::  Method) where 
  MethodDirection Method_TextDocumentImplementation = MM.ClientInitiates
  MethodDirection Method_TextDocumentTypeDefinition = MM.ClientInitiates
  MethodDirection Method_WorkspaceWorkspaceFolders = MM.ServerInitiates
  MethodDirection Method_WorkspaceConfiguration = MM.ServerInitiates
  MethodDirection Method_TextDocumentDocumentColor = MM.ClientInitiates
  MethodDirection Method_TextDocumentColorPresentation = MM.ClientInitiates
  MethodDirection Method_TextDocumentFoldingRange = MM.ClientInitiates
  MethodDirection Method_TextDocumentDeclaration = MM.ClientInitiates
  MethodDirection Method_TextDocumentSelectionRange = MM.ClientInitiates
  MethodDirection Method_WindowWorkDoneProgressCreate = MM.ServerInitiates
  MethodDirection Method_TextDocumentPrepareCallHierarchy = MM.ClientInitiates
  MethodDirection Method_CallHierarchyIncomingCalls = MM.ClientInitiates
  MethodDirection Method_CallHierarchyOutgoingCalls = MM.ClientInitiates
  MethodDirection Method_TextDocumentSemanticTokensFull = MM.ClientInitiates
  MethodDirection Method_TextDocumentSemanticTokensFullDelta = MM.ClientInitiates
  MethodDirection Method_TextDocumentSemanticTokensRange = MM.ClientInitiates
  MethodDirection Method_WorkspaceSemanticTokensRefresh = MM.ServerInitiates
  MethodDirection Method_WindowShowDocument = MM.ServerInitiates
  MethodDirection Method_TextDocumentLinkedEditingRange = MM.ClientInitiates
  MethodDirection Method_WorkspaceWillCreateFiles = MM.ClientInitiates
  MethodDirection Method_WorkspaceWillRenameFiles = MM.ClientInitiates
  MethodDirection Method_WorkspaceWillDeleteFiles = MM.ClientInitiates
  MethodDirection Method_TextDocumentMoniker = MM.ClientInitiates
  MethodDirection Method_TextDocumentPrepareTypeHierarchy = MM.ClientInitiates
  MethodDirection Method_TypeHierarchySupertypes = MM.ClientInitiates
  MethodDirection Method_TypeHierarchySubtypes = MM.ClientInitiates
  MethodDirection Method_TextDocumentInlineValue = MM.ClientInitiates
  MethodDirection Method_WorkspaceInlineValueRefresh = MM.ServerInitiates
  MethodDirection Method_TextDocumentInlayHint = MM.ClientInitiates
  MethodDirection Method_InlayHintResolve = MM.ClientInitiates
  MethodDirection Method_WorkspaceInlayHintRefresh = MM.ServerInitiates
  MethodDirection Method_TextDocumentDiagnostic = MM.ClientInitiates
  MethodDirection Method_WorkspaceDiagnostic = MM.ClientInitiates
  MethodDirection Method_WorkspaceDiagnosticRefresh = MM.ServerInitiates
  MethodDirection Method_ClientRegisterCapability = MM.ServerInitiates
  MethodDirection Method_ClientUnregisterCapability = MM.ServerInitiates
  MethodDirection Method_Initialize = MM.ClientInitiates
  MethodDirection Method_Shutdown = MM.ClientInitiates
  MethodDirection Method_WindowShowMessageRequest = MM.ServerInitiates
  MethodDirection Method_TextDocumentWillSaveWaitUntil = MM.ClientInitiates
  MethodDirection Method_TextDocumentCompletion = MM.ClientInitiates
  MethodDirection Method_CompletionItemResolve = MM.ClientInitiates
  MethodDirection Method_TextDocumentHover = MM.ClientInitiates
  MethodDirection Method_TextDocumentSignatureHelp = MM.ClientInitiates
  MethodDirection Method_TextDocumentDefinition = MM.ClientInitiates
  MethodDirection Method_TextDocumentReferences = MM.ClientInitiates
  MethodDirection Method_TextDocumentDocumentHighlight = MM.ClientInitiates
  MethodDirection Method_TextDocumentDocumentSymbol = MM.ClientInitiates
  MethodDirection Method_TextDocumentCodeAction = MM.ClientInitiates
  MethodDirection Method_CodeActionResolve = MM.ClientInitiates
  MethodDirection Method_WorkspaceSymbol = MM.ClientInitiates
  MethodDirection Method_WorkspaceSymbolResolve = MM.ClientInitiates
  MethodDirection Method_TextDocumentCodeLens = MM.ClientInitiates
  MethodDirection Method_CodeLensResolve = MM.ClientInitiates
  MethodDirection Method_WorkspaceCodeLensRefresh = MM.ServerInitiates
  MethodDirection Method_TextDocumentDocumentLink = MM.ClientInitiates
  MethodDirection Method_DocumentLinkResolve = MM.ClientInitiates
  MethodDirection Method_TextDocumentFormatting = MM.ClientInitiates
  MethodDirection Method_TextDocumentRangeFormatting = MM.ClientInitiates
  MethodDirection Method_TextDocumentOnTypeFormatting = MM.ClientInitiates
  MethodDirection Method_TextDocumentRename = MM.ClientInitiates
  MethodDirection Method_TextDocumentPrepareRename = MM.ClientInitiates
  MethodDirection Method_WorkspaceExecuteCommand = MM.ClientInitiates
  MethodDirection Method_WorkspaceApplyEdit = MM.ServerInitiates
  MethodDirection Method_WorkspaceDidChangeWorkspaceFolders = MM.ClientInitiates
  MethodDirection Method_WindowWorkDoneProgressCancel = MM.ClientInitiates
  MethodDirection Method_WorkspaceDidCreateFiles = MM.ClientInitiates
  MethodDirection Method_WorkspaceDidRenameFiles = MM.ClientInitiates
  MethodDirection Method_WorkspaceDidDeleteFiles = MM.ClientInitiates
  MethodDirection Method_NotebookDocumentDidOpen = MM.ClientInitiates
  MethodDirection Method_NotebookDocumentDidChange = MM.ClientInitiates
  MethodDirection Method_NotebookDocumentDidSave = MM.ClientInitiates
  MethodDirection Method_NotebookDocumentDidClose = MM.ClientInitiates
  MethodDirection Method_Initialized = MM.ClientInitiates
  MethodDirection Method_Exit = MM.ClientInitiates
  MethodDirection Method_WorkspaceDidChangeConfiguration = MM.ClientInitiates
  MethodDirection Method_WindowShowMessage = MM.ServerInitiates
  MethodDirection Method_WindowLogMessage = MM.ServerInitiates
  MethodDirection Method_TelemetryEvent = MM.ServerInitiates
  MethodDirection Method_TextDocumentDidOpen = MM.ClientInitiates
  MethodDirection Method_TextDocumentDidChange = MM.ClientInitiates
  MethodDirection Method_TextDocumentDidClose = MM.ClientInitiates
  MethodDirection Method_TextDocumentDidSave = MM.ClientInitiates
  MethodDirection Method_TextDocumentWillSave = MM.ClientInitiates
  MethodDirection Method_WorkspaceDidChangeWatchedFiles = MM.ClientInitiates
  MethodDirection Method_TextDocumentPublishDiagnostics = MM.ServerInitiates
  MethodDirection Method_SetTrace = MM.ClientInitiates
  MethodDirection Method_LogTrace = MM.ServerInitiates
  MethodDirection Method_CancelRequest = MM.EitherInitiates
  MethodDirection Method_Progress = MM.EitherInitiates

-- | Get a singleton witness for the message direction of a 'SMethod'.
methodDirection :: forall m . SMethod m -> MM.SInitiator (MethodDirection m)
methodDirection SMethod_TextDocumentImplementation = MM.SClientInitiates
methodDirection SMethod_TextDocumentTypeDefinition = MM.SClientInitiates
methodDirection SMethod_WorkspaceWorkspaceFolders = MM.SServerInitiates
methodDirection SMethod_WorkspaceConfiguration = MM.SServerInitiates
methodDirection SMethod_TextDocumentDocumentColor = MM.SClientInitiates
methodDirection SMethod_TextDocumentColorPresentation = MM.SClientInitiates
methodDirection SMethod_TextDocumentFoldingRange = MM.SClientInitiates
methodDirection SMethod_TextDocumentDeclaration = MM.SClientInitiates
methodDirection SMethod_TextDocumentSelectionRange = MM.SClientInitiates
methodDirection SMethod_WindowWorkDoneProgressCreate = MM.SServerInitiates
methodDirection SMethod_TextDocumentPrepareCallHierarchy = MM.SClientInitiates
methodDirection SMethod_CallHierarchyIncomingCalls = MM.SClientInitiates
methodDirection SMethod_CallHierarchyOutgoingCalls = MM.SClientInitiates
methodDirection SMethod_TextDocumentSemanticTokensFull = MM.SClientInitiates
methodDirection SMethod_TextDocumentSemanticTokensFullDelta = MM.SClientInitiates
methodDirection SMethod_TextDocumentSemanticTokensRange = MM.SClientInitiates
methodDirection SMethod_WorkspaceSemanticTokensRefresh = MM.SServerInitiates
methodDirection SMethod_WindowShowDocument = MM.SServerInitiates
methodDirection SMethod_TextDocumentLinkedEditingRange = MM.SClientInitiates
methodDirection SMethod_WorkspaceWillCreateFiles = MM.SClientInitiates
methodDirection SMethod_WorkspaceWillRenameFiles = MM.SClientInitiates
methodDirection SMethod_WorkspaceWillDeleteFiles = MM.SClientInitiates
methodDirection SMethod_TextDocumentMoniker = MM.SClientInitiates
methodDirection SMethod_TextDocumentPrepareTypeHierarchy = MM.SClientInitiates
methodDirection SMethod_TypeHierarchySupertypes = MM.SClientInitiates
methodDirection SMethod_TypeHierarchySubtypes = MM.SClientInitiates
methodDirection SMethod_TextDocumentInlineValue = MM.SClientInitiates
methodDirection SMethod_WorkspaceInlineValueRefresh = MM.SServerInitiates
methodDirection SMethod_TextDocumentInlayHint = MM.SClientInitiates
methodDirection SMethod_InlayHintResolve = MM.SClientInitiates
methodDirection SMethod_WorkspaceInlayHintRefresh = MM.SServerInitiates
methodDirection SMethod_TextDocumentDiagnostic = MM.SClientInitiates
methodDirection SMethod_WorkspaceDiagnostic = MM.SClientInitiates
methodDirection SMethod_WorkspaceDiagnosticRefresh = MM.SServerInitiates
methodDirection SMethod_ClientRegisterCapability = MM.SServerInitiates
methodDirection SMethod_ClientUnregisterCapability = MM.SServerInitiates
methodDirection SMethod_Initialize = MM.SClientInitiates
methodDirection SMethod_Shutdown = MM.SClientInitiates
methodDirection SMethod_WindowShowMessageRequest = MM.SServerInitiates
methodDirection SMethod_TextDocumentWillSaveWaitUntil = MM.SClientInitiates
methodDirection SMethod_TextDocumentCompletion = MM.SClientInitiates
methodDirection SMethod_CompletionItemResolve = MM.SClientInitiates
methodDirection SMethod_TextDocumentHover = MM.SClientInitiates
methodDirection SMethod_TextDocumentSignatureHelp = MM.SClientInitiates
methodDirection SMethod_TextDocumentDefinition = MM.SClientInitiates
methodDirection SMethod_TextDocumentReferences = MM.SClientInitiates
methodDirection SMethod_TextDocumentDocumentHighlight = MM.SClientInitiates
methodDirection SMethod_TextDocumentDocumentSymbol = MM.SClientInitiates
methodDirection SMethod_TextDocumentCodeAction = MM.SClientInitiates
methodDirection SMethod_CodeActionResolve = MM.SClientInitiates
methodDirection SMethod_WorkspaceSymbol = MM.SClientInitiates
methodDirection SMethod_WorkspaceSymbolResolve = MM.SClientInitiates
methodDirection SMethod_TextDocumentCodeLens = MM.SClientInitiates
methodDirection SMethod_CodeLensResolve = MM.SClientInitiates
methodDirection SMethod_WorkspaceCodeLensRefresh = MM.SServerInitiates
methodDirection SMethod_TextDocumentDocumentLink = MM.SClientInitiates
methodDirection SMethod_DocumentLinkResolve = MM.SClientInitiates
methodDirection SMethod_TextDocumentFormatting = MM.SClientInitiates
methodDirection SMethod_TextDocumentRangeFormatting = MM.SClientInitiates
methodDirection SMethod_TextDocumentOnTypeFormatting = MM.SClientInitiates
methodDirection SMethod_TextDocumentRename = MM.SClientInitiates
methodDirection SMethod_TextDocumentPrepareRename = MM.SClientInitiates
methodDirection SMethod_WorkspaceExecuteCommand = MM.SClientInitiates
methodDirection SMethod_WorkspaceApplyEdit = MM.SServerInitiates
methodDirection SMethod_WorkspaceDidChangeWorkspaceFolders = MM.SClientInitiates
methodDirection SMethod_WindowWorkDoneProgressCancel = MM.SClientInitiates
methodDirection SMethod_WorkspaceDidCreateFiles = MM.SClientInitiates
methodDirection SMethod_WorkspaceDidRenameFiles = MM.SClientInitiates
methodDirection SMethod_WorkspaceDidDeleteFiles = MM.SClientInitiates
methodDirection SMethod_NotebookDocumentDidOpen = MM.SClientInitiates
methodDirection SMethod_NotebookDocumentDidChange = MM.SClientInitiates
methodDirection SMethod_NotebookDocumentDidSave = MM.SClientInitiates
methodDirection SMethod_NotebookDocumentDidClose = MM.SClientInitiates
methodDirection SMethod_Initialized = MM.SClientInitiates
methodDirection SMethod_Exit = MM.SClientInitiates
methodDirection SMethod_WorkspaceDidChangeConfiguration = MM.SClientInitiates
methodDirection SMethod_WindowShowMessage = MM.SServerInitiates
methodDirection SMethod_WindowLogMessage = MM.SServerInitiates
methodDirection SMethod_TelemetryEvent = MM.SServerInitiates
methodDirection SMethod_TextDocumentDidOpen = MM.SClientInitiates
methodDirection SMethod_TextDocumentDidChange = MM.SClientInitiates
methodDirection SMethod_TextDocumentDidClose = MM.SClientInitiates
methodDirection SMethod_TextDocumentDidSave = MM.SClientInitiates
methodDirection SMethod_TextDocumentWillSave = MM.SClientInitiates
methodDirection SMethod_WorkspaceDidChangeWatchedFiles = MM.SClientInitiates
methodDirection SMethod_TextDocumentPublishDiagnostics = MM.SServerInitiates
methodDirection SMethod_SetTrace = MM.SClientInitiates
methodDirection SMethod_LogTrace = MM.SServerInitiates
methodDirection SMethod_CancelRequest = MM.SEitherInitiates
methodDirection SMethod_Progress = MM.SEitherInitiates

-- | Maps a LSP method to its message direction.
type MethodType :: Method -> MM.RequestOrNotification
type family MethodType (m ::  Method) where 
  MethodType Method_TextDocumentImplementation = MM.Request
  MethodType Method_TextDocumentTypeDefinition = MM.Request
  MethodType Method_WorkspaceWorkspaceFolders = MM.Request
  MethodType Method_WorkspaceConfiguration = MM.Request
  MethodType Method_TextDocumentDocumentColor = MM.Request
  MethodType Method_TextDocumentColorPresentation = MM.Request
  MethodType Method_TextDocumentFoldingRange = MM.Request
  MethodType Method_TextDocumentDeclaration = MM.Request
  MethodType Method_TextDocumentSelectionRange = MM.Request
  MethodType Method_WindowWorkDoneProgressCreate = MM.Request
  MethodType Method_TextDocumentPrepareCallHierarchy = MM.Request
  MethodType Method_CallHierarchyIncomingCalls = MM.Request
  MethodType Method_CallHierarchyOutgoingCalls = MM.Request
  MethodType Method_TextDocumentSemanticTokensFull = MM.Request
  MethodType Method_TextDocumentSemanticTokensFullDelta = MM.Request
  MethodType Method_TextDocumentSemanticTokensRange = MM.Request
  MethodType Method_WorkspaceSemanticTokensRefresh = MM.Request
  MethodType Method_WindowShowDocument = MM.Request
  MethodType Method_TextDocumentLinkedEditingRange = MM.Request
  MethodType Method_WorkspaceWillCreateFiles = MM.Request
  MethodType Method_WorkspaceWillRenameFiles = MM.Request
  MethodType Method_WorkspaceWillDeleteFiles = MM.Request
  MethodType Method_TextDocumentMoniker = MM.Request
  MethodType Method_TextDocumentPrepareTypeHierarchy = MM.Request
  MethodType Method_TypeHierarchySupertypes = MM.Request
  MethodType Method_TypeHierarchySubtypes = MM.Request
  MethodType Method_TextDocumentInlineValue = MM.Request
  MethodType Method_WorkspaceInlineValueRefresh = MM.Request
  MethodType Method_TextDocumentInlayHint = MM.Request
  MethodType Method_InlayHintResolve = MM.Request
  MethodType Method_WorkspaceInlayHintRefresh = MM.Request
  MethodType Method_TextDocumentDiagnostic = MM.Request
  MethodType Method_WorkspaceDiagnostic = MM.Request
  MethodType Method_WorkspaceDiagnosticRefresh = MM.Request
  MethodType Method_ClientRegisterCapability = MM.Request
  MethodType Method_ClientUnregisterCapability = MM.Request
  MethodType Method_Initialize = MM.Request
  MethodType Method_Shutdown = MM.Request
  MethodType Method_WindowShowMessageRequest = MM.Request
  MethodType Method_TextDocumentWillSaveWaitUntil = MM.Request
  MethodType Method_TextDocumentCompletion = MM.Request
  MethodType Method_CompletionItemResolve = MM.Request
  MethodType Method_TextDocumentHover = MM.Request
  MethodType Method_TextDocumentSignatureHelp = MM.Request
  MethodType Method_TextDocumentDefinition = MM.Request
  MethodType Method_TextDocumentReferences = MM.Request
  MethodType Method_TextDocumentDocumentHighlight = MM.Request
  MethodType Method_TextDocumentDocumentSymbol = MM.Request
  MethodType Method_TextDocumentCodeAction = MM.Request
  MethodType Method_CodeActionResolve = MM.Request
  MethodType Method_WorkspaceSymbol = MM.Request
  MethodType Method_WorkspaceSymbolResolve = MM.Request
  MethodType Method_TextDocumentCodeLens = MM.Request
  MethodType Method_CodeLensResolve = MM.Request
  MethodType Method_WorkspaceCodeLensRefresh = MM.Request
  MethodType Method_TextDocumentDocumentLink = MM.Request
  MethodType Method_DocumentLinkResolve = MM.Request
  MethodType Method_TextDocumentFormatting = MM.Request
  MethodType Method_TextDocumentRangeFormatting = MM.Request
  MethodType Method_TextDocumentOnTypeFormatting = MM.Request
  MethodType Method_TextDocumentRename = MM.Request
  MethodType Method_TextDocumentPrepareRename = MM.Request
  MethodType Method_WorkspaceExecuteCommand = MM.Request
  MethodType Method_WorkspaceApplyEdit = MM.Request
  MethodType Method_WorkspaceDidChangeWorkspaceFolders = MM.Notification
  MethodType Method_WindowWorkDoneProgressCancel = MM.Notification
  MethodType Method_WorkspaceDidCreateFiles = MM.Notification
  MethodType Method_WorkspaceDidRenameFiles = MM.Notification
  MethodType Method_WorkspaceDidDeleteFiles = MM.Notification
  MethodType Method_NotebookDocumentDidOpen = MM.Notification
  MethodType Method_NotebookDocumentDidChange = MM.Notification
  MethodType Method_NotebookDocumentDidSave = MM.Notification
  MethodType Method_NotebookDocumentDidClose = MM.Notification
  MethodType Method_Initialized = MM.Notification
  MethodType Method_Exit = MM.Notification
  MethodType Method_WorkspaceDidChangeConfiguration = MM.Notification
  MethodType Method_WindowShowMessage = MM.Notification
  MethodType Method_WindowLogMessage = MM.Notification
  MethodType Method_TelemetryEvent = MM.Notification
  MethodType Method_TextDocumentDidOpen = MM.Notification
  MethodType Method_TextDocumentDidChange = MM.Notification
  MethodType Method_TextDocumentDidClose = MM.Notification
  MethodType Method_TextDocumentDidSave = MM.Notification
  MethodType Method_TextDocumentWillSave = MM.Notification
  MethodType Method_WorkspaceDidChangeWatchedFiles = MM.Notification
  MethodType Method_TextDocumentPublishDiagnostics = MM.Notification
  MethodType Method_SetTrace = MM.Notification
  MethodType Method_LogTrace = MM.Notification
  MethodType Method_CancelRequest = MM.Notification
  MethodType Method_Progress = MM.Notification

-- | Get a singleton witness for the message kind of a 'SMethod'.
methodType :: forall m . SMethod m -> MM.SRequestOrNotification (MethodType m)
methodType SMethod_TextDocumentImplementation = MM.SRequest
methodType SMethod_TextDocumentTypeDefinition = MM.SRequest
methodType SMethod_WorkspaceWorkspaceFolders = MM.SRequest
methodType SMethod_WorkspaceConfiguration = MM.SRequest
methodType SMethod_TextDocumentDocumentColor = MM.SRequest
methodType SMethod_TextDocumentColorPresentation = MM.SRequest
methodType SMethod_TextDocumentFoldingRange = MM.SRequest
methodType SMethod_TextDocumentDeclaration = MM.SRequest
methodType SMethod_TextDocumentSelectionRange = MM.SRequest
methodType SMethod_WindowWorkDoneProgressCreate = MM.SRequest
methodType SMethod_TextDocumentPrepareCallHierarchy = MM.SRequest
methodType SMethod_CallHierarchyIncomingCalls = MM.SRequest
methodType SMethod_CallHierarchyOutgoingCalls = MM.SRequest
methodType SMethod_TextDocumentSemanticTokensFull = MM.SRequest
methodType SMethod_TextDocumentSemanticTokensFullDelta = MM.SRequest
methodType SMethod_TextDocumentSemanticTokensRange = MM.SRequest
methodType SMethod_WorkspaceSemanticTokensRefresh = MM.SRequest
methodType SMethod_WindowShowDocument = MM.SRequest
methodType SMethod_TextDocumentLinkedEditingRange = MM.SRequest
methodType SMethod_WorkspaceWillCreateFiles = MM.SRequest
methodType SMethod_WorkspaceWillRenameFiles = MM.SRequest
methodType SMethod_WorkspaceWillDeleteFiles = MM.SRequest
methodType SMethod_TextDocumentMoniker = MM.SRequest
methodType SMethod_TextDocumentPrepareTypeHierarchy = MM.SRequest
methodType SMethod_TypeHierarchySupertypes = MM.SRequest
methodType SMethod_TypeHierarchySubtypes = MM.SRequest
methodType SMethod_TextDocumentInlineValue = MM.SRequest
methodType SMethod_WorkspaceInlineValueRefresh = MM.SRequest
methodType SMethod_TextDocumentInlayHint = MM.SRequest
methodType SMethod_InlayHintResolve = MM.SRequest
methodType SMethod_WorkspaceInlayHintRefresh = MM.SRequest
methodType SMethod_TextDocumentDiagnostic = MM.SRequest
methodType SMethod_WorkspaceDiagnostic = MM.SRequest
methodType SMethod_WorkspaceDiagnosticRefresh = MM.SRequest
methodType SMethod_ClientRegisterCapability = MM.SRequest
methodType SMethod_ClientUnregisterCapability = MM.SRequest
methodType SMethod_Initialize = MM.SRequest
methodType SMethod_Shutdown = MM.SRequest
methodType SMethod_WindowShowMessageRequest = MM.SRequest
methodType SMethod_TextDocumentWillSaveWaitUntil = MM.SRequest
methodType SMethod_TextDocumentCompletion = MM.SRequest
methodType SMethod_CompletionItemResolve = MM.SRequest
methodType SMethod_TextDocumentHover = MM.SRequest
methodType SMethod_TextDocumentSignatureHelp = MM.SRequest
methodType SMethod_TextDocumentDefinition = MM.SRequest
methodType SMethod_TextDocumentReferences = MM.SRequest
methodType SMethod_TextDocumentDocumentHighlight = MM.SRequest
methodType SMethod_TextDocumentDocumentSymbol = MM.SRequest
methodType SMethod_TextDocumentCodeAction = MM.SRequest
methodType SMethod_CodeActionResolve = MM.SRequest
methodType SMethod_WorkspaceSymbol = MM.SRequest
methodType SMethod_WorkspaceSymbolResolve = MM.SRequest
methodType SMethod_TextDocumentCodeLens = MM.SRequest
methodType SMethod_CodeLensResolve = MM.SRequest
methodType SMethod_WorkspaceCodeLensRefresh = MM.SRequest
methodType SMethod_TextDocumentDocumentLink = MM.SRequest
methodType SMethod_DocumentLinkResolve = MM.SRequest
methodType SMethod_TextDocumentFormatting = MM.SRequest
methodType SMethod_TextDocumentRangeFormatting = MM.SRequest
methodType SMethod_TextDocumentOnTypeFormatting = MM.SRequest
methodType SMethod_TextDocumentRename = MM.SRequest
methodType SMethod_TextDocumentPrepareRename = MM.SRequest
methodType SMethod_WorkspaceExecuteCommand = MM.SRequest
methodType SMethod_WorkspaceApplyEdit = MM.SRequest
methodType SMethod_WorkspaceDidChangeWorkspaceFolders = MM.SNotification
methodType SMethod_WindowWorkDoneProgressCancel = MM.SNotification
methodType SMethod_WorkspaceDidCreateFiles = MM.SNotification
methodType SMethod_WorkspaceDidRenameFiles = MM.SNotification
methodType SMethod_WorkspaceDidDeleteFiles = MM.SNotification
methodType SMethod_NotebookDocumentDidOpen = MM.SNotification
methodType SMethod_NotebookDocumentDidChange = MM.SNotification
methodType SMethod_NotebookDocumentDidSave = MM.SNotification
methodType SMethod_NotebookDocumentDidClose = MM.SNotification
methodType SMethod_Initialized = MM.SNotification
methodType SMethod_Exit = MM.SNotification
methodType SMethod_WorkspaceDidChangeConfiguration = MM.SNotification
methodType SMethod_WindowShowMessage = MM.SNotification
methodType SMethod_WindowLogMessage = MM.SNotification
methodType SMethod_TelemetryEvent = MM.SNotification
methodType SMethod_TextDocumentDidOpen = MM.SNotification
methodType SMethod_TextDocumentDidChange = MM.SNotification
methodType SMethod_TextDocumentDidClose = MM.SNotification
methodType SMethod_TextDocumentDidSave = MM.SNotification
methodType SMethod_TextDocumentWillSave = MM.SNotification
methodType SMethod_WorkspaceDidChangeWatchedFiles = MM.SNotification
methodType SMethod_TextDocumentPublishDiagnostics = MM.SNotification
methodType SMethod_SetTrace = MM.SNotification
methodType SMethod_LogTrace = MM.SNotification
methodType SMethod_CancelRequest = MM.SNotification
methodType SMethod_Progress = MM.SNotification

type instance S.Sing = SMethod
instance S.SingKind Method where
  type Demote Method = Method
  fromSing SMethod_TextDocumentImplementation = Method_TextDocumentImplementation
  fromSing SMethod_TextDocumentTypeDefinition = Method_TextDocumentTypeDefinition
  fromSing SMethod_WorkspaceWorkspaceFolders = Method_WorkspaceWorkspaceFolders
  fromSing SMethod_WorkspaceConfiguration = Method_WorkspaceConfiguration
  fromSing SMethod_TextDocumentDocumentColor = Method_TextDocumentDocumentColor
  fromSing SMethod_TextDocumentColorPresentation = Method_TextDocumentColorPresentation
  fromSing SMethod_TextDocumentFoldingRange = Method_TextDocumentFoldingRange
  fromSing SMethod_TextDocumentDeclaration = Method_TextDocumentDeclaration
  fromSing SMethod_TextDocumentSelectionRange = Method_TextDocumentSelectionRange
  fromSing SMethod_WindowWorkDoneProgressCreate = Method_WindowWorkDoneProgressCreate
  fromSing SMethod_TextDocumentPrepareCallHierarchy = Method_TextDocumentPrepareCallHierarchy
  fromSing SMethod_CallHierarchyIncomingCalls = Method_CallHierarchyIncomingCalls
  fromSing SMethod_CallHierarchyOutgoingCalls = Method_CallHierarchyOutgoingCalls
  fromSing SMethod_TextDocumentSemanticTokensFull = Method_TextDocumentSemanticTokensFull
  fromSing SMethod_TextDocumentSemanticTokensFullDelta = Method_TextDocumentSemanticTokensFullDelta
  fromSing SMethod_TextDocumentSemanticTokensRange = Method_TextDocumentSemanticTokensRange
  fromSing SMethod_WorkspaceSemanticTokensRefresh = Method_WorkspaceSemanticTokensRefresh
  fromSing SMethod_WindowShowDocument = Method_WindowShowDocument
  fromSing SMethod_TextDocumentLinkedEditingRange = Method_TextDocumentLinkedEditingRange
  fromSing SMethod_WorkspaceWillCreateFiles = Method_WorkspaceWillCreateFiles
  fromSing SMethod_WorkspaceWillRenameFiles = Method_WorkspaceWillRenameFiles
  fromSing SMethod_WorkspaceWillDeleteFiles = Method_WorkspaceWillDeleteFiles
  fromSing SMethod_TextDocumentMoniker = Method_TextDocumentMoniker
  fromSing SMethod_TextDocumentPrepareTypeHierarchy = Method_TextDocumentPrepareTypeHierarchy
  fromSing SMethod_TypeHierarchySupertypes = Method_TypeHierarchySupertypes
  fromSing SMethod_TypeHierarchySubtypes = Method_TypeHierarchySubtypes
  fromSing SMethod_TextDocumentInlineValue = Method_TextDocumentInlineValue
  fromSing SMethod_WorkspaceInlineValueRefresh = Method_WorkspaceInlineValueRefresh
  fromSing SMethod_TextDocumentInlayHint = Method_TextDocumentInlayHint
  fromSing SMethod_InlayHintResolve = Method_InlayHintResolve
  fromSing SMethod_WorkspaceInlayHintRefresh = Method_WorkspaceInlayHintRefresh
  fromSing SMethod_TextDocumentDiagnostic = Method_TextDocumentDiagnostic
  fromSing SMethod_WorkspaceDiagnostic = Method_WorkspaceDiagnostic
  fromSing SMethod_WorkspaceDiagnosticRefresh = Method_WorkspaceDiagnosticRefresh
  fromSing SMethod_ClientRegisterCapability = Method_ClientRegisterCapability
  fromSing SMethod_ClientUnregisterCapability = Method_ClientUnregisterCapability
  fromSing SMethod_Initialize = Method_Initialize
  fromSing SMethod_Shutdown = Method_Shutdown
  fromSing SMethod_WindowShowMessageRequest = Method_WindowShowMessageRequest
  fromSing SMethod_TextDocumentWillSaveWaitUntil = Method_TextDocumentWillSaveWaitUntil
  fromSing SMethod_TextDocumentCompletion = Method_TextDocumentCompletion
  fromSing SMethod_CompletionItemResolve = Method_CompletionItemResolve
  fromSing SMethod_TextDocumentHover = Method_TextDocumentHover
  fromSing SMethod_TextDocumentSignatureHelp = Method_TextDocumentSignatureHelp
  fromSing SMethod_TextDocumentDefinition = Method_TextDocumentDefinition
  fromSing SMethod_TextDocumentReferences = Method_TextDocumentReferences
  fromSing SMethod_TextDocumentDocumentHighlight = Method_TextDocumentDocumentHighlight
  fromSing SMethod_TextDocumentDocumentSymbol = Method_TextDocumentDocumentSymbol
  fromSing SMethod_TextDocumentCodeAction = Method_TextDocumentCodeAction
  fromSing SMethod_CodeActionResolve = Method_CodeActionResolve
  fromSing SMethod_WorkspaceSymbol = Method_WorkspaceSymbol
  fromSing SMethod_WorkspaceSymbolResolve = Method_WorkspaceSymbolResolve
  fromSing SMethod_TextDocumentCodeLens = Method_TextDocumentCodeLens
  fromSing SMethod_CodeLensResolve = Method_CodeLensResolve
  fromSing SMethod_WorkspaceCodeLensRefresh = Method_WorkspaceCodeLensRefresh
  fromSing SMethod_TextDocumentDocumentLink = Method_TextDocumentDocumentLink
  fromSing SMethod_DocumentLinkResolve = Method_DocumentLinkResolve
  fromSing SMethod_TextDocumentFormatting = Method_TextDocumentFormatting
  fromSing SMethod_TextDocumentRangeFormatting = Method_TextDocumentRangeFormatting
  fromSing SMethod_TextDocumentOnTypeFormatting = Method_TextDocumentOnTypeFormatting
  fromSing SMethod_TextDocumentRename = Method_TextDocumentRename
  fromSing SMethod_TextDocumentPrepareRename = Method_TextDocumentPrepareRename
  fromSing SMethod_WorkspaceExecuteCommand = Method_WorkspaceExecuteCommand
  fromSing SMethod_WorkspaceApplyEdit = Method_WorkspaceApplyEdit
  fromSing SMethod_WorkspaceDidChangeWorkspaceFolders = Method_WorkspaceDidChangeWorkspaceFolders
  fromSing SMethod_WindowWorkDoneProgressCancel = Method_WindowWorkDoneProgressCancel
  fromSing SMethod_WorkspaceDidCreateFiles = Method_WorkspaceDidCreateFiles
  fromSing SMethod_WorkspaceDidRenameFiles = Method_WorkspaceDidRenameFiles
  fromSing SMethod_WorkspaceDidDeleteFiles = Method_WorkspaceDidDeleteFiles
  fromSing SMethod_NotebookDocumentDidOpen = Method_NotebookDocumentDidOpen
  fromSing SMethod_NotebookDocumentDidChange = Method_NotebookDocumentDidChange
  fromSing SMethod_NotebookDocumentDidSave = Method_NotebookDocumentDidSave
  fromSing SMethod_NotebookDocumentDidClose = Method_NotebookDocumentDidClose
  fromSing SMethod_Initialized = Method_Initialized
  fromSing SMethod_Exit = Method_Exit
  fromSing SMethod_WorkspaceDidChangeConfiguration = Method_WorkspaceDidChangeConfiguration
  fromSing SMethod_WindowShowMessage = Method_WindowShowMessage
  fromSing SMethod_WindowLogMessage = Method_WindowLogMessage
  fromSing SMethod_TelemetryEvent = Method_TelemetryEvent
  fromSing SMethod_TextDocumentDidOpen = Method_TextDocumentDidOpen
  fromSing SMethod_TextDocumentDidChange = Method_TextDocumentDidChange
  fromSing SMethod_TextDocumentDidClose = Method_TextDocumentDidClose
  fromSing SMethod_TextDocumentDidSave = Method_TextDocumentDidSave
  fromSing SMethod_TextDocumentWillSave = Method_TextDocumentWillSave
  fromSing SMethod_WorkspaceDidChangeWatchedFiles = Method_WorkspaceDidChangeWatchedFiles
  fromSing SMethod_TextDocumentPublishDiagnostics = Method_TextDocumentPublishDiagnostics
  fromSing SMethod_SetTrace = Method_SetTrace
  fromSing SMethod_LogTrace = Method_LogTrace
  fromSing SMethod_CancelRequest = Method_CancelRequest
  fromSing SMethod_Progress = Method_Progress
  toSing Method_TextDocumentImplementation = S.SomeSing SMethod_TextDocumentImplementation
  toSing Method_TextDocumentTypeDefinition = S.SomeSing SMethod_TextDocumentTypeDefinition
  toSing Method_WorkspaceWorkspaceFolders = S.SomeSing SMethod_WorkspaceWorkspaceFolders
  toSing Method_WorkspaceConfiguration = S.SomeSing SMethod_WorkspaceConfiguration
  toSing Method_TextDocumentDocumentColor = S.SomeSing SMethod_TextDocumentDocumentColor
  toSing Method_TextDocumentColorPresentation = S.SomeSing SMethod_TextDocumentColorPresentation
  toSing Method_TextDocumentFoldingRange = S.SomeSing SMethod_TextDocumentFoldingRange
  toSing Method_TextDocumentDeclaration = S.SomeSing SMethod_TextDocumentDeclaration
  toSing Method_TextDocumentSelectionRange = S.SomeSing SMethod_TextDocumentSelectionRange
  toSing Method_WindowWorkDoneProgressCreate = S.SomeSing SMethod_WindowWorkDoneProgressCreate
  toSing Method_TextDocumentPrepareCallHierarchy = S.SomeSing SMethod_TextDocumentPrepareCallHierarchy
  toSing Method_CallHierarchyIncomingCalls = S.SomeSing SMethod_CallHierarchyIncomingCalls
  toSing Method_CallHierarchyOutgoingCalls = S.SomeSing SMethod_CallHierarchyOutgoingCalls
  toSing Method_TextDocumentSemanticTokensFull = S.SomeSing SMethod_TextDocumentSemanticTokensFull
  toSing Method_TextDocumentSemanticTokensFullDelta = S.SomeSing SMethod_TextDocumentSemanticTokensFullDelta
  toSing Method_TextDocumentSemanticTokensRange = S.SomeSing SMethod_TextDocumentSemanticTokensRange
  toSing Method_WorkspaceSemanticTokensRefresh = S.SomeSing SMethod_WorkspaceSemanticTokensRefresh
  toSing Method_WindowShowDocument = S.SomeSing SMethod_WindowShowDocument
  toSing Method_TextDocumentLinkedEditingRange = S.SomeSing SMethod_TextDocumentLinkedEditingRange
  toSing Method_WorkspaceWillCreateFiles = S.SomeSing SMethod_WorkspaceWillCreateFiles
  toSing Method_WorkspaceWillRenameFiles = S.SomeSing SMethod_WorkspaceWillRenameFiles
  toSing Method_WorkspaceWillDeleteFiles = S.SomeSing SMethod_WorkspaceWillDeleteFiles
  toSing Method_TextDocumentMoniker = S.SomeSing SMethod_TextDocumentMoniker
  toSing Method_TextDocumentPrepareTypeHierarchy = S.SomeSing SMethod_TextDocumentPrepareTypeHierarchy
  toSing Method_TypeHierarchySupertypes = S.SomeSing SMethod_TypeHierarchySupertypes
  toSing Method_TypeHierarchySubtypes = S.SomeSing SMethod_TypeHierarchySubtypes
  toSing Method_TextDocumentInlineValue = S.SomeSing SMethod_TextDocumentInlineValue
  toSing Method_WorkspaceInlineValueRefresh = S.SomeSing SMethod_WorkspaceInlineValueRefresh
  toSing Method_TextDocumentInlayHint = S.SomeSing SMethod_TextDocumentInlayHint
  toSing Method_InlayHintResolve = S.SomeSing SMethod_InlayHintResolve
  toSing Method_WorkspaceInlayHintRefresh = S.SomeSing SMethod_WorkspaceInlayHintRefresh
  toSing Method_TextDocumentDiagnostic = S.SomeSing SMethod_TextDocumentDiagnostic
  toSing Method_WorkspaceDiagnostic = S.SomeSing SMethod_WorkspaceDiagnostic
  toSing Method_WorkspaceDiagnosticRefresh = S.SomeSing SMethod_WorkspaceDiagnosticRefresh
  toSing Method_ClientRegisterCapability = S.SomeSing SMethod_ClientRegisterCapability
  toSing Method_ClientUnregisterCapability = S.SomeSing SMethod_ClientUnregisterCapability
  toSing Method_Initialize = S.SomeSing SMethod_Initialize
  toSing Method_Shutdown = S.SomeSing SMethod_Shutdown
  toSing Method_WindowShowMessageRequest = S.SomeSing SMethod_WindowShowMessageRequest
  toSing Method_TextDocumentWillSaveWaitUntil = S.SomeSing SMethod_TextDocumentWillSaveWaitUntil
  toSing Method_TextDocumentCompletion = S.SomeSing SMethod_TextDocumentCompletion
  toSing Method_CompletionItemResolve = S.SomeSing SMethod_CompletionItemResolve
  toSing Method_TextDocumentHover = S.SomeSing SMethod_TextDocumentHover
  toSing Method_TextDocumentSignatureHelp = S.SomeSing SMethod_TextDocumentSignatureHelp
  toSing Method_TextDocumentDefinition = S.SomeSing SMethod_TextDocumentDefinition
  toSing Method_TextDocumentReferences = S.SomeSing SMethod_TextDocumentReferences
  toSing Method_TextDocumentDocumentHighlight = S.SomeSing SMethod_TextDocumentDocumentHighlight
  toSing Method_TextDocumentDocumentSymbol = S.SomeSing SMethod_TextDocumentDocumentSymbol
  toSing Method_TextDocumentCodeAction = S.SomeSing SMethod_TextDocumentCodeAction
  toSing Method_CodeActionResolve = S.SomeSing SMethod_CodeActionResolve
  toSing Method_WorkspaceSymbol = S.SomeSing SMethod_WorkspaceSymbol
  toSing Method_WorkspaceSymbolResolve = S.SomeSing SMethod_WorkspaceSymbolResolve
  toSing Method_TextDocumentCodeLens = S.SomeSing SMethod_TextDocumentCodeLens
  toSing Method_CodeLensResolve = S.SomeSing SMethod_CodeLensResolve
  toSing Method_WorkspaceCodeLensRefresh = S.SomeSing SMethod_WorkspaceCodeLensRefresh
  toSing Method_TextDocumentDocumentLink = S.SomeSing SMethod_TextDocumentDocumentLink
  toSing Method_DocumentLinkResolve = S.SomeSing SMethod_DocumentLinkResolve
  toSing Method_TextDocumentFormatting = S.SomeSing SMethod_TextDocumentFormatting
  toSing Method_TextDocumentRangeFormatting = S.SomeSing SMethod_TextDocumentRangeFormatting
  toSing Method_TextDocumentOnTypeFormatting = S.SomeSing SMethod_TextDocumentOnTypeFormatting
  toSing Method_TextDocumentRename = S.SomeSing SMethod_TextDocumentRename
  toSing Method_TextDocumentPrepareRename = S.SomeSing SMethod_TextDocumentPrepareRename
  toSing Method_WorkspaceExecuteCommand = S.SomeSing SMethod_WorkspaceExecuteCommand
  toSing Method_WorkspaceApplyEdit = S.SomeSing SMethod_WorkspaceApplyEdit
  toSing Method_WorkspaceDidChangeWorkspaceFolders = S.SomeSing SMethod_WorkspaceDidChangeWorkspaceFolders
  toSing Method_WindowWorkDoneProgressCancel = S.SomeSing SMethod_WindowWorkDoneProgressCancel
  toSing Method_WorkspaceDidCreateFiles = S.SomeSing SMethod_WorkspaceDidCreateFiles
  toSing Method_WorkspaceDidRenameFiles = S.SomeSing SMethod_WorkspaceDidRenameFiles
  toSing Method_WorkspaceDidDeleteFiles = S.SomeSing SMethod_WorkspaceDidDeleteFiles
  toSing Method_NotebookDocumentDidOpen = S.SomeSing SMethod_NotebookDocumentDidOpen
  toSing Method_NotebookDocumentDidChange = S.SomeSing SMethod_NotebookDocumentDidChange
  toSing Method_NotebookDocumentDidSave = S.SomeSing SMethod_NotebookDocumentDidSave
  toSing Method_NotebookDocumentDidClose = S.SomeSing SMethod_NotebookDocumentDidClose
  toSing Method_Initialized = S.SomeSing SMethod_Initialized
  toSing Method_Exit = S.SomeSing SMethod_Exit
  toSing Method_WorkspaceDidChangeConfiguration = S.SomeSing SMethod_WorkspaceDidChangeConfiguration
  toSing Method_WindowShowMessage = S.SomeSing SMethod_WindowShowMessage
  toSing Method_WindowLogMessage = S.SomeSing SMethod_WindowLogMessage
  toSing Method_TelemetryEvent = S.SomeSing SMethod_TelemetryEvent
  toSing Method_TextDocumentDidOpen = S.SomeSing SMethod_TextDocumentDidOpen
  toSing Method_TextDocumentDidChange = S.SomeSing SMethod_TextDocumentDidChange
  toSing Method_TextDocumentDidClose = S.SomeSing SMethod_TextDocumentDidClose
  toSing Method_TextDocumentDidSave = S.SomeSing SMethod_TextDocumentDidSave
  toSing Method_TextDocumentWillSave = S.SomeSing SMethod_TextDocumentWillSave
  toSing Method_WorkspaceDidChangeWatchedFiles = S.SomeSing SMethod_WorkspaceDidChangeWatchedFiles
  toSing Method_TextDocumentPublishDiagnostics = S.SomeSing SMethod_TextDocumentPublishDiagnostics
  toSing Method_SetTrace = S.SomeSing SMethod_SetTrace
  toSing Method_LogTrace = S.SomeSing SMethod_LogTrace
  toSing Method_CancelRequest = S.SomeSing SMethod_CancelRequest
  toSing Method_Progress = S.SomeSing SMethod_Progress
