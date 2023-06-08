-- THIS IS A GENERATED FILE, DO NOT EDIT
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Lens where

import qualified Control.Lens.TH
import qualified Language.LSP.Protocol.Internal.Types.AnnotatedTextEdit
import qualified Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams
import qualified Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditResult
import qualified Language.LSP.Protocol.Internal.Types.BaseSymbolInformation
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCall
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCallsParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOptions
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCall
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCallsParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyPrepareParams
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CancelParams
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotation
import qualified Language.LSP.Protocol.Internal.Types.ClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CodeAction
import qualified Language.LSP.Protocol.Internal.Types.CodeActionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CodeActionContext
import qualified Language.LSP.Protocol.Internal.Types.CodeActionOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeActionParams
import qualified Language.LSP.Protocol.Internal.Types.CodeActionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeDescription
import qualified Language.LSP.Protocol.Internal.Types.CodeLens
import qualified Language.LSP.Protocol.Internal.Types.CodeLensClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CodeLensOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeLensParams
import qualified Language.LSP.Protocol.Internal.Types.CodeLensRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.Color
import qualified Language.LSP.Protocol.Internal.Types.ColorInformation
import qualified Language.LSP.Protocol.Internal.Types.ColorPresentation
import qualified Language.LSP.Protocol.Internal.Types.ColorPresentationParams
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.CompletionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CompletionContext
import qualified Language.LSP.Protocol.Internal.Types.CompletionItem
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemLabelDetails
import qualified Language.LSP.Protocol.Internal.Types.CompletionList
import qualified Language.LSP.Protocol.Internal.Types.CompletionOptions
import qualified Language.LSP.Protocol.Internal.Types.CompletionParams
import qualified Language.LSP.Protocol.Internal.Types.CompletionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ConfigurationItem
import qualified Language.LSP.Protocol.Internal.Types.ConfigurationParams
import qualified Language.LSP.Protocol.Internal.Types.CreateFile
import qualified Language.LSP.Protocol.Internal.Types.CreateFileOptions
import qualified Language.LSP.Protocol.Internal.Types.CreateFilesParams
import qualified Language.LSP.Protocol.Internal.Types.DeclarationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DeclarationOptions
import qualified Language.LSP.Protocol.Internal.Types.DeclarationParams
import qualified Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DefinitionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DefinitionOptions
import qualified Language.LSP.Protocol.Internal.Types.DefinitionParams
import qualified Language.LSP.Protocol.Internal.Types.DefinitionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DeleteFile
import qualified Language.LSP.Protocol.Internal.Types.DeleteFileOptions
import qualified Language.LSP.Protocol.Internal.Types.DeleteFilesParams
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticOptions
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticRelatedInformation
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DidChangeNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesParams
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWorkspaceFoldersParams
import qualified Language.LSP.Protocol.Internal.Types.DidCloseNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidCloseTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidOpenNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidOpenTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidSaveNotebookDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DidSaveTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportPartialResult
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlight
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentLink
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbol
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolOptions
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolParams
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandOptions
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandParams
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ExecutionSummary
import qualified Language.LSP.Protocol.Internal.Types.FileCreate
import qualified Language.LSP.Protocol.Internal.Types.FileDelete
import qualified Language.LSP.Protocol.Internal.Types.FileEvent
import qualified Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.FileOperationFilter
import qualified Language.LSP.Protocol.Internal.Types.FileOperationOptions
import qualified Language.LSP.Protocol.Internal.Types.FileOperationPattern
import qualified Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions
import qualified Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.FileRename
import qualified Language.LSP.Protocol.Internal.Types.FileSystemWatcher
import qualified Language.LSP.Protocol.Internal.Types.FoldingRange
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeParams
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.FormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.Hover
import qualified Language.LSP.Protocol.Internal.Types.HoverClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.HoverOptions
import qualified Language.LSP.Protocol.Internal.Types.HoverParams
import qualified Language.LSP.Protocol.Internal.Types.HoverRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ImplementationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ImplementationOptions
import qualified Language.LSP.Protocol.Internal.Types.ImplementationParams
import qualified Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InitializeError
import qualified Language.LSP.Protocol.Internal.Types.InitializeParams
import qualified Language.LSP.Protocol.Internal.Types.InitializeResult
import qualified Language.LSP.Protocol.Internal.Types.InitializedParams
import qualified Language.LSP.Protocol.Internal.Types.InlayHint
import qualified Language.LSP.Protocol.Internal.Types.InlayHintClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlayHintLabelPart
import qualified Language.LSP.Protocol.Internal.Types.InlayHintOptions
import qualified Language.LSP.Protocol.Internal.Types.InlayHintParams
import qualified Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InlayHintWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlineValueClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlineValueContext
import qualified Language.LSP.Protocol.Internal.Types.InlineValueEvaluatableExpression
import qualified Language.LSP.Protocol.Internal.Types.InlineValueOptions
import qualified Language.LSP.Protocol.Internal.Types.InlineValueParams
import qualified Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.InlineValueText
import qualified Language.LSP.Protocol.Internal.Types.InlineValueVariableLookup
import qualified Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InsertReplaceEdit
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeParams
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRanges
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Internal.Types.LocationLink
import qualified Language.LSP.Protocol.Internal.Types.LogMessageParams
import qualified Language.LSP.Protocol.Internal.Types.LogTraceParams
import qualified Language.LSP.Protocol.Internal.Types.MarkdownClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Internal.Types.MessageActionItem
import qualified Language.LSP.Protocol.Internal.Types.Moniker
import qualified Language.LSP.Protocol.Internal.Types.MonikerClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.MonikerOptions
import qualified Language.LSP.Protocol.Internal.Types.MonikerParams
import qualified Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.NotebookCell
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellTextDocumentFilter
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocument
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.OptionalVersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.ParameterInformation
import qualified Language.LSP.Protocol.Internal.Types.PartialResultParams
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Internal.Types.PrepareRenameParams
import qualified Language.LSP.Protocol.Internal.Types.PreviousResultId
import qualified Language.LSP.Protocol.Internal.Types.ProgressParams
import qualified Language.LSP.Protocol.Internal.Types.PublishDiagnosticsClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.PublishDiagnosticsParams
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.ReferenceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ReferenceContext
import qualified Language.LSP.Protocol.Internal.Types.ReferenceOptions
import qualified Language.LSP.Protocol.Internal.Types.ReferenceParams
import qualified Language.LSP.Protocol.Internal.Types.ReferenceRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.Registration
import qualified Language.LSP.Protocol.Internal.Types.RegistrationParams
import qualified Language.LSP.Protocol.Internal.Types.RegularExpressionsClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.RelatedFullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.RelatedUnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.RelativePattern
import qualified Language.LSP.Protocol.Internal.Types.RenameClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.RenameFile
import qualified Language.LSP.Protocol.Internal.Types.RenameFileOptions
import qualified Language.LSP.Protocol.Internal.Types.RenameFilesParams
import qualified Language.LSP.Protocol.Internal.Types.RenameOptions
import qualified Language.LSP.Protocol.Internal.Types.RenameParams
import qualified Language.LSP.Protocol.Internal.Types.RenameRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.ResourceOperation
import qualified Language.LSP.Protocol.Internal.Types.SaveOptions
import qualified Language.LSP.Protocol.Internal.Types.SelectionRange
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeOptions
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeParams
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokens
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensDelta
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaPartialResult
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensEdit
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensPartialResult
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensRangeParams
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ServerCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SetTraceParams
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentResult
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageParams
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageRequestClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageRequestParams
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelp
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpContext
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpOptions
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpParams
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SignatureInformation
import qualified Language.LSP.Protocol.Internal.Types.StaticRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.SymbolInformation
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentEdit
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentItem
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentPositionParams
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSaveRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncOptions
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionParams
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyPrepareParams
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchySubtypesParams
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchySupertypesParams
import qualified Language.LSP.Protocol.Internal.Types.UInitializeParams
import qualified Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.Unregistration
import qualified Language.LSP.Protocol.Internal.Types.UnregistrationParams
import qualified Language.LSP.Protocol.Internal.Types.VersionedNotebookDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams
import qualified Language.LSP.Protocol.Internal.Types.WindowClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressBegin
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressCancelParams
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressCreateParams
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressEnd
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressOptions
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressParams
import qualified Language.LSP.Protocol.Internal.Types.WorkDoneProgressReport
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReportPartialResult
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEdit
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersInitializeParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbol
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolOptions
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolParams
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolRegistrationOptions
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceUnchangedDocumentDiagnosticReport

Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ImplementationParams.ImplementationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Location.Location
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ImplementationRegistrationOptions.ImplementationRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeDefinitionParams.TypeDefinitionParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeDefinitionRegistrationOptions.TypeDefinitionRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeWorkspaceFoldersParams.DidChangeWorkspaceFoldersParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ConfigurationParams.ConfigurationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentColorParams.DocumentColorParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ColorInformation.ColorInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentColorRegistrationOptions.DocumentColorRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ColorPresentationParams.ColorPresentationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ColorPresentation.ColorPresentation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressOptions.WorkDoneProgressOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions.TextDocumentRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FoldingRangeParams.FoldingRangeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FoldingRange.FoldingRange
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FoldingRangeRegistrationOptions.FoldingRangeRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeclarationParams.DeclarationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions.DeclarationRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SelectionRangeParams.SelectionRangeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SelectionRange.SelectionRange
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions.SelectionRangeRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressCreateParams.WorkDoneProgressCreateParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressCancelParams.WorkDoneProgressCancelParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyPrepareParams.CallHierarchyPrepareParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyItem.CallHierarchyItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyRegistrationOptions.CallHierarchyRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCallsParams.CallHierarchyIncomingCallsParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCall.CallHierarchyIncomingCall
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCallsParams.CallHierarchyOutgoingCallsParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCall.CallHierarchyOutgoingCall
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensParams.SemanticTokensParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokens.SemanticTokens
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensPartialResult.SemanticTokensPartialResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions.SemanticTokensRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaParams.SemanticTokensDeltaParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensDelta.SemanticTokensDelta
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaPartialResult.SemanticTokensDeltaPartialResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensRangeParams.SemanticTokensRangeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowDocumentParams.ShowDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowDocumentResult.ShowDocumentResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LinkedEditingRangeParams.LinkedEditingRangeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LinkedEditingRanges.LinkedEditingRanges
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions.LinkedEditingRangeRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CreateFilesParams.CreateFilesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameFilesParams.RenameFilesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeleteFilesParams.DeleteFilesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MonikerParams.MonikerParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Moniker.Moniker
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions.MonikerRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchyPrepareParams.TypeHierarchyPrepareParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchyItem.TypeHierarchyItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchyRegistrationOptions.TypeHierarchyRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchySupertypesParams.TypeHierarchySupertypesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchySubtypesParams.TypeHierarchySubtypesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueParams.InlineValueParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueRegistrationOptions.InlineValueRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintParams.InlayHintParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHint.InlayHint
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions.InlayHintRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentDiagnosticParams.DocumentDiagnosticParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportPartialResult.DocumentDiagnosticReportPartialResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData.DiagnosticServerCancellationData
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions.DiagnosticRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticParams.WorkspaceDiagnosticParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReport.WorkspaceDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReportPartialResult.WorkspaceDiagnosticReportPartialResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidOpenNotebookDocumentParams.DidOpenNotebookDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeNotebookDocumentParams.DidChangeNotebookDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidSaveNotebookDocumentParams.DidSaveNotebookDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidCloseNotebookDocumentParams.DidCloseNotebookDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RegistrationParams.RegistrationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.UnregistrationParams.UnregistrationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InitializeParams.InitializeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InitializeResult.InitializeResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InitializeError.InitializeError
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InitializedParams.InitializedParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeConfigurationParams.DidChangeConfigurationParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions.DidChangeConfigurationRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowMessageParams.ShowMessageParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowMessageRequestParams.ShowMessageRequestParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MessageActionItem.MessageActionItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LogMessageParams.LogMessageParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidOpenTextDocumentParams.DidOpenTextDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeTextDocumentParams.DidChangeTextDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions.TextDocumentChangeRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidCloseTextDocumentParams.DidCloseTextDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidSaveTextDocumentParams.DidSaveTextDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentSaveRegistrationOptions.TextDocumentSaveRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams.WillSaveTextDocumentParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesParams.DidChangeWatchedFilesParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions.DidChangeWatchedFilesRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.PublishDiagnosticsParams.PublishDiagnosticsParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionParams.CompletionParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionList.CompletionList
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionRegistrationOptions.CompletionRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.HoverParams.HoverParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Hover.Hover
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.HoverRegistrationOptions.HoverRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelpParams.SignatureHelpParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelp.SignatureHelp
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelpRegistrationOptions.SignatureHelpRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DefinitionParams.DefinitionParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DefinitionRegistrationOptions.DefinitionRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ReferenceParams.ReferenceParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ReferenceRegistrationOptions.ReferenceRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentHighlightParams.DocumentHighlightParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentHighlight.DocumentHighlight
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentHighlightRegistrationOptions.DocumentHighlightRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentSymbolParams.DocumentSymbolParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SymbolInformation.SymbolInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentSymbol.DocumentSymbol
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentSymbolRegistrationOptions.DocumentSymbolRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeActionParams.CodeActionParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Command.Command
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeAction.CodeAction
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeActionRegistrationOptions.CodeActionRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceSymbolParams.WorkspaceSymbolParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceSymbol.WorkspaceSymbol
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceSymbolRegistrationOptions.WorkspaceSymbolRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLensParams.CodeLensParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLens.CodeLens
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLensRegistrationOptions.CodeLensRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentLinkParams.DocumentLinkParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentLink.DocumentLink
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentLinkRegistrationOptions.DocumentLinkRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentFormattingParams.DocumentFormattingParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentFormattingRegistrationOptions.DocumentFormattingRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams.DocumentRangeFormattingParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingRegistrationOptions.DocumentRangeFormattingRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingParams.DocumentOnTypeFormattingParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingRegistrationOptions.DocumentOnTypeFormattingRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameParams.RenameParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameRegistrationOptions.RenameRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.PrepareRenameParams.PrepareRenameParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ExecuteCommandParams.ExecuteCommandParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ExecuteCommandRegistrationOptions.ExecuteCommandRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams.ApplyWorkspaceEditParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditResult.ApplyWorkspaceEditResult
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressBegin.WorkDoneProgressBegin
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressReport.WorkDoneProgressReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressEnd.WorkDoneProgressEnd
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SetTraceParams.SetTraceParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LogTraceParams.LogTraceParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CancelParams.CancelParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ProgressParams.ProgressParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentPositionParams.TextDocumentPositionParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkDoneProgressParams.WorkDoneProgressParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.PartialResultParams.PartialResultParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LocationLink.LocationLink
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Range.Range
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ImplementationOptions.ImplementationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.StaticRegistrationOptions.StaticRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeDefinitionOptions.TypeDefinitionOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent.WorkspaceFoldersChangeEvent
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ConfigurationItem.ConfigurationItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Color.Color
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentColorOptions.DocumentColorOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FoldingRangeOptions.FoldingRangeOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeclarationOptions.DeclarationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Position.Position
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SelectionRangeOptions.SelectionRangeOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyOptions.CallHierarchyOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensOptions.SemanticTokensOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensEdit.SemanticTokensEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LinkedEditingRangeOptions.LinkedEditingRangeOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileCreate.FileCreate
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentEdit.TextDocumentEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CreateFile.CreateFile
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameFile.RenameFile
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeleteFile.DeleteFile
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ChangeAnnotation.ChangeAnnotation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationFilter.FileOperationFilter
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileRename.FileRename
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileDelete.FileDelete
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MonikerOptions.MonikerOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions.TypeHierarchyOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueContext.InlineValueContext
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueText.InlineValueText
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueVariableLookup.InlineValueVariableLookup
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueEvaluatableExpression.InlineValueEvaluatableExpression
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueOptions.InlineValueOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintLabelPart.InlayHintLabelPart
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintOptions.InlayHintOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RelatedFullDocumentDiagnosticReport.RelatedFullDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RelatedUnchangedDocumentDiagnosticReport.RelatedUnchangedDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport.FullDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport.UnchangedDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticOptions.DiagnosticOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.PreviousResultId.PreviousResultId
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocument.NotebookDocument
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentItem.TextDocumentItem
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.VersionedNotebookDocumentIdentifier.VersionedNotebookDocumentIdentifier
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent.NotebookDocumentChangeEvent
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier.NotebookDocumentIdentifier
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Registration.Registration
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Unregistration.Unregistration
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.UInitializeParams.UInitializeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceFoldersInitializeParams.WorkspaceFoldersInitializeParams
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ServerCapabilities.ServerCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier.VersionedTextDocumentIdentifier
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SaveOptions.SaveOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileEvent.FileEvent
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileSystemWatcher.FileSystemWatcher
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionContext.CompletionContext
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionItemLabelDetails.CompletionItemLabelDetails
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InsertReplaceEdit.InsertReplaceEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionOptions.CompletionOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.HoverOptions.HoverOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelpContext.SignatureHelpContext
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureInformation.SignatureInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelpOptions.SignatureHelpOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DefinitionOptions.DefinitionOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ReferenceContext.ReferenceContext
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ReferenceOptions.ReferenceOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentHighlightOptions.DocumentHighlightOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.BaseSymbolInformation.BaseSymbolInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentSymbolOptions.DocumentSymbolOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeActionContext.CodeActionContext
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeActionOptions.CodeActionOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceSymbolOptions.WorkspaceSymbolOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLensOptions.CodeLensOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentLinkOptions.DocumentLinkOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FormattingOptions.FormattingOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentFormattingOptions.DocumentFormattingOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingOptions.DocumentRangeFormattingOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingOptions.DocumentOnTypeFormattingOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameOptions.RenameOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ExecuteCommandOptions.ExecuteCommandOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensLegend.SemanticTokensLegend
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.OptionalVersionedTextDocumentIdentifier.OptionalVersionedTextDocumentIdentifier
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.AnnotatedTextEdit.AnnotatedTextEdit
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ResourceOperation.ResourceOperation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CreateFileOptions.CreateFileOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameFileOptions.RenameFileOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeleteFileOptions.DeleteFileOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationPattern.FileOperationPattern
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceFullDocumentDiagnosticReport.WorkspaceFullDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceUnchangedDocumentDiagnosticReport.WorkspaceUnchangedDocumentDiagnosticReport
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookCell.NotebookCell
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange.NotebookCellArrayChange
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ClientCapabilities.ClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentSyncOptions.TextDocumentSyncOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions.NotebookDocumentSyncOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions.NotebookDocumentSyncRegistrationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities.WorkspaceFoldersServerCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationOptions.FileOperationOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeDescription.CodeDescription
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticRelatedInformation.DiagnosticRelatedInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ParameterInformation.ParameterInformation
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookCellTextDocumentFilter.NotebookCellTextDocumentFilter
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions.FileOperationPatternOptions
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ExecutionSummary.ExecutionSummary
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities.WorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities.TextDocumentClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities.NotebookDocumentClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WindowClientCapabilities.WindowClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities.GeneralClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RelativePattern.RelativePattern
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities.WorkspaceEditClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities.DidChangeConfigurationClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities.DidChangeWatchedFilesClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities.WorkspaceSymbolClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities.ExecuteCommandClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensWorkspaceClientCapabilities.SemanticTokensWorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities.CodeLensWorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities.FileOperationClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities.InlineValueWorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintWorkspaceClientCapabilities.InlayHintWorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities.DiagnosticWorkspaceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities.TextDocumentSyncClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CompletionClientCapabilities.CompletionClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.HoverClientCapabilities.HoverClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities.SignatureHelpClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DeclarationClientCapabilities.DeclarationClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DefinitionClientCapabilities.DefinitionClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeDefinitionClientCapabilities.TypeDefinitionClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ImplementationClientCapabilities.ImplementationClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ReferenceClientCapabilities.ReferenceClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentHighlightClientCapabilities.DocumentHighlightClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentSymbolClientCapabilities.DocumentSymbolClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeActionClientCapabilities.CodeActionClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CodeLensClientCapabilities.CodeLensClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentLinkClientCapabilities.DocumentLinkClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentColorClientCapabilities.DocumentColorClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentFormattingClientCapabilities.DocumentFormattingClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingClientCapabilities.DocumentRangeFormattingClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingClientCapabilities.DocumentOnTypeFormattingClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RenameClientCapabilities.RenameClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.FoldingRangeClientCapabilities.FoldingRangeClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SelectionRangeClientCapabilities.SelectionRangeClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.PublishDiagnosticsClientCapabilities.PublishDiagnosticsClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.CallHierarchyClientCapabilities.CallHierarchyClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.SemanticTokensClientCapabilities.SemanticTokensClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.LinkedEditingRangeClientCapabilities.LinkedEditingRangeClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MonikerClientCapabilities.MonikerClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.TypeHierarchyClientCapabilities.TypeHierarchyClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlineValueClientCapabilities.InlineValueClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.InlayHintClientCapabilities.InlayHintClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.DiagnosticClientCapabilities.DiagnosticClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncClientCapabilities.NotebookDocumentSyncClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowMessageRequestClientCapabilities.ShowMessageRequestClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.ShowDocumentClientCapabilities.ShowDocumentClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.RegularExpressionsClientCapabilities.RegularExpressionsClientCapabilities
Control.Lens.TH.makeFieldsNoPrefix ''Language.LSP.Protocol.Internal.Types.MarkdownClientCapabilities.MarkdownClientCapabilities
