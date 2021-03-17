{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeInType             #-}

module Language.LSP.Types.Lens where

import           Language.LSP.Types.Cancellation
import           Language.LSP.Types.ClientCapabilities
import           Language.LSP.Types.CodeAction
import           Language.LSP.Types.CodeLens
import           Language.LSP.Types.DocumentColor
import           Language.LSP.Types.Command
import           Language.LSP.Types.Completion
import           Language.LSP.Types.Configuration
import           Language.LSP.Types.Declaration
import           Language.LSP.Types.Definition
import           Language.LSP.Types.Diagnostic
import           Language.LSP.Types.DocumentFilter
import           Language.LSP.Types.DocumentHighlight
import           Language.LSP.Types.DocumentLink
import           Language.LSP.Types.FoldingRange
import           Language.LSP.Types.Formatting
import           Language.LSP.Types.Hover
import           Language.LSP.Types.Implementation
import           Language.LSP.Types.Initialize
import           Language.LSP.Types.Location
import           Language.LSP.Types.MarkupContent
import           Language.LSP.Types.Progress
import           Language.LSP.Types.Registration
import           Language.LSP.Types.References
import           Language.LSP.Types.Rename
import           Language.LSP.Types.SignatureHelp
import           Language.LSP.Types.SelectionRange
import           Language.LSP.Types.ServerCapabilities
import           Language.LSP.Types.StaticRegistrationOptions
import           Language.LSP.Types.DocumentSymbol
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.TypeDefinition
import           Language.LSP.Types.Window
import           Language.LSP.Types.WatchedFiles
import           Language.LSP.Types.WorkspaceEdit
import           Language.LSP.Types.WorkspaceFolders
import           Language.LSP.Types.WorkspaceSymbol
import           Language.LSP.Types.Message
import           Control.Lens.TH

-- TODO: This is out of date and very unmantainable, use TH to call all these!!

-- client capabilities
makeFieldsNoPrefix ''WorkspaceClientCapabilities
makeFieldsNoPrefix ''WindowClientCapabilities
makeFieldsNoPrefix ''ClientCapabilities

-- ---------------------------------------------------------------------

makeFieldsNoPrefix ''SaveOptions
makeFieldsNoPrefix ''WorkspaceServerCapabilities
makeFieldsNoPrefix ''WorkspaceFoldersServerCapabilities
makeFieldsNoPrefix ''ServerCapabilities
makeFieldsNoPrefix ''Registration
makeFieldsNoPrefix ''RegistrationParams
makeFieldsNoPrefix ''Unregistration
makeFieldsNoPrefix ''UnregistrationParams
makeFieldsNoPrefix ''ParameterInformation
makeFieldsNoPrefix ''SignatureInformation
makeFieldsNoPrefix ''ApplyWorkspaceEditParams
makeFieldsNoPrefix ''ApplyWorkspaceEditResponseBody

-- ---------------------------------------------------------------------

-- Initialize
makeFieldsNoPrefix ''InitializeParams
makeFieldsNoPrefix ''InitializeError
makeFieldsNoPrefix ''InitializeResult
makeFieldsNoPrefix ''ClientInfo
makeFieldsNoPrefix ''ServerInfo
makeFieldsNoPrefix ''InitializedParams

-- Configuration
makeFieldsNoPrefix ''DidChangeConfigurationParams
makeFieldsNoPrefix ''ConfigurationItem
makeFieldsNoPrefix ''ConfigurationParams
makeFieldsNoPrefix ''DidChangeConfigurationClientCapabilities

-- Watched files
makeFieldsNoPrefix ''DidChangeWatchedFilesClientCapabilities
makeFieldsNoPrefix ''DidChangeWatchedFilesRegistrationOptions
makeFieldsNoPrefix ''FileSystemWatcher
makeFieldsNoPrefix ''WatchKind
makeFieldsNoPrefix ''FileEvent
makeFieldsNoPrefix ''DidChangeWatchedFilesParams

-- Workspace symbols
makeFieldsNoPrefix ''WorkspaceSymbolKindClientCapabilities
makeFieldsNoPrefix ''WorkspaceSymbolClientCapabilities
makeFieldsNoPrefix ''WorkspaceSymbolOptions
makeFieldsNoPrefix ''WorkspaceSymbolRegistrationOptions
makeFieldsNoPrefix ''WorkspaceSymbolParams

-- Location
makeFieldsNoPrefix ''Position
makeFieldsNoPrefix ''Range
makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''LocationLink

-- Markup
makeFieldsNoPrefix ''MarkupContent

-- Completion
makeFieldsNoPrefix ''InsertReplaceEdit
makeFieldsNoPrefix ''CompletionItem
makeFieldsNoPrefix ''CompletionContext
makeFieldsNoPrefix ''CompletionList
makeFieldsNoPrefix ''CompletionParams
makeFieldsNoPrefix ''CompletionOptions
makeFieldsNoPrefix ''CompletionRegistrationOptions
makeFieldsNoPrefix ''CompletionItemTagsClientCapabilities
makeFieldsNoPrefix ''CompletionItemResolveClientCapabilities
makeFieldsNoPrefix ''CompletionItemInsertTextModeClientCapabilities
makeFieldsNoPrefix ''CompletionItemClientCapabilities
makeFieldsNoPrefix ''CompletionItemKindClientCapabilities
makeFieldsNoPrefix ''CompletionClientCapabilities

-- Declaration
makeFieldsNoPrefix ''DeclarationClientCapabilities
makeFieldsNoPrefix ''DeclarationOptions
makeFieldsNoPrefix ''DeclarationRegistrationOptions
makeFieldsNoPrefix ''DeclarationParams

-- CodeActions
makeFieldsNoPrefix ''CodeActionKindClientCapabilities
makeFieldsNoPrefix ''CodeActionLiteralSupport
makeFieldsNoPrefix ''CodeActionClientCapabilities
makeFieldsNoPrefix ''CodeActionResolveClientCapabilities
makeFieldsNoPrefix ''CodeActionOptions
makeFieldsNoPrefix ''CodeActionRegistrationOptions
makeFieldsNoPrefix ''CodeActionContext
makeFieldsNoPrefix ''CodeActionParams
makeFieldsNoPrefix ''CodeAction

-- CodeLens
makeFieldsNoPrefix ''CodeLensClientCapabilities
makeFieldsNoPrefix ''CodeLensOptions
makeFieldsNoPrefix ''CodeLensRegistrationOptions
makeFieldsNoPrefix ''CodeLensParams
makeFieldsNoPrefix ''CodeLens

-- DocumentLink
makeFieldsNoPrefix ''DocumentLinkClientCapabilities
makeFieldsNoPrefix ''DocumentLinkOptions
makeFieldsNoPrefix ''DocumentLinkRegistrationOptions
makeFieldsNoPrefix ''DocumentLinkParams
makeFieldsNoPrefix ''DocumentLink

-- DocumentColor
makeFieldsNoPrefix ''DocumentColorClientCapabilities
makeFieldsNoPrefix ''DocumentColorOptions
makeFieldsNoPrefix ''DocumentColorRegistrationOptions
makeFieldsNoPrefix ''DocumentColorParams
makeFieldsNoPrefix ''Color
makeFieldsNoPrefix ''ColorInformation

-- ColorPresentation
makeFieldsNoPrefix ''ColorPresentationParams
makeFieldsNoPrefix ''ColorPresentation

-- Formatting
makeFieldsNoPrefix ''DocumentFormattingClientCapabilities
makeFieldsNoPrefix ''DocumentFormattingOptions
makeFieldsNoPrefix ''DocumentFormattingRegistrationOptions
makeFieldsNoPrefix ''FormattingOptions
makeFieldsNoPrefix ''DocumentFormattingParams

-- RangeFormatting
makeFieldsNoPrefix ''DocumentRangeFormattingClientCapabilities
makeFieldsNoPrefix ''DocumentRangeFormattingOptions
makeFieldsNoPrefix ''DocumentRangeFormattingRegistrationOptions
makeFieldsNoPrefix ''DocumentRangeFormattingParams

-- OnTypeFormatting
makeFieldsNoPrefix ''DocumentOnTypeFormattingClientCapabilities
makeFieldsNoPrefix ''DocumentOnTypeFormattingOptions
makeFieldsNoPrefix ''DocumentOnTypeFormattingRegistrationOptions
makeFieldsNoPrefix ''DocumentOnTypeFormattingParams

-- Rename
makeFieldsNoPrefix ''RenameClientCapabilities
makeFieldsNoPrefix ''RenameOptions
makeFieldsNoPrefix ''RenameRegistrationOptions
makeFieldsNoPrefix ''RenameParams

-- PrepareRename
makeFieldsNoPrefix ''PrepareRenameParams
makeFieldsNoPrefix ''RangeWithPlaceholder

-- References
makeFieldsNoPrefix ''ReferencesClientCapabilities
makeFieldsNoPrefix ''ReferenceOptions
makeFieldsNoPrefix ''ReferenceRegistrationOptions
makeFieldsNoPrefix ''ReferenceContext
makeFieldsNoPrefix ''ReferenceParams

-- FoldingRange
makeFieldsNoPrefix ''FoldingRangeClientCapabilities
makeFieldsNoPrefix ''FoldingRangeOptions
makeFieldsNoPrefix ''FoldingRangeRegistrationOptions
makeFieldsNoPrefix ''FoldingRangeParams
makeFieldsNoPrefix ''FoldingRange

-- SelectionRange
makeFieldsNoPrefix ''SelectionRangeClientCapabilities
makeFieldsNoPrefix ''SelectionRangeOptions
makeFieldsNoPrefix ''SelectionRangeRegistrationOptions
makeFieldsNoPrefix ''SelectionRangeParams
makeFieldsNoPrefix ''SelectionRange

-- DocumentHighlight
makeFieldsNoPrefix ''DocumentHighlightClientCapabilities
makeFieldsNoPrefix ''DocumentHighlightOptions
makeFieldsNoPrefix ''DocumentHighlightRegistrationOptions
makeFieldsNoPrefix ''DocumentHighlightParams
makeFieldsNoPrefix ''DocumentHighlight

-- DocumentSymbol
makeFieldsNoPrefix ''DocumentSymbolKindClientCapabilities
makeFieldsNoPrefix ''DocumentSymbolClientCapabilities
makeFieldsNoPrefix ''DocumentSymbolOptions
makeFieldsNoPrefix ''DocumentSymbolRegistrationOptions
makeFieldsNoPrefix ''DocumentSymbolParams
makeFieldsNoPrefix ''DocumentSymbol
makeFieldsNoPrefix ''SymbolInformation

-- DocumentFilter
makeFieldsNoPrefix ''DocumentFilter

-- WorkspaceEdit
makeFieldsNoPrefix ''TextEdit
makeFieldsNoPrefix ''ChangeAnnotation
makeFieldsNoPrefix ''AnnotatedTextEdit
makeFieldsNoPrefix ''VersionedTextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentEdit
makeFieldsNoPrefix ''CreateFileOptions
makeFieldsNoPrefix ''CreateFile
makeFieldsNoPrefix ''RenameFileOptions
makeFieldsNoPrefix ''RenameFile
makeFieldsNoPrefix ''DeleteFileOptions
makeFieldsNoPrefix ''DeleteFile
makeFieldsNoPrefix ''WorkspaceEdit
makeFieldsNoPrefix ''WorkspaceEditClientCapabilities
makeFieldsNoPrefix ''WorkspaceEditChangeAnnotationClientCapabilities

-- Workspace Folders
makeFieldsNoPrefix ''WorkspaceFolder
makeFieldsNoPrefix ''WorkspaceFoldersChangeEvent
makeFieldsNoPrefix ''DidChangeWorkspaceFoldersParams

-- Message
makeFieldsNoPrefix ''RequestMessage
makeFieldsNoPrefix ''ResponseError
makeFieldsNoPrefix ''ResponseMessage
makeFieldsNoPrefix ''NotificationMessage
makeFieldsNoPrefix ''CancelParams

-- TextDocument
makeFieldsNoPrefix ''TextDocumentItem
makeFieldsNoPrefix ''TextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentPositionParams
makeFieldsNoPrefix ''TextDocumentSyncClientCapabilities
makeFieldsNoPrefix ''TextDocumentClientCapabilities
makeFieldsNoPrefix ''TextDocumentRegistrationOptions
makeFieldsNoPrefix ''TextDocumentSyncOptions
makeFieldsNoPrefix ''DidOpenTextDocumentParams
makeFieldsNoPrefix ''TextDocumentContentChangeEvent
makeFieldsNoPrefix ''DidChangeTextDocumentParams
makeFieldsNoPrefix ''TextDocumentChangeRegistrationOptions
makeFieldsNoPrefix ''WillSaveTextDocumentParams
makeFieldsNoPrefix ''DidSaveTextDocumentParams
makeFieldsNoPrefix ''TextDocumentSaveRegistrationOptions
makeFieldsNoPrefix ''DidCloseTextDocumentParams

-- Command
makeFieldsNoPrefix ''Command
makeFieldsNoPrefix ''ExecuteCommandParams
makeFieldsNoPrefix ''ExecuteCommandRegistrationOptions
makeFieldsNoPrefix ''ExecuteCommandClientCapabilities
makeFieldsNoPrefix ''ExecuteCommandOptions

-- Diagnostic
makeFieldsNoPrefix ''DiagnosticRelatedInformation
makeFieldsNoPrefix ''Diagnostic
makeFieldsNoPrefix ''PublishDiagnosticsTagsClientCapabilities
makeFieldsNoPrefix ''PublishDiagnosticsClientCapabilities
makeFieldsNoPrefix ''PublishDiagnosticsParams

-- Hover
makeFieldsNoPrefix ''HoverClientCapabilities
makeFieldsNoPrefix ''Hover
makeFieldsNoPrefix ''HoverParams
makeFieldsNoPrefix ''HoverOptions
makeFieldsNoPrefix ''HoverRegistrationOptions
makeFieldsNoPrefix ''LanguageString

-- Implementation
makeFieldsNoPrefix ''ImplementationClientCapabilities
makeFieldsNoPrefix ''ImplementationOptions
makeFieldsNoPrefix ''ImplementationRegistrationOptions
makeFieldsNoPrefix ''ImplementationParams

-- Definition
makeFieldsNoPrefix ''DefinitionOptions
makeFieldsNoPrefix ''DefinitionRegistrationOptions
makeFieldsNoPrefix ''DefinitionParams
makeFieldsNoPrefix ''DefinitionClientCapabilities

-- Type Definition
makeFieldsNoPrefix ''TypeDefinitionOptions
makeFieldsNoPrefix ''TypeDefinitionRegistrationOptions
makeFieldsNoPrefix ''TypeDefinitionParams
makeFieldsNoPrefix ''TypeDefinitionClientCapabilities

-- Window
makeFieldsNoPrefix ''ShowMessageParams
makeFieldsNoPrefix ''MessageActionItem
makeFieldsNoPrefix ''ShowMessageRequestParams
makeFieldsNoPrefix ''LogMessageParams
makeFieldsNoPrefix ''ProgressParams
makeFieldsNoPrefix ''WorkDoneProgressBeginParams
makeFieldsNoPrefix ''WorkDoneProgressReportParams
makeFieldsNoPrefix ''WorkDoneProgressEndParams
makeFieldsNoPrefix ''WorkDoneProgressCancelParams
makeFieldsNoPrefix ''WorkDoneProgressCreateParams
makeFieldsNoPrefix ''WorkDoneProgressOptions
makeFieldsNoPrefix ''WorkDoneProgressParams
makeFieldsNoPrefix ''PartialResultParams

-- Signature Help
makeFieldsNoPrefix ''SignatureHelpSignatureInformation
makeFieldsNoPrefix ''SignatureHelpParameterInformation
makeFieldsNoPrefix ''SignatureHelpClientCapabilities
makeFieldsNoPrefix ''SignatureHelpOptions
makeFieldsNoPrefix ''SignatureHelpRegistrationOptions
makeFieldsNoPrefix ''SignatureHelp

-- Static registration
makeFieldsNoPrefix ''StaticRegistrationOptions
