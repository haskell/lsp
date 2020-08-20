{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.Haskell.LSP.Types.Lens where

import           Language.Haskell.LSP.Types.Cancellation
import           Language.Haskell.LSP.Types.ClientCapabilities
import           Language.Haskell.LSP.Types.CodeAction
import           Language.Haskell.LSP.Types.CodeLens
import           Language.Haskell.LSP.Types.DocumentColor
import           Language.Haskell.LSP.Types.Command
import           Language.Haskell.LSP.Types.Completion
import           Language.Haskell.LSP.Types.Configuration
import           Language.Haskell.LSP.Types.Declaration
import           Language.Haskell.LSP.Types.Definition
import           Language.Haskell.LSP.Types.Diagnostic
import           Language.Haskell.LSP.Types.DocumentFilter
import           Language.Haskell.LSP.Types.DocumentHighlight
import           Language.Haskell.LSP.Types.DocumentLink
import           Language.Haskell.LSP.Types.FoldingRange
import           Language.Haskell.LSP.Types.Formatting
import           Language.Haskell.LSP.Types.Hover
import           Language.Haskell.LSP.Types.Implementation
import           Language.Haskell.LSP.Types.Initialize
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Progress
import           Language.Haskell.LSP.Types.Registration
import           Language.Haskell.LSP.Types.References
import           Language.Haskell.LSP.Types.Rename
import           Language.Haskell.LSP.Types.SignatureHelp
import           Language.Haskell.LSP.Types.SelectionRange
import           Language.Haskell.LSP.Types.ServerCapabilities
import           Language.Haskell.LSP.Types.DocumentSymbol
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.TypeDefinition
import           Language.Haskell.LSP.Types.Window
import           Language.Haskell.LSP.Types.WatchedFiles
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders
import           Language.Haskell.LSP.Types.WorkspaceSymbol
import           Language.Haskell.LSP.Types.Message
import           Control.Lens.TH

-- TODO: This is out of date and very unmantainable, use TH to call all these!!

-- client capabilities
makeFieldsNoPrefix ''WorkspaceEditClientCapabilities
makeFieldsNoPrefix ''DidChangeConfigurationClientCapabilities
makeFieldsNoPrefix ''ExecuteCommandClientCapabilities
makeFieldsNoPrefix ''WorkspaceClientCapabilities
makeFieldsNoPrefix ''TextDocumentSyncClientCapabilities
makeFieldsNoPrefix ''CompletionItemTagsClientCapabilities
makeFieldsNoPrefix ''CompletionItemClientCapabilities
makeFieldsNoPrefix ''CompletionItemKindClientCapabilities
makeFieldsNoPrefix ''CompletionClientCapabilities
makeFieldsNoPrefix ''HoverClientCapabilities
makeFieldsNoPrefix ''SignatureHelpSignatureInformation
makeFieldsNoPrefix ''SignatureHelpParameterInformation
makeFieldsNoPrefix ''SignatureHelpClientCapabilities
makeFieldsNoPrefix ''ReferencesClientCapabilities
makeFieldsNoPrefix ''DefinitionClientCapabilities
makeFieldsNoPrefix ''TypeDefinitionClientCapabilities
makeFieldsNoPrefix ''ImplementationClientCapabilities
makeFieldsNoPrefix ''PublishDiagnosticsClientCapabilities
makeFieldsNoPrefix ''PublishDiagnosticsTagsClientCapabilities
makeFieldsNoPrefix ''TextDocumentClientCapabilities
makeFieldsNoPrefix ''ClientCapabilities

-- ---------------------------------------------------------------------

makeFieldsNoPrefix ''CompletionOptions
makeFieldsNoPrefix ''SignatureHelpOptions
makeFieldsNoPrefix ''ExecuteCommandOptions
makeFieldsNoPrefix ''SaveOptions
makeFieldsNoPrefix ''TextDocumentSyncOptions
makeFieldsNoPrefix ''WorkspaceServerCapabilities
makeFieldsNoPrefix ''WorkspaceFoldersServerCapabilities
makeFieldsNoPrefix ''ServerCapabilities
makeFieldsNoPrefix ''Registration
makeFieldsNoPrefix ''RegistrationParams
makeFieldsNoPrefix ''TextDocumentRegistrationOptions
makeFieldsNoPrefix ''Unregistration
makeFieldsNoPrefix ''UnregistrationParams
makeFieldsNoPrefix ''DidChangeConfigurationParams
makeFieldsNoPrefix ''ConfigurationItem
makeFieldsNoPrefix ''ConfigurationParams
makeFieldsNoPrefix ''DidOpenTextDocumentParams
makeFieldsNoPrefix ''TextDocumentContentChangeEvent
makeFieldsNoPrefix ''DidChangeTextDocumentParams
makeFieldsNoPrefix ''TextDocumentChangeRegistrationOptions
makeFieldsNoPrefix ''WillSaveTextDocumentParams
makeFieldsNoPrefix ''DidSaveTextDocumentParams
makeFieldsNoPrefix ''TextDocumentSaveRegistrationOptions
makeFieldsNoPrefix ''DidCloseTextDocumentParams
makeFieldsNoPrefix ''PublishDiagnosticsParams
makeFieldsNoPrefix ''LanguageString
makeFieldsNoPrefix ''ParameterInformation
makeFieldsNoPrefix ''SignatureInformation
makeFieldsNoPrefix ''SignatureHelp
makeFieldsNoPrefix ''SignatureHelpRegistrationOptions
makeFieldsNoPrefix ''ReferenceContext
makeFieldsNoPrefix ''ReferenceParams
makeFieldsNoPrefix ''ExecuteCommandParams
makeFieldsNoPrefix ''ExecuteCommandRegistrationOptions
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

-- Completion
makeFieldsNoPrefix ''CompletionItem
makeFieldsNoPrefix ''CompletionContext
makeFieldsNoPrefix ''CompletionList
makeFieldsNoPrefix ''CompletionParams
makeFieldsNoPrefix ''CompletionRegistrationOptions

-- Declaration
makeFieldsNoPrefix ''DeclarationClientCapabilities
makeFieldsNoPrefix ''DeclarationOptions
makeFieldsNoPrefix ''DeclarationRegistrationOptions
makeFieldsNoPrefix ''DeclarationParams

-- CodeActions
makeFieldsNoPrefix ''CodeActionKindClientCapabilities
makeFieldsNoPrefix ''CodeActionLiteralSupport
makeFieldsNoPrefix ''CodeActionClientCapabilities
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
makeFieldsNoPrefix ''VersionedTextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentEdit
makeFieldsNoPrefix ''WorkspaceEdit

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

-- Command
makeFieldsNoPrefix ''Command

-- Diagnostic
makeFieldsNoPrefix ''Diagnostic
makeFieldsNoPrefix ''DiagnosticRelatedInformation

-- Hover
makeFieldsNoPrefix ''Hover
makeFieldsNoPrefix ''HoverRegistrationOptions


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
