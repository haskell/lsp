{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Haskell.LSP.Types.Lens where
import Language.Haskell.LSP.Types.DataTypesJSON
import Language.Haskell.LSP.Types.ClientCapabilities
import Control.Lens.TH

-- client capabilities
makeFieldsNoPrefix ''WorkspaceEditClientCapabilities
makeFieldsNoPrefix ''DidChangeConfigurationClientCapabilities
makeFieldsNoPrefix ''DidChangeWatchedFilesClientCapabilities
makeFieldsNoPrefix ''SymbolKindClientCapabilities
makeFieldsNoPrefix ''SymbolClientCapabilities
makeFieldsNoPrefix ''ExecuteClientCapabilities
makeFieldsNoPrefix ''WorkspaceClientCapabilities
makeFieldsNoPrefix ''SynchronizationTextDocumentClientCapabilities
makeFieldsNoPrefix ''CompletionItemClientCapabilities
makeFieldsNoPrefix ''CompletionItemKindClientCapabilities
makeFieldsNoPrefix ''CompletionClientCapabilities
makeFieldsNoPrefix ''HoverClientCapabilities
makeFieldsNoPrefix ''SignatureInformationClientCapabilities
makeFieldsNoPrefix ''SignatureHelpClientCapabilities
makeFieldsNoPrefix ''ReferencesClientCapabilities
makeFieldsNoPrefix ''DocumentHighlightClientCapabilities
makeFieldsNoPrefix ''DocumentSymbolKindClientCapabilities
makeFieldsNoPrefix ''DocumentSymbolClientCapabilities
makeFieldsNoPrefix ''FormattingClientCapabilities
makeFieldsNoPrefix ''RangeFormattingClientCapabilities
makeFieldsNoPrefix ''OnTypeFormattingClientCapabilities
makeFieldsNoPrefix ''DefinitionClientCapabilities
makeFieldsNoPrefix ''TypeDefinitionClientCapabilities
makeFieldsNoPrefix ''ImplementationClientCapabilities
makeFieldsNoPrefix ''CodeActionKindClientCapabilities
makeFieldsNoPrefix ''CodeActionLiteralSupport
makeFieldsNoPrefix ''CodeActionClientCapabilities
makeFieldsNoPrefix ''CodeLensClientCapabilities
makeFieldsNoPrefix ''DocumentLinkClientCapabilities
makeFieldsNoPrefix ''ColorProviderClientCapabilities
makeFieldsNoPrefix ''RenameClientCapabilities
makeFieldsNoPrefix ''PublishDiagnosticsClientCapabilities
makeFieldsNoPrefix ''TextDocumentClientCapabilities
makeFieldsNoPrefix ''ClientCapabilities

-- ---------------------------------------------------------------------

makeFieldsNoPrefix ''InitializeParams
makeFieldsNoPrefix ''InitializeError
makeFieldsNoPrefix ''CompletionOptions
makeFieldsNoPrefix ''SignatureHelpOptions
makeFieldsNoPrefix ''CodeLensOptions
makeFieldsNoPrefix ''DocumentOnTypeFormattingOptions
makeFieldsNoPrefix ''DocumentLinkOptions
makeFieldsNoPrefix ''ExecuteCommandOptions
makeFieldsNoPrefix ''SaveOptions
makeFieldsNoPrefix ''TextDocumentSyncOptions
makeFieldsNoPrefix ''WorkspaceFolderOptions
makeFieldsNoPrefix ''WorkspaceOptions
makeFieldsNoPrefix ''InitializeResponseCapabilitiesInner
makeFieldsNoPrefix ''InitializeResponseCapabilities
makeFieldsNoPrefix ''ShowMessageParams
makeFieldsNoPrefix ''MessageActionItem
makeFieldsNoPrefix ''ShowMessageRequestParams
makeFieldsNoPrefix ''LogMessageParams
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
makeFieldsNoPrefix ''DidCloseTextDocumentParams
makeFieldsNoPrefix ''FileEvent
makeFieldsNoPrefix ''DidChangeWatchedFilesParams
makeFieldsNoPrefix ''PublishDiagnosticsParams
makeFieldsNoPrefix ''LanguageString
makeFieldsNoPrefix ''Hover
makeFieldsNoPrefix ''ParameterInformation
makeFieldsNoPrefix ''SignatureInformation
makeFieldsNoPrefix ''SignatureHelp
makeFieldsNoPrefix ''SignatureHelpRegistrationOptions
makeFieldsNoPrefix ''ReferenceContext
makeFieldsNoPrefix ''ReferenceParams
makeFieldsNoPrefix ''DocumentHighlight
makeFieldsNoPrefix ''WorkspaceSymbolParams
makeFieldsNoPrefix ''CodeLensParams
makeFieldsNoPrefix ''CodeLens
makeFieldsNoPrefix ''CodeLensRegistrationOptions
makeFieldsNoPrefix ''DocumentLinkParams
makeFieldsNoPrefix ''DocumentLink
makeFieldsNoPrefix ''FormattingOptions
makeFieldsNoPrefix ''DocumentFormattingParams
makeFieldsNoPrefix ''DocumentRangeFormattingParams
makeFieldsNoPrefix ''DocumentOnTypeFormattingParams
makeFieldsNoPrefix ''RenameParams
makeFieldsNoPrefix ''ExecuteCommandParams
makeFieldsNoPrefix ''ExecuteCommandRegistrationOptions
makeFieldsNoPrefix ''ApplyWorkspaceEditParams
makeFieldsNoPrefix ''ApplyWorkspaceEditResponseBody
makeFieldsNoPrefix ''TraceParams
makeFieldsNoPrefix ''TraceNotification

-- ---------------------------------------------------------------------

-- Location
makeFieldsNoPrefix ''Position
makeFieldsNoPrefix ''Range
makeFieldsNoPrefix ''Location

-- Completion
makeFieldsNoPrefix ''CompletionItem
makeFieldsNoPrefix ''CompletionContext
makeFieldsNoPrefix ''CompletionListType
makeFieldsNoPrefix ''CompletionParams
makeFieldsNoPrefix ''CompletionRegistrationOptions

-- CodeActions
makeFieldsNoPrefix ''CodeActionContext
makeFieldsNoPrefix ''CodeActionParams
makeFieldsNoPrefix ''CodeAction

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

-- Symbol
makeFieldsNoPrefix ''DocumentSymbolParams
makeFieldsNoPrefix ''DocumentSymbol
makeFieldsNoPrefix ''SymbolInformation

-- Color
makeFieldsNoPrefix ''Color
makeFieldsNoPrefix ''ColorInformation
makeFieldsNoPrefix ''DocumentColorParams
makeFieldsNoPrefix ''ColorPresentationParams
makeFieldsNoPrefix ''ColorPresentation

-- Folding Range
makeFieldsNoPrefix ''FoldingRange
makeFieldsNoPrefix ''FoldingRangeParams
