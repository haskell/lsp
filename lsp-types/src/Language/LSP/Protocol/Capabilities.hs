{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Protocol.Capabilities (
  fullCaps,
  LSPVersion (..),
  capsForVersion,
  dynamicRegistrationSupported,
) where

import Control.Lens
import Data.Maybe
import Data.Row
import Data.Set qualified as Set
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Prelude hiding (min)

{-
TODO: this is out-of-date/needs an audit
TODO: can we generate this? process the 'since' annotations in the metamodel?
-}

-- | Capabilities for full conformance to the current LSP specification.
fullCaps :: ClientCapabilities
fullCaps = capsForVersion (LSPVersion maxBound maxBound)

-- | A specific version of the LSP specification.
data LSPVersion = LSPVersion Int Int

-- | Capabilities for full conformance to the LSP specification up until a version.
capsForVersion :: LSPVersion -> ClientCapabilities
capsForVersion (LSPVersion maj min) = caps
 where
  caps =
    ClientCapabilities
      { _workspace = Just w
      , _textDocument = Just td
      , _window = Just window
      , _general = since 3 16 general
      , _notebookDocument = since 3 17 $ NotebookDocumentClientCapabilities $ NotebookDocumentSyncClientCapabilities dynamicReg (Just True)
      , _experimental = Nothing
      }
  w =
    WorkspaceClientCapabilities
      { _applyEdit = Just True
      , _workspaceEdit =
          Just
            ( WorkspaceEditClientCapabilities
                (Just True)
                (since 3 13 resourceOperations)
                Nothing
                (since 3 16 True)
                (since 3 16 (#groupsOnLabel .== Just True))
            )
      , _didChangeConfiguration = Just (DidChangeConfigurationClientCapabilities dynamicReg)
      , _didChangeWatchedFiles = Just (DidChangeWatchedFilesClientCapabilities dynamicReg (Just True))
      , _symbol = Just symbolCapabilities
      , _executeCommand = Just (ExecuteCommandClientCapabilities dynamicReg)
      , _codeLens = Just (CodeLensWorkspaceClientCapabilities $ Just True)
      , _workspaceFolders = since 3 6 True
      , _configuration = since 3 6 True
      , _semanticTokens = since 3 16 (SemanticTokensWorkspaceClientCapabilities $ Just True)
      , _inlayHint = since 3 17 (InlayHintWorkspaceClientCapabilities $ Just True)
      , _fileOperations = since 3 16 fileOperations
      , _inlineValue = since 3 17 (InlineValueWorkspaceClientCapabilities $ Just True)
      , _diagnostics = since 3 17 (DiagnosticWorkspaceClientCapabilities $ Just True)
      }

  resourceOperations =
    [ ResourceOperationKind_Create
    , ResourceOperationKind_Delete
    , ResourceOperationKind_Rename
    ]

  fileOperations =
    FileOperationClientCapabilities
      dynamicReg
      (Just True)
      (Just True)
      (Just True)
      (Just True)
      (Just True)
      (Just True)

  symbolCapabilities =
    WorkspaceSymbolClientCapabilities
      dynamicReg
      (since 3 4 (#valueSet .== Just sKs))
      (since 3 16 (#valueSet .== [SymbolTag_Deprecated]))
      (since 3 17 (#properties .== []))

  sKs
    | maj >= 3 && min >= 4 = oldSKs ++ newSKs
    | otherwise = oldSKs

  oldSKs =
    [ SymbolKind_File
    , SymbolKind_Module
    , SymbolKind_Namespace
    , SymbolKind_Package
    , SymbolKind_Class
    , SymbolKind_Method
    , SymbolKind_Property
    , SymbolKind_Field
    , SymbolKind_Constructor
    , SymbolKind_Enum
    , SymbolKind_Interface
    , SymbolKind_Function
    , SymbolKind_Variable
    , SymbolKind_Constant
    , SymbolKind_String
    , SymbolKind_Number
    , SymbolKind_Boolean
    , SymbolKind_Array
    ]

  newSKs =
    [ SymbolKind_Object
    , SymbolKind_Key
    , SymbolKind_Null
    , SymbolKind_EnumMember
    , SymbolKind_Struct
    , SymbolKind_Event
    , SymbolKind_Operator
    , SymbolKind_TypeParameter
    ]

  -- Only one token format for now, just list it here
  tfs = [TokenFormat_Relative]

  semanticTokensCapabilities =
    SemanticTokensClientCapabilities
      { _dynamicRegistration = Just True
      , _requests = #range .== Just (InL True) .+ #full .== Just (InR (#delta .== Just True))
      , _tokenTypes = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenTypes)
      , _tokenModifiers = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenModifiers)
      , _formats = tfs
      , _overlappingTokenSupport = Just True
      , _multilineTokenSupport = Just True
      , _serverCancelSupport = Just True
      , _augmentsSyntaxTokens = Just True
      }

  td =
    TextDocumentClientCapabilities
      { _synchronization = Just sync
      , _completion = Just completionCapability
      , _hover = Just hoverCapability
      , _signatureHelp = Just signatureHelpCapability
      , _references = Just (ReferenceClientCapabilities dynamicReg)
      , _documentHighlight = Just (DocumentHighlightClientCapabilities dynamicReg)
      , _documentSymbol = Just documentSymbolCapability
      , _formatting = Just (DocumentFormattingClientCapabilities dynamicReg)
      , _rangeFormatting = Just (DocumentRangeFormattingClientCapabilities dynamicReg)
      , _onTypeFormatting = Just (DocumentOnTypeFormattingClientCapabilities dynamicReg)
      , _declaration = since 3 14 (DeclarationClientCapabilities dynamicReg (Just True))
      , _definition = Just (DefinitionClientCapabilities dynamicReg (since 3 14 True))
      , _typeDefinition = since 3 6 (TypeDefinitionClientCapabilities dynamicReg (since 3 14 True))
      , _implementation = since 3 6 (ImplementationClientCapabilities dynamicReg (since 3 14 True))
      , _codeAction = Just codeActionCapability
      , _codeLens = Just (CodeLensClientCapabilities dynamicReg)
      , _documentLink = Just (DocumentLinkClientCapabilities dynamicReg (since 3 15 True))
      , _colorProvider = since 3 6 (DocumentColorClientCapabilities dynamicReg)
      , _rename = Just (RenameClientCapabilities dynamicReg (since 3 12 True) (since 3 16 PrepareSupportDefaultBehavior_Identifier) (since 3 16 True))
      , _publishDiagnostics = Just publishDiagnosticsCapabilities
      , _foldingRange = since 3 10 foldingRangeCapability
      , _selectionRange = since 3 5 (SelectionRangeClientCapabilities dynamicReg)
      , _callHierarchy = since 3 16 (CallHierarchyClientCapabilities dynamicReg)
      , _semanticTokens = since 3 16 semanticTokensCapabilities
      , _linkedEditingRange = since 3 16 (LinkedEditingRangeClientCapabilities dynamicReg)
      , _moniker = since 3 16 (MonikerClientCapabilities dynamicReg)
      , _inlayHint = since 3 17 inlayHintCapabilities
      , _typeHierarchy = since 3 17 (TypeHierarchyClientCapabilities dynamicReg)
      , _inlineValue = since 3 17 (InlineValueClientCapabilities dynamicReg)
      , _diagnostic = since 3 17 (DiagnosticClientCapabilities dynamicReg (Just True))
      }

  sync =
    TextDocumentSyncClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _willSave = Just True
      , _willSaveWaitUntil = Just True
      , _didSave = Just True
      }

  completionCapability =
    CompletionClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _completionItem = Just completionItemCapabilities
      , _completionItemKind = since 3 4 (#valueSet .== Just ciKs)
      , _insertTextMode = since 3 17 InsertTextMode_AsIs
      , _contextSupport = since 3 3 True
      , _completionList = since 3 17 (#itemDefaults .== Just [])
      }

  inlayHintCapabilities =
    InlayHintClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _resolveSupport = Just (#properties .== [])
      }

  completionItemCapabilities =
    #snippetSupport .== Just True
      .+ #commitCharactersSupport .== Just True
      .+ #documentationFormat .== since 3 3 allMarkups
      .+ #deprecatedSupport .== Just True
      .+ #preselectSupport .== since 3 9 True
      .+ #tagSupport .== since 3 15 (#valueSet .== [])
      .+ #insertReplaceSupport .== since 3 16 True
      .+ #resolveSupport .== since 3 16 (#properties .== ["documentation", "details"])
      .+ #insertTextModeSupport .== since 3 16 (#valueSet .== [])
      .+ #labelDetailsSupport .== since 3 17 True

  ciKs
    | maj >= 3 && min >= 4 = oldCiKs ++ newCiKs
    | otherwise = oldCiKs

  oldCiKs =
    [ CompletionItemKind_Text
    , CompletionItemKind_Method
    , CompletionItemKind_Function
    , CompletionItemKind_Constructor
    , CompletionItemKind_Field
    , CompletionItemKind_Variable
    , CompletionItemKind_Class
    , CompletionItemKind_Interface
    , CompletionItemKind_Module
    , CompletionItemKind_Property
    , CompletionItemKind_Unit
    , CompletionItemKind_Value
    , CompletionItemKind_Enum
    , CompletionItemKind_Keyword
    , CompletionItemKind_Snippet
    , CompletionItemKind_Color
    , CompletionItemKind_File
    , CompletionItemKind_Reference
    ]

  newCiKs =
    [ CompletionItemKind_Folder
    , CompletionItemKind_EnumMember
    , CompletionItemKind_Constant
    , CompletionItemKind_Struct
    , CompletionItemKind_Event
    , CompletionItemKind_Operator
    , CompletionItemKind_TypeParameter
    ]

  hoverCapability =
    HoverClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _contentFormat = since 3 3 allMarkups
      }

  codeActionCapability =
    CodeActionClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _codeActionLiteralSupport = since 3 8 (#codeActionKind .== (#valueSet .== Set.toList knownValues))
      , _isPreferredSupport = since 3 15 True
      , _disabledSupport = since 3 16 True
      , _dataSupport = since 3 16 True
      , _resolveSupport = since 3 16 (#properties .== [])
      , _honorsChangeAnnotations = since 3 16 True
      }

  signatureHelpCapability =
    SignatureHelpClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _signatureInformation = Just (#documentationFormat .== Just allMarkups .+ #parameterInformation .== Just (#labelOffsetSupport .== Just True) .+ #activeParameterSupport .== Just True)
      , _contextSupport = since 3 16 True
      }

  documentSymbolCapability =
    DocumentSymbolClientCapabilities
      { _dynamicRegistration = dynamicReg
      , -- same as workspace symbol kinds
        _symbolKind = Just (#valueSet .== Just sKs)
      , _hierarchicalDocumentSymbolSupport = since 3 10 True
      , _tagSupport = since 3 16 (#valueSet .== [SymbolTag_Deprecated])
      , _labelSupport = since 3 16 True
      }

  foldingRangeCapability =
    FoldingRangeClientCapabilities
      { _dynamicRegistration = dynamicReg
      , _rangeLimit = Nothing
      , _lineFoldingOnly = Nothing
      , _foldingRangeKind = since 3 17 (#valueSet .== Just [])
      , _foldingRange = since 3 16 (#collapsedText .== Just True)
      }

  publishDiagnosticsCapabilities =
    PublishDiagnosticsClientCapabilities
      { _relatedInformation = since 3 7 True
      , _tagSupport = since 3 15 (#valueSet .== [DiagnosticTag_Unnecessary, DiagnosticTag_Deprecated])
      , _versionSupport = since 3 15 True
      , _codeDescriptionSupport = since 3 16 True
      , _dataSupport = since 3 16 True
      }

  dynamicReg
    | maj >= 3 = Just True
    | otherwise = Nothing
  since :: Int -> Int -> a -> Maybe a
  since x y a
    | maj >= x && min >= y = Just a
    | otherwise = Nothing

  window =
    WindowClientCapabilities
      { _workDoneProgress = since 3 15 True
      , _showMessage = since 3 16 $ ShowMessageRequestClientCapabilities Nothing
      , _showDocument = since 3 16 $ ShowDocumentClientCapabilities True
      }

  general =
    GeneralClientCapabilities
      { _staleRequestSupport = since 3 16 (#cancel .== True .+ #retryOnContentModified .== [])
      , _regularExpressions = since 3 16 $ RegularExpressionsClientCapabilities "" Nothing
      , _markdown = since 3 16 $ MarkdownClientCapabilities "" Nothing (Just [])
      , _positionEncodings = since 3 17 [PositionEncodingKind_UTF16]
      }

  allMarkups = [MarkupKind_PlainText, MarkupKind_Markdown]

-- | Whether the client supports dynamic registration for the given method.
dynamicRegistrationSupported :: SMethod m -> ClientCapabilities -> Bool
dynamicRegistrationSupported method caps = fromMaybe False $ case method of
  SMethod_WorkspaceDidChangeConfiguration -> caps ^? ws . L.didChangeConfiguration . _Just . dyn
  SMethod_WorkspaceDidChangeWatchedFiles -> caps ^? ws . L.didChangeWatchedFiles . _Just . dyn
  SMethod_WorkspaceSymbol -> caps ^? ws . L.symbol . _Just . dyn
  SMethod_WorkspaceExecuteCommand -> caps ^? ws . L.executeCommand . _Just . dyn
  SMethod_WorkspaceWillCreateFiles -> caps ^? ws . L.fileOperations . _Just . dyn
  SMethod_WorkspaceDidCreateFiles -> caps ^? ws . L.fileOperations . _Just . dyn
  SMethod_WorkspaceWillDeleteFiles -> caps ^? ws . L.fileOperations . _Just . dyn
  SMethod_WorkspaceDidDeleteFiles -> caps ^? ws . L.fileOperations . _Just . dyn
  SMethod_TextDocumentDidOpen -> caps ^? td . L.synchronization . _Just . dyn
  SMethod_TextDocumentDidChange -> caps ^? td . L.synchronization . _Just . dyn
  SMethod_TextDocumentDidClose -> caps ^? td . L.synchronization . _Just . dyn
  SMethod_TextDocumentCompletion -> caps ^? td . L.completion . _Just . dyn
  SMethod_TextDocumentHover -> caps ^? td . L.hover . _Just . dyn
  SMethod_TextDocumentSignatureHelp -> caps ^? td . L.signatureHelp . _Just . dyn
  SMethod_TextDocumentDeclaration -> caps ^? td . L.declaration . _Just . dyn
  SMethod_TextDocumentDefinition -> caps ^? td . L.definition . _Just . dyn
  SMethod_TextDocumentTypeDefinition -> caps ^? td . L.typeDefinition . _Just . dyn
  SMethod_TextDocumentImplementation -> caps ^? td . L.implementation . _Just . dyn
  SMethod_TextDocumentReferences -> caps ^? td . L.references . _Just . dyn
  SMethod_TextDocumentDocumentHighlight -> caps ^? td . L.documentHighlight . _Just . dyn
  SMethod_TextDocumentDocumentSymbol -> caps ^? td . L.documentSymbol . _Just . dyn
  SMethod_TextDocumentCodeAction -> caps ^? td . L.codeAction . _Just . dyn
  SMethod_TextDocumentCodeLens -> caps ^? td . L.codeLens . _Just . dyn
  SMethod_TextDocumentDocumentLink -> caps ^? td . L.documentLink . _Just . dyn
  SMethod_TextDocumentDocumentColor -> caps ^? td . L.colorProvider . _Just . dyn
  SMethod_TextDocumentColorPresentation -> caps ^? td . L.colorProvider . _Just . dyn
  SMethod_TextDocumentFormatting -> caps ^? td . L.formatting . _Just . dyn
  SMethod_TextDocumentRangeFormatting -> caps ^? td . L.rangeFormatting . _Just . dyn
  SMethod_TextDocumentOnTypeFormatting -> caps ^? td . L.onTypeFormatting . _Just . dyn
  SMethod_TextDocumentRename -> caps ^? td . L.rename . _Just . dyn
  SMethod_TextDocumentFoldingRange -> caps ^? td . L.foldingRange . _Just . dyn
  SMethod_TextDocumentSelectionRange -> caps ^? td . L.selectionRange . _Just . dyn
  SMethod_TextDocumentLinkedEditingRange -> caps ^? td . L.linkedEditingRange . _Just . dyn
  SMethod_TextDocumentPrepareCallHierarchy -> caps ^? td . L.callHierarchy . _Just . dyn
  SMethod_TextDocumentInlayHint -> caps ^? td . L.inlayHint . _Just . dyn
  SMethod_TextDocumentInlineValue -> caps ^? td . L.inlineValue . _Just . dyn
  SMethod_TextDocumentMoniker -> caps ^? td . L.moniker . _Just . dyn
  SMethod_TextDocumentPrepareTypeHierarchy -> caps ^? td . L.typeHierarchy . _Just . dyn
  SMethod_TextDocumentDiagnostic -> caps ^? td . L.diagnostic . _Just . dyn
  -- semantic tokens is messed up due to it having you register with an otherwise non-existent method
  -- SMethod_TextDocumentSemanticTokens       -> capDyn $ clientCaps ^? L.textDocument . _Just . L.semanticTokens . _Just
  -- Notebook document methods alway support dynamic registration, it seems?
  _ -> Just False
 where
  td :: Traversal' ClientCapabilities TextDocumentClientCapabilities
  td = L.textDocument . _Just

  ws :: Traversal' ClientCapabilities WorkspaceClientCapabilities
  ws = L.workspace . _Just

  dyn :: L.HasDynamicRegistration a (Maybe Bool) => Traversal' a Bool
  dyn = L.dynamicRegistration . _Just
