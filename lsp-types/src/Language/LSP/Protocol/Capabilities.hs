{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Protocol.Capabilities (
  fullCaps,
  LSPVersion (..),
  capsForVersion,
  dynamicRegistrationSupported,
) where

import Data.Maybe
import Data.Set qualified as Set
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
      { workspace = Just w
      , textDocument = Just td
      , window = Just window
      , general = since 3 16 general
      , notebookDocument = since 3 17 $ NotebookDocumentClientCapabilities $ NotebookDocumentSyncClientCapabilities dynamicReg (Just True)
      , experimental = Nothing
      }
  w =
    WorkspaceClientCapabilities
      { applyEdit = Just True
      , workspaceEdit =
          Just
            ( WorkspaceEditClientCapabilities
                (Just True)
                (since 3 13 resourceOperations)
                Nothing
                (since 3 16 True)
                (since 3 16 (ChangeAnnotationsSupportOptions{groupsOnLabel = Just True}))
            )
      , didChangeConfiguration = Just (DidChangeConfigurationClientCapabilities dynamicReg)
      , didChangeWatchedFiles = Just (DidChangeWatchedFilesClientCapabilities dynamicReg (Just True))
      , symbol = Just symbolCapabilities
      , executeCommand = Just (ExecuteCommandClientCapabilities dynamicReg)
      , codeLens = Just (CodeLensWorkspaceClientCapabilities $ Just True)
      , workspaceFolders = since 3 6 True
      , configuration = since 3 6 True
      , semanticTokens = since 3 16 (SemanticTokensWorkspaceClientCapabilities $ Just True)
      , inlayHint = since 3 17 (InlayHintWorkspaceClientCapabilities $ Just True)
      , fileOperations = since 3 16 fileOperations
      , inlineValue = since 3 17 (InlineValueWorkspaceClientCapabilities $ Just True)
      , diagnostics = since 3 17 (DiagnosticWorkspaceClientCapabilities $ Just True)
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
      (since 3 4 (ClientSymbolKindOptions{valueSet = Just sKs}))
      (since 3 16 (ClientSymbolTagOptions{valueSet = [SymbolTag_Deprecated]}))
      (since 3 17 (ClientSymbolResolveOptions{properties = []}))

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
      { dynamicRegistration = Just True
      , requests = ClientSemanticTokensRequestOptions{range = Just (InL True), full = Just (InR (ClientSemanticTokensRequestFullDelta{delta = Just True}))}
      , tokenTypes = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenTypes)
      , tokenModifiers = toEnumBaseType <$> Set.toList (knownValues @SemanticTokenModifiers)
      , formats = tfs
      , overlappingTokenSupport = Just True
      , multilineTokenSupport = Just True
      , serverCancelSupport = Just True
      , augmentsSyntaxTokens = Just True
      }

  td =
    TextDocumentClientCapabilities
      { synchronization = Just sync
      , completion = Just completionCapability
      , hover = Just hoverCapability
      , signatureHelp = Just signatureHelpCapability
      , references = Just (ReferenceClientCapabilities dynamicReg)
      , documentHighlight = Just (DocumentHighlightClientCapabilities dynamicReg)
      , documentSymbol = Just documentSymbolCapability
      , formatting = Just (DocumentFormattingClientCapabilities dynamicReg)
      , rangeFormatting = Just (DocumentRangeFormattingClientCapabilities dynamicReg)
      , onTypeFormatting = Just (DocumentOnTypeFormattingClientCapabilities dynamicReg)
      , declaration = since 3 14 (DeclarationClientCapabilities dynamicReg (Just True))
      , definition = Just (DefinitionClientCapabilities dynamicReg (since 3 14 True))
      , typeDefinition = since 3 6 (TypeDefinitionClientCapabilities dynamicReg (since 3 14 True))
      , implementation = since 3 6 (ImplementationClientCapabilities dynamicReg (since 3 14 True))
      , codeAction = Just codeActionCapability
      , codeLens = Just (CodeLensClientCapabilities dynamicReg)
      , documentLink = Just (DocumentLinkClientCapabilities dynamicReg (since 3 15 True))
      , colorProvider = since 3 6 (DocumentColorClientCapabilities dynamicReg)
      , rename = Just (RenameClientCapabilities dynamicReg (since 3 12 True) (since 3 16 PrepareSupportDefaultBehavior_Identifier) (since 3 16 True))
      , publishDiagnostics = Just publishDiagnosticsCapabilities
      , foldingRange = since 3 10 foldingRangeCapability
      , selectionRange = since 3 5 (SelectionRangeClientCapabilities dynamicReg)
      , callHierarchy = since 3 16 (CallHierarchyClientCapabilities dynamicReg)
      , semanticTokens = since 3 16 semanticTokensCapabilities
      , linkedEditingRange = since 3 16 (LinkedEditingRangeClientCapabilities dynamicReg)
      , moniker = since 3 16 (MonikerClientCapabilities dynamicReg)
      , inlayHint = since 3 17 inlayHintCapabilities
      , typeHierarchy = since 3 17 (TypeHierarchyClientCapabilities dynamicReg)
      , inlineValue = since 3 17 (InlineValueClientCapabilities dynamicReg)
      , diagnostic = since 3 17 (DiagnosticClientCapabilities dynamicReg (Just True))
      }

  sync =
    TextDocumentSyncClientCapabilities
      { dynamicRegistration = dynamicReg
      , willSave = Just True
      , willSaveWaitUntil = Just True
      , didSave = Just True
      }

  completionCapability =
    CompletionClientCapabilities
      { dynamicRegistration = dynamicReg
      , completionItem = Just completionItemCapabilities
      , completionItemKind = since 3 4 (ClientCompletionItemOptionsKind{valueSet = Just ciKs})
      , insertTextMode = since 3 17 InsertTextMode_AsIs
      , contextSupport = since 3 3 True
      , completionList = since 3 17 (CompletionListCapabilities{itemDefaults = Just []})
      }

  inlayHintCapabilities =
    InlayHintClientCapabilities
      { dynamicRegistration = dynamicReg
      , resolveSupport = Just (ClientInlayHintResolveOptions{properties = []})
      }

  completionItemCapabilities =
    ClientCompletionItemOptions
      { snippetSupport = Just True
      , commitCharactersSupport = Just True
      , documentationFormat = since 3 3 allMarkups
      , deprecatedSupport = Just True
      , preselectSupport = since 3 9 True
      , tagSupport = since 3 15 (CompletionItemTagOptions{valueSet = []})
      , insertReplaceSupport = since 3 16 True
      , resolveSupport = since 3 16 (ClientCompletionItemResolveOptions{properties = ["documentation", "details"]})
      , insertTextModeSupport = since 3 16 (ClientCompletionItemInsertTextModeOptions{valueSet = []})
      , labelDetailsSupport = since 3 17 True
      }

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
      { dynamicRegistration = dynamicReg
      , contentFormat = since 3 3 allMarkups
      }

  codeActionCapability =
    CodeActionClientCapabilities
      { dynamicRegistration = dynamicReg
      , codeActionLiteralSupport = since 3 8 (ClientCodeActionLiteralOptions{codeActionKind = ClientCodeActionKindOptions{valueSet = Set.toList knownValues}})
      , isPreferredSupport = since 3 15 True
      , disabledSupport = since 3 16 True
      , dataSupport = since 3 16 True
      , resolveSupport = since 3 16 (ClientCodeActionResolveOptions{properties = []})
      , honorsChangeAnnotations = since 3 16 True
      }

  signatureHelpCapability =
    SignatureHelpClientCapabilities
      { dynamicRegistration = dynamicReg
      , signatureInformation =
          Just $
            ClientSignatureInformationOptions
              { documentationFormat = Just allMarkups
              , parameterInformation = Just (ClientSignatureParameterInformationOptions{labelOffsetSupport = Just True})
              , activeParameterSupport = Just True
              }
      , contextSupport = since 3 16 True
      }

  documentSymbolCapability =
    DocumentSymbolClientCapabilities
      { dynamicRegistration = dynamicReg
      , -- same as workspace symbol kinds
        symbolKind = Just (ClientSymbolKindOptions{valueSet = Just sKs})
      , hierarchicalDocumentSymbolSupport = since 3 10 True
      , tagSupport = since 3 16 (ClientSymbolTagOptions{valueSet = [SymbolTag_Deprecated]})
      , labelSupport = since 3 16 True
      }

  foldingRangeCapability =
    FoldingRangeClientCapabilities
      { dynamicRegistration = dynamicReg
      , rangeLimit = Nothing
      , lineFoldingOnly = Nothing
      , foldingRangeKind = since 3 17 (ClientFoldingRangeKindOptions{valueSet = Just []})
      , foldingRange = since 3 16 (ClientFoldingRangeOptions{collapsedText = Just True})
      }

  publishDiagnosticsCapabilities =
    PublishDiagnosticsClientCapabilities
      { relatedInformation = since 3 7 True
      , tagSupport = since 3 15 (ClientDiagnosticsTagOptions{valueSet = [DiagnosticTag_Unnecessary, DiagnosticTag_Deprecated]})
      , versionSupport = since 3 15 True
      , codeDescriptionSupport = since 3 16 True
      , dataSupport = since 3 16 True
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
      { workDoneProgress = since 3 15 True
      , showMessage = since 3 16 $ ShowMessageRequestClientCapabilities Nothing
      , showDocument = since 3 16 $ ShowDocumentClientCapabilities True
      }

  general =
    GeneralClientCapabilities
      { staleRequestSupport = since 3 16 (StaleRequestSupportOptions{cancel = True, retryOnContentModified = []})
      , regularExpressions = since 3 16 $ RegularExpressionsClientCapabilities (RegularExpressionEngineKind "") Nothing
      , markdown = since 3 16 $ MarkdownClientCapabilities "" Nothing (Just [])
      , positionEncodings = since 3 17 [PositionEncodingKind_UTF16]
      }

  allMarkups = [MarkupKind_PlainText, MarkupKind_Markdown]

-- | Whether the client supports dynamic registration for the given method.
dynamicRegistrationSupported :: forall t (m :: Method ClientToServer t). SMethod m -> ClientCapabilities -> Bool
dynamicRegistrationSupported method caps = fromMaybe False $ case method of
  SMethod_WorkspaceDidChangeConfiguration -> caps.workspace >>= didChangeConfiguration >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceDidChangeWatchedFiles -> caps.workspace >>= didChangeWatchedFiles >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceSymbol -> caps.workspace >>= symbol >>= \s -> s.dynamicRegistration
  SMethod_WorkspaceExecuteCommand -> caps.workspace >>= executeCommand >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceWillCreateFiles -> caps.workspace >>= \w -> w.fileOperations >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceDidCreateFiles -> caps.workspace >>= \w -> w.fileOperations >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceWillDeleteFiles -> caps.workspace >>= \w -> w.fileOperations >>= \c -> c.dynamicRegistration
  SMethod_WorkspaceDidDeleteFiles -> caps.workspace >>= \w -> w.fileOperations >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDidOpen -> caps.textDocument >>= \td -> td.synchronization >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDidChange -> caps.textDocument >>= \td -> td.synchronization >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDidClose -> caps.textDocument >>= \td -> td.synchronization >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentCompletion -> caps.textDocument >>= completion >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentHover -> caps.textDocument >>= hover >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentSignatureHelp -> caps.textDocument >>= signatureHelp >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDeclaration -> caps.textDocument >>= declaration >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDefinition -> caps.textDocument >>= definition >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentTypeDefinition -> caps.textDocument >>= typeDefinition >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentImplementation -> caps.textDocument >>= implementation >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentReferences -> caps.textDocument >>= references >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDocumentHighlight -> caps.textDocument >>= documentHighlight >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDocumentSymbol -> caps.textDocument >>= documentSymbol >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentCodeAction -> caps.textDocument >>= codeAction >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentCodeLens -> caps.textDocument >>= \td -> td.codeLens >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDocumentLink -> caps.textDocument >>= documentLink >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDocumentColor -> caps.textDocument >>= \td -> td.colorProvider >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentColorPresentation -> caps.textDocument >>= \td -> td.colorProvider >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentFormatting -> caps.textDocument >>= formatting >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentRangeFormatting -> caps.textDocument >>= rangeFormatting >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentOnTypeFormatting -> caps.textDocument >>= onTypeFormatting >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentRename -> caps.textDocument >>= rename >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentFoldingRange -> caps.textDocument >>= \td -> td.foldingRange >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentSelectionRange -> caps.textDocument >>= \td -> td.selectionRange >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentLinkedEditingRange -> caps.textDocument >>= linkedEditingRange >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentPrepareCallHierarchy -> caps.textDocument >>= callHierarchy >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentInlayHint -> caps.textDocument >>= \td -> td.inlayHint >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentInlineValue -> caps.textDocument >>= \td -> td.inlineValue >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentMoniker -> caps.textDocument >>= moniker >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentPrepareTypeHierarchy -> caps.textDocument >>= typeHierarchy >>= \c -> c.dynamicRegistration
  SMethod_TextDocumentDiagnostic -> caps.textDocument >>= diagnostic >>= \c -> c.dynamicRegistration
  -- semantic tokens is messed up due to it having you register with an otherwise non-existent method
  -- SMethod_TextDocumentSemanticTokens       -> capDyn $ clientCaps ^? L.textDocument . _Just . L.semanticTokens . _Just
  -- Notebook document methods alway support dynamic registration, it seems?
  _ -> Just False
