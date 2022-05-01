{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.LSP.Types.Capabilities
  (
    fullCaps
  , LSPVersion(..)
  , capsForVersion
  ) where

import           Data.Row
import           Language.LSP.Types.Common
import           Language.LSP.Types.CodeAction
import           Language.LSP.Types.SemanticTokens
import           Language.LSP.Types.Internal.Generated
import           Prelude                               hiding (min)

{-
TODO: this is out-of-date/needs an audit
TODO: can we generate this? process the 'since' annotations in the metamodel?
-}

-- | Capabilities for full conformance to the current (v3.15) LSP specification.
fullCaps :: ClientCapabilities
fullCaps = capsForVersion (LSPVersion maxBound maxBound)

-- | A specific version of the LSP specification.
data LSPVersion = LSPVersion Int Int -- ^ Construct a major.minor version

-- | Capabilities for full conformance to the LSP specification up until a version.
-- Some important milestones:
--
-- * 3.12 textDocument/prepareRename request
-- * 3.11 CodeActionOptions provided by the server
-- * 3.10 hierarchical document symbols, folding ranges
-- * 3.9 completion item preselect
-- * 3.8 codeAction literals
-- * 3.7 related information in diagnostics
-- * 3.6 workspace folders, colors, goto type/implementation
-- * 3.4 extended completion item and symbol item kinds
-- * 3.0 dynamic registration
capsForVersion :: LSPVersion -> ClientCapabilities
capsForVersion (LSPVersion maj min) = caps
  where
    caps = ClientCapabilities {
      _workspace=Just w
      , _textDocument=Just td
      , _window=Just window
      , _general=since 3 16 general
      , _experimental=Nothing
      -- TODO
      , _notebookDocument=Nothing
      }
    w = WorkspaceClientCapabilities {
      _applyEdit = Just True
      , _workspaceEdit = Just (WorkspaceEditClientCapabilities
                  (Just True)
                  (since 3 13 resourceOperations)
                  Nothing
                  (since 3 16 True)
                  (since 3 16 (#groupsOnLabel .== Just True)))
      , _didChangeConfiguration = Just (DidChangeConfigurationClientCapabilities dynamicReg)
      , _didChangeWatchedFiles = Just (DidChangeWatchedFilesClientCapabilities dynamicReg (Just True))
      , _symbol = Just symbolCapabilities
      , _executeCommand = Just (ExecuteCommandClientCapabilities dynamicReg)
      , _workspaceFolders = since 3 6 True
      , _configuration = since 3 6 True
      , _semanticTokens = since 3 16 (SemanticTokensWorkspaceClientCapabilities $ Just True)
      -- TODO
      , _codeLens = Nothing
      , _fileOperations = Nothing
      , _inlineValue = Nothing
      , _inlayHint = Nothing
      , _diagnostics = Nothing
      }

    resourceOperations =
      [ ResourceOperationKind_Create
      , ResourceOperationKind_Delete
      , ResourceOperationKind_Rename
      ]

    symbolCapabilities = WorkspaceSymbolClientCapabilities
      dynamicReg
      (since 3 4 (#valueSet .== Just sKs))
      (since 3 16 (#valueSet .== [SymbolTag_Deprecated]))
      (since 3 17 (#properties .== []))

    sKs
      | maj >= 3 && min >= 4 = oldSKs ++ newSKs
      | otherwise            = oldSKs

    oldSKs =   [ SymbolKind_File
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

    newSKs = [ SymbolKind_Object
             , SymbolKind_Key
             , SymbolKind_Null
             , SymbolKind_EnumMember
             , SymbolKind_Struct
             , SymbolKind_Event
             , SymbolKind_Operator
             , SymbolKind_TypeParameter
             ]

    -- Only one token format for now, just list it here
    tfs = [ TokenFormat_Relative ]

    semanticTokensCapabilities = SemanticTokensClientCapabilities {
        _dynamicRegistration=Just True
        , _requests= #range .== Just (InL True) .+ #full .== Just (InR (#delta .== Just True))
        , _tokenTypes=fmap semanticTokenTypesToValue knownSemanticTokenTypes
        , _tokenModifiers=fmap semanticTokenModifiersToValue knownSemanticTokenModifiers
        , _formats=tfs
        , _overlappingTokenSupport=Just True
        , _multilineTokenSupport=Just True
        , _serverCancelSupport=Just True
        , _augmentsSyntaxTokens=Just True
      }

    td = TextDocumentClientCapabilities {
          _synchronization=Just sync
          , _completion=Just completionCapability
          , _hover=Just hoverCapability
          , _signatureHelp=Just signatureHelpCapability
          , _references=Just (ReferenceClientCapabilities dynamicReg)
          , _documentHighlight=Just (DocumentHighlightClientCapabilities dynamicReg)
          , _documentSymbol=Just documentSymbolCapability
          , _formatting=Just (DocumentFormattingClientCapabilities dynamicReg)
          , _rangeFormatting=Just (DocumentRangeFormattingClientCapabilities dynamicReg)
          , _onTypeFormatting=Just (DocumentOnTypeFormattingClientCapabilities dynamicReg)
          , _declaration=since 3 14 (DeclarationClientCapabilities dynamicReg (Just True))
          , _definition=Just (DefinitionClientCapabilities dynamicReg (since 3 14 True))
          , _typeDefinition=since 3 6 (TypeDefinitionClientCapabilities dynamicReg (since 3 14 True))
          , _implementation=since 3 6 (ImplementationClientCapabilities dynamicReg (since 3 14 True))
          , _codeAction=Just codeActionCapability
          , _codeLens=Just (CodeLensClientCapabilities dynamicReg)
          , _documentLink=Just (DocumentLinkClientCapabilities dynamicReg (since 3 15 True))
          , _colorProvider=since 3 6 (DocumentColorClientCapabilities dynamicReg)
          , _rename=Just (RenameClientCapabilities dynamicReg (since 3 12 True) (since 3 16 PrepareSupportDefaultBehavior_Identifier) (since 3 16 True))
          , _publishDiagnostics=Just publishDiagnosticsCapabilities
          , _foldingRange=since 3 10 foldingRangeCapability
          , _selectionRange=since 3 5 (SelectionRangeClientCapabilities dynamicReg)
          , _callHierarchy=since 3 16 (CallHierarchyClientCapabilities dynamicReg)
          , _semanticTokens=since 3 16 semanticTokensCapabilities
          -- TODO
          , _linkedEditingRange=Nothing
          , _moniker=Nothing
          , _typeHierarchy=Nothing
          , _inlineValue=Nothing
          , _inlayHint=Nothing
          , _diagnostic=Nothing
        }

    sync =
      TextDocumentSyncClientCapabilities {
        _dynamicRegistration=dynamicReg
        , _willSave=Just True
        , _willSaveWaitUntil=Just True
        , _didSave=Just True
        }

    completionCapability =
      CompletionClientCapabilities{
        _dynamicRegistration=dynamicReg
        , _completionItem=Just completionItemCapabilities
        , _completionItemKind=since 3 4 (#valueSet .== Just ciKs)
        , _insertTextMode=since 3 17 InsertTextMode_AsIs
        , _contextSupport=since 3 3 True
        , _completionList=since 3 17 (#itemDefaults .== Just [])
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
      | otherwise            = oldCiKs

    oldCiKs =   [ CompletionItemKind_Text
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

    newCiKs =   [ CompletionItemKind_Folder
                , CompletionItemKind_EnumMember
                , CompletionItemKind_Constant
                , CompletionItemKind_Struct
                , CompletionItemKind_Event
                , CompletionItemKind_Operator
                , CompletionItemKind_TypeParameter
                ]

    hoverCapability =
      HoverClientCapabilities {
        _dynamicRegistration=dynamicReg
        , _contentFormat=since 3 3 allMarkups
      }

    codeActionCapability
      = CodeActionClientCapabilities {
          _dynamicRegistration=dynamicReg
          , _codeActionLiteralSupport=since 3 8 (#codeActionKind .== (#valueSet .== specCodeActionKinds))
          , _isPreferredSupport=since 3 15 True
          , _disabledSupport=since 3 16 True
          , _dataSupport=since 3 16 True
          , _resolveSupport=since 3 16 (#properties .== [])
          , _honorsChangeAnnotations=since 3 16 True
        }

    signatureHelpCapability =
      SignatureHelpClientCapabilities {
        _dynamicRegistration=dynamicReg
        , _signatureInformation=Just (#documentationFormat .== Just allMarkups .+ #parameterInformation .== Just (#labelOffsetSupport .== Just True) .+ #activeParameterSupport .== Just True)
        , _contextSupport=since 3 16 True
      }

    documentSymbolCapability =
      DocumentSymbolClientCapabilities {
        _dynamicRegistration=dynamicReg
        -- same as workspace symbol kinds
        , _symbolKind=Just (#valueSet .== Just sKs)
        , _hierarchicalDocumentSymbolSupport=since 3 10 True
        , _tagSupport=since 3 16 (#valueSet .== [SymbolTag_Deprecated])
        , _labelSupport=since 3 16 True
      }

    foldingRangeCapability =
      FoldingRangeClientCapabilities {
        _dynamicRegistration=dynamicReg
        , _rangeLimit=Nothing
        , _lineFoldingOnly=Nothing
        , _foldingRangeKind=since 3 17 (#valueSet .== Just [])
        , _foldingRange=since 3 16 (#collapsedText .== Just True)
      }

    publishDiagnosticsCapabilities =
      PublishDiagnosticsClientCapabilities {
        _relatedInformation=since 3 7 True
        , _tagSupport=since 3 15 (#valueSet .== [ DiagnosticTag_Unnecessary, DiagnosticTag_Deprecated ])
        , _versionSupport=since 3 15 True
        , _codeDescriptionSupport=since 3 16 True
        , _dataSupport=since 3 16 True
      }

    dynamicReg
      | maj >= 3  = Just True
      | otherwise = Nothing
    since x y a
      | maj >= x && min >= y = Just a
      | otherwise            = Nothing

    window =
      WindowClientCapabilities {
        _workDoneProgress=since 3 15 True
        , _showMessage=since 3 16 $ ShowMessageRequestClientCapabilities Nothing
        , _showDocument=since 3 16 $ ShowDocumentClientCapabilities True
      }

    general = GeneralClientCapabilities {
      _staleRequestSupport=since 3 16 (#cancel .== True .+ #retryOnContentModified .== [])
      , _regularExpressions=since 3 16 $ RegularExpressionsClientCapabilities "" Nothing
      , _markdown=since 3 16 $ MarkdownClientCapabilities "" Nothing (Just [])
      -- TODO
      , _positionEncodings=Nothing
      }

    allMarkups = [MarkupKind_PlainText, MarkupKind_Markdown]
