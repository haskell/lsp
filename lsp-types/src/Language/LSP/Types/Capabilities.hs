{-# LANGUAGE OverloadedStrings #-}
module Language.LSP.Types.Capabilities
  (
    module Language.LSP.Types.ClientCapabilities
  , module Language.LSP.Types.ServerCapabilities
  , module Language.LSP.Types.WorkspaceEdit
  , fullCaps
  , LSPVersion(..)
  , capsForVersion
  ) where

import Prelude hiding (min)
import Language.LSP.Types.ClientCapabilities
import Language.LSP.Types.ServerCapabilities
import Language.LSP.Types.WorkspaceEdit
import Language.LSP.Types

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
capsForVersion (LSPVersion maj min) = ClientCapabilities (Just w) (Just td) (Just window) Nothing
  where
    w = WorkspaceClientCapabilities
          (Just True)
          (Just (WorkspaceEditClientCapabilities
                  (Just True)
                  (since 3 13 resourceOperations)
                  Nothing
                  (since 3 16 True)
                  (since 3 16 (WorkspaceEditChangeAnnotationClientCapabilities (Just True)))))
          (Just (DidChangeConfigurationClientCapabilities dynamicReg))
          (Just (DidChangeWatchedFilesClientCapabilities dynamicReg))
          (Just symbolCapabilities)
          (Just (ExecuteCommandClientCapabilities dynamicReg))
          (since 3 6 True)
          (since 3 6 True)

    resourceOperations = List
      [ ResourceOperationCreate
      , ResourceOperationDelete
      , ResourceOperationRename
      ]

    symbolCapabilities = WorkspaceSymbolClientCapabilities
      dynamicReg
      (since 3 4 symbolKindCapabilities)
      (since 3 16 symbolTagCapabilities)

    symbolKindCapabilities =
      WorkspaceSymbolKindClientCapabilities (Just sKs)

    symbolTagCapabilities =
      WorkspaceSymbolTagClientCapabilities (Just (List [StDeprecated]))

    sKs
      | maj >= 3 && min >= 4 = List (oldSKs ++ newSKs)
      | otherwise            = List oldSKs

    oldSKs =   [ SkFile
               , SkModule
               , SkNamespace
               , SkPackage
               , SkClass
               , SkMethod
               , SkProperty
               , SkField
               , SkConstructor
               , SkEnum
               , SkInterface
               , SkFunction
               , SkVariable
               , SkConstant
               , SkString
               , SkNumber
               , SkBoolean
               , SkArray
               ]

    newSKs = [ SkObject
             , SkKey
             , SkNull
             , SkEnumMember
             , SkStruct
             , SkEvent
             , SkOperator
             , SkTypeParameter
             ]

    td = TextDocumentClientCapabilities
          (Just sync)
          (Just completionCapability)
          (Just hoverCapability)
          (Just signatureHelpCapability)
          (Just (ReferencesClientCapabilities dynamicReg))
          (Just (DocumentHighlightClientCapabilities dynamicReg))
          (Just documentSymbolCapability)
          (Just (DocumentFormattingClientCapabilities dynamicReg))
          (Just (DocumentRangeFormattingClientCapabilities dynamicReg))
          (Just (DocumentOnTypeFormattingClientCapabilities dynamicReg))
          (since 3 14 (DeclarationClientCapabilities dynamicReg (Just True)))
          (Just (DefinitionClientCapabilities dynamicReg (since 3 14 True)))
          (since 3 6 (TypeDefinitionClientCapabilities dynamicReg (since 3 14 True)))
          (since 3 6 (ImplementationClientCapabilities dynamicReg (since 3 14 True)))
          (Just codeActionCapability)
          (Just (CodeLensClientCapabilities dynamicReg))
          (Just (DocumentLinkClientCapabilities dynamicReg (since 3 15 True)))
          (since 3 6 (DocumentColorClientCapabilities dynamicReg))
          (Just (RenameClientCapabilities dynamicReg (since 3 12 True) (since 3 16 PsIdentifier) (since 3 16 True)))
          (Just publishDiagnosticsCapabilities)
          (since 3 10 foldingRangeCapability)
          (since 3 5 (SelectionRangeClientCapabilities dynamicReg))
          (since 3 16 (CallHierarchyClientCapabilities dynamicReg))
    sync =
      TextDocumentSyncClientCapabilities
        dynamicReg
        (Just True)
        (Just True)
        (Just True)

    completionCapability =
      CompletionClientCapabilities
        dynamicReg
        (Just completionItemCapabilities)
        (since 3 4 completionItemKindCapabilities)
        (since 3 3 True)

    completionItemCapabilities = CompletionItemClientCapabilities
      (Just True)
      (Just True)
      (since 3 3 (List [MkPlainText, MkMarkdown]))
      (Just True)
      (since 3 9 True)
      (since 3 15 completionItemTagsCapabilities)
      (since 3 16 True)
      (since 3 16 (CompletionItemResolveClientCapabilities (List ["documentation", "details"])))
      (since 3 16 (CompletionItemInsertTextModeClientCapabilities (List [])))

    completionItemKindCapabilities =
      CompletionItemKindClientCapabilities (Just ciKs)

    completionItemTagsCapabilities =
      CompletionItemTagsClientCapabilities (List [ CitDeprecated ])

    ciKs
      | maj >= 3 && min >= 4 = List (oldCiKs ++ newCiKs)
      | otherwise            = List oldCiKs

    oldCiKs =   [ CiText
                , CiMethod
                , CiFunction
                , CiConstructor
                , CiField
                , CiVariable
                , CiClass
                , CiInterface
                , CiModule
                , CiProperty
                , CiUnit
                , CiValue
                , CiEnum
                , CiKeyword
                , CiSnippet
                , CiColor
                , CiFile
                , CiReference
                ]

    newCiKs =   [ CiFolder
                , CiEnumMember
                , CiConstant
                , CiStruct
                , CiEvent
                , CiOperator
                , CiTypeParameter
                ]

    hoverCapability =
      HoverClientCapabilities
        dynamicReg
        (since 3 3 (List [MkPlainText, MkMarkdown]))

    codeActionCapability
      = CodeActionClientCapabilities
          dynamicReg
          (since 3 8 (CodeActionLiteralSupport caKs))
          (since 3 15 True)
          (since 3 16 True)
          (since 3 16 True)
          (since 3 16 (CodeActionResolveClientCapabilities (List [])))
          (since 3 16 True)
    caKs = CodeActionKindClientCapabilities
              (List [ CodeActionQuickFix
                    , CodeActionRefactor
                    , CodeActionRefactorExtract
                    , CodeActionRefactorInline
                    , CodeActionRefactorRewrite
                    , CodeActionSource
                    , CodeActionSourceOrganizeImports
                    ])

    signatureHelpCapability =
      SignatureHelpClientCapabilities
        dynamicReg
        (Just signatureInformationCapability)
        Nothing

    signatureInformationCapability =
      SignatureHelpSignatureInformation
        (Just (List [MkPlainText, MkMarkdown]))
        (Just signatureParameterCapability)
        (since 3 16 True)

    signatureParameterCapability =
      SignatureHelpParameterInformation (since 3 14 True)

    documentSymbolCapability =
      DocumentSymbolClientCapabilities
        dynamicReg
        (since 3 4 documentSymbolKind)
        (since 3 10 True)
        (since 3 16 documentSymbolTag)
        (since 3 16 True)

    documentSymbolKind =
      DocumentSymbolKindClientCapabilities
        (Just sKs) -- same as workspace symbol kinds

    documentSymbolTag =
      DocumentSymbolTagClientCapabilities (Just (List [StDeprecated]))

    foldingRangeCapability =
      FoldingRangeClientCapabilities
        dynamicReg
        Nothing
        (Just False)

    publishDiagnosticsCapabilities =
      PublishDiagnosticsClientCapabilities
        (since 3 7 True)
        (since 3 15 publishDiagnosticsTagsCapabilities)
        (since 3 15 True)

    publishDiagnosticsTagsCapabilities =
      PublishDiagnosticsTagsClientCapabilities
        (List [ DtUnnecessary, DtDeprecated ])

    dynamicReg
      | maj >= 3  = Just True
      | otherwise = Nothing
    since x y a
      | maj >= x && min >= y = Just a
      | otherwise            = Nothing

    window = WindowClientCapabilities (since 3 15 True)
