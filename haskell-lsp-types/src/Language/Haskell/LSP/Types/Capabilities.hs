module Language.Haskell.LSP.Types.Capabilities
  (
    module Language.Haskell.LSP.Types.ClientCapabilities
  , fullCaps
  , LSPVersion(..)
  , capsForVersion
  ) where

import Prelude hiding (min)
import Language.Haskell.LSP.Types.ClientCapabilities
import Language.Haskell.LSP.Types

-- | The whole shebang. The real deal.
-- Capabilities for full conformance to the current (v3.10) LSP specification.
fullCaps :: ClientCapabilities
fullCaps = capsForVersion (LSPVersion maxBound maxBound)

-- | A specific version of the LSP specification.
data LSPVersion = LSPVersion Int Int -- ^ Construct a major.minor version

-- | Capabilities for full conformance to the LSP specification up until a version.
-- Some important milestones:
--
-- * 3.9 completion item preselect
-- * 3.8 codeAction literals
-- * 3.7 related information in diagnostics
-- * 3.6 workspace folders, colors, goto type/implementation
-- * 3.4 extended completion item and symbol item kinds
-- * 3.0 dynamic registration
capsForVersion :: LSPVersion -> ClientCapabilities
capsForVersion (LSPVersion maj min) = ClientCapabilities (Just w) (Just td) Nothing Nothing
  where
    w = WorkspaceClientCapabilities
          (Just True)
          (Just (WorkspaceEditClientCapabilities (Just True)))
          (Just (DidChangeConfigurationClientCapabilities dynamicReg))
          (Just (DidChangeWatchedFilesClientCapabilities dynamicReg))
          (Just symbolCapabilities)
          (Just (ExecuteClientCapabilities dynamicReg))
          (since 3 6 True)
          (since 3 6 True)

    symbolCapabilities = SymbolClientCapabilities
      dynamicReg
      (since 3 4 symbolKindCapabilities)

    symbolKindCapabilities =
      SymbolKindClientCapabilities (Just sKs)

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
          (Just (FormattingClientCapabilities (Just True)))
          (Just (RangeFormattingClientCapabilities dynamicReg))
          (Just (OnTypeFormattingClientCapabilities dynamicReg))
          (Just (DefinitionClientCapabilities dynamicReg))
          (since 3 6 (TypeDefinitionClientCapabilities dynamicReg))
          (since 3 6 (ImplementationClientCapabilities dynamicReg))
          (Just codeActionCapability)
          (Just (CodeLensClientCapabilities dynamicReg))
          (Just (DocumentLinkClientCapabilities dynamicReg))
          (since 3 6 (ColorProviderClientCapabilities dynamicReg))
          (Just (RenameClientCapabilities dynamicReg Nothing))
          (Just (PublishDiagnosticsClientCapabilities (since 3 7 True)))
          (since 3 10 foldingRangeCapability)
    sync =
      SynchronizationTextDocumentClientCapabilities
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

    completionItemKindCapabilities =
      CompletionItemKindClientCapabilities (Just ciKs)

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

    signatureInformationCapability =
      SignatureInformationClientCapabilities
        (Just (List [MkPlainText, MkMarkdown]))

    documentSymbolCapability =
      DocumentSymbolClientCapabilities
        dynamicReg
        (since 3 4 documentSymbolKind)
        (since 3 10 True)

    documentSymbolKind =
      DocumentSymbolKindClientCapabilities
        (Just sKs) -- same as workspace symbol kinds

    foldingRangeCapability =
      FoldingRangeClientCapabilities
        dynamicReg
        Nothing
        (Just False)

    dynamicReg
      | maj >= 3  = Just True
      | otherwise = Nothing
    since x y a
      | maj >= x && min >= y = Just a
      | otherwise            = Nothing
