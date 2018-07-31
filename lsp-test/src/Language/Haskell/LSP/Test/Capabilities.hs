-- TODO: Move this into haskell-lsp
module Language.Haskell.LSP.Test.Capabilities where
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities

-- | Capabilities for full conformance to the current (v3.10) LSP specification.
-- The whole shebang.
fullCaps :: ClientCapabilities
fullCaps = capsForVersion (LSPVersion maxBound maxBound)

-- | A specific version of the LSP specification.
data LSPVersion = LSPVersion Int -- ^ Major
                             Int -- ^ Minor

-- | Capabilities for full conformance to the LSP specification up until a version.
-- Some important milestones:
--
-- * 3.8 codeAction literals
-- * 3.0 dynamic registration
capsForVersion :: LSPVersion -> ClientCapabilities
capsForVersion (LSPVersion maj min) = ClientCapabilities (Just w) (Just td) Nothing
  where
    w = WorkspaceClientCapabilities
          (Just True)
          (Just (WorkspaceEditClientCapabilities (Just True)))
          (Just (DidChangeConfigurationClientCapabilities dynamicReg))
          (Just (DidChangeWatchedFilesClientCapabilities dynamicReg))
          (Just (SymbolClientCapabilities dynamicReg))
          (Just (ExecuteClientCapabilities dynamicReg))
    td = TextDocumentClientCapabilities
          (Just sync)
          (Just (CompletionClientCapabilities 
                  dynamicReg
                  (Just (CompletionItemClientCapabilities (Just True)))))
          (Just (HoverClientCapabilities dynamicReg))
          (Just (SignatureHelpClientCapabilities dynamicReg))
          (Just (ReferencesClientCapabilities dynamicReg))
          (Just (DocumentHighlightClientCapabilities dynamicReg))
          (Just (DocumentSymbolClientCapabilities dynamicReg))
          (Just (FormattingClientCapabilities (Just True)))
          (Just (RangeFormattingClientCapabilities dynamicReg))
          (Just (OnTypeFormattingClientCapabilities dynamicReg))
          (Just (DefinitionClientCapabilities dynamicReg))
          (Just codeAction)
          (Just (CodeLensClientCapabilities dynamicReg))
          (Just (DocumentLinkClientCapabilities dynamicReg))
          (Just (RenameClientCapabilities dynamicReg))
    sync = SynchronizationTextDocumentClientCapabilities
            dynamicReg
            (Just True)
            (Just True)
            (Just True)
    codeAction = CodeActionClientCapabilities
                  dynamicReg
                  codeActionLiterals
    codeActionLiterals
      | maj >= 3 && min >= 8 = Just (CodeActionLiteralSupport kinds)
      | otherwise            = Nothing
    kinds = CodeActionKindValueSet
              (List [ CodeActionQuickFix
                    , CodeActionRefactor
                    , CodeActionRefactorExtract
                    , CodeActionRefactorInline
                    , CodeActionRefactorRewrite
                    , CodeActionSource
                    , CodeActionSourceOrganizeImports
                    ])
    dynamicReg
      | maj >= 3  = Just True
      | otherwise = Nothing
