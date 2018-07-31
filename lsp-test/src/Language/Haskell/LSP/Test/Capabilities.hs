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
capsForVersion :: LSPVersion -> ClientCapabilities
capsForVersion (LSPVersion maj min) = ClientCapabilities (Just w) (Just td) Nothing
  where
    w = WorkspaceClientCapabilities
          (Just True)
          (Just (WorkspaceEditClientCapabilities (Just True)))
          (Just (DidChangeConfigurationClientCapabilities (Just True)))
          (Just (DidChangeWatchedFilesClientCapabilities (Just True)))
          (Just (SymbolClientCapabilities (Just True)))
          (Just (ExecuteClientCapabilities (Just True)))
    td = TextDocumentClientCapabilities
          (Just sync)
          (Just (CompletionClientCapabilities 
                  (Just True)
                  (Just (CompletionItemClientCapabilities (Just True)))))
          (Just (HoverClientCapabilities (Just True)))
          (Just (SignatureHelpClientCapabilities (Just True)))
          (Just (ReferencesClientCapabilities (Just True)))
          (Just (DocumentHighlightClientCapabilities (Just True)))
          (Just (DocumentSymbolClientCapabilities (Just True)))
          (Just (FormattingClientCapabilities (Just True)))
          (Just (RangeFormattingClientCapabilities (Just True)))
          (Just (OnTypeFormattingClientCapabilities (Just True)))
          (Just (DefinitionClientCapabilities (Just True)))
          (Just codeAction)
          (Just (CodeLensClientCapabilities (Just True)))
          (Just (DocumentLinkClientCapabilities (Just True)))
          (Just (RenameClientCapabilities (Just True)))
    sync = SynchronizationTextDocumentClientCapabilities
            (Just True)
            (Just True)
            (Just True)
            (Just True)
    codeAction = CodeActionClientCapabilities
                  (Just True)
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
