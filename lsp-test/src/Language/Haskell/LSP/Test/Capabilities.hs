module Language.Haskell.LSP.Test.Capabilities where
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities

-- | Capabilities for full conformance to the LSP specification.
-- The whole shebang.
fullCaps :: ClientCapabilities
fullCaps = ClientCapabilities (Just w) (Just td) Nothing
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
                  (Just (CodeActionLiteralSupport kinds))
    kinds = CodeActionKindValueSet
              (List [ CodeActionQuickFix
                    , CodeActionRefactor
                    , CodeActionRefactorExtract
                    , CodeActionRefactorInline
                    , CodeActionRefactorRewrite
                    , CodeActionSource
                    , CodeActionSourceOrganizeImports
                    ])
