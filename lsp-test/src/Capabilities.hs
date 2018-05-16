module Capabilities where

import Language.Haskell.LSP.TH.ClientCapabilities

capabilities :: ClientCapabilities
capabilities = ClientCapabilities (Just workspaceCapabilities)
                                  (Just textDocumentCapabilities)
                                  Nothing
  where
    workspaceCapabilities = WorkspaceClientCapabilities
      (Just False)
      (Just (WorkspaceEditClientCapabilities (Just False)))
      (Just (DidChangeConfigurationClientCapabilities (Just False)))
      (Just (DidChangeWatchedFilesClientCapabilities (Just False)))
      (Just (SymbolClientCapabilities (Just False)))
      (Just (ExecuteClientCapabilities (Just False)))
    textDocumentCapabilities = TextDocumentClientCapabilities
      (Just
        (SynchronizationTextDocumentClientCapabilities (Just False)
                                                        (Just False)
                                                        (Just False)
                                                        (Just False)
        )
      )
      (Just
        (CompletionClientCapabilities
          (Just False)
          (Just (CompletionItemClientCapabilities (Just False)))
        )
      )
      (Just (HoverClientCapabilities (Just False)))
      (Just (SignatureHelpClientCapabilities (Just False)))
      (Just (ReferencesClientCapabilities (Just False)))
      (Just (DocumentHighlightClientCapabilities (Just False)))
      (Just (DocumentSymbolClientCapabilities (Just False)))
      (Just (FormattingClientCapabilities (Just False)))
      (Just (RangeFormattingClientCapabilities (Just False)))
      (Just (OnTypeFormattingClientCapabilities (Just False)))
      (Just (DefinitionClientCapabilities (Just False)))
      (Just (CodeActionClientCapabilities (Just False)))
      (Just (CodeLensClientCapabilities (Just False)))
      (Just (DocumentLinkClientCapabilities (Just False)))
      (Just (RenameClientCapabilities (Just False)))
