{-# OPTIONS_GHC -Wno-orphans #-}
module Language.LSP.Protocol.Types.Orphans where

import           Language.LSP.Protocol.Internal.Types
import           Data.Default
import           Data.Semigroup                ()

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')
instance Monoid WorkspaceEdit where
  mempty = WorkspaceEdit Nothing Nothing Nothing

instance Default ClientCapabilities where
    def = ClientCapabilities def def def def def Nothing
instance Default WorkspaceClientCapabilities
instance Default TextDocumentClientCapabilities
instance Default NotebookDocumentClientCapabilities where
instance Default NotebookDocumentSyncClientCapabilities
instance Default WindowClientCapabilities
instance Default GeneralClientCapabilities