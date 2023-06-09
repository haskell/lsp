{-# OPTIONS_GHC -Wno-orphans #-}
module Language.LSP.Protocol.Types.CustomInstances where

import           Language.LSP.Protocol.Internal.Types
import           Data.Default
import           Data.Hashable
import           Data.Semigroup                ()

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')
instance Monoid WorkspaceEdit where
  mempty = WorkspaceEdit Nothing Nothing Nothing

instance Hashable Location
instance Hashable Range
instance Hashable Position

instance Default ClientCapabilities where
    def = ClientCapabilities (Just def) (Just def) (Just def) (Just def) (Just def) Nothing

instance Default WorkspaceClientCapabilities
instance Default TextDocumentClientCapabilities
instance Default NotebookDocumentClientCapabilities where
    def = NotebookDocumentClientCapabilities def
instance Default NotebookDocumentSyncClientCapabilities
instance Default WindowClientCapabilities
instance Default GeneralClientCapabilities