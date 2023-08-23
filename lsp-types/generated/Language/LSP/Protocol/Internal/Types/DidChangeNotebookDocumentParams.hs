-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeNotebookDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.VersionedNotebookDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The params sent in a change notebook document notification.

@since 3.17.0
-}
data DidChangeNotebookDocumentParams = DidChangeNotebookDocumentParams 
  { {-|
  The notebook document that did change. The version number points
  to the version after all provided changes have been applied. If
  only the text document content of a cell changes the notebook version
  doesn't necessarily have to change.
  -}
  _notebookDocument :: Language.LSP.Protocol.Internal.Types.VersionedNotebookDocumentIdentifier.VersionedNotebookDocumentIdentifier
  , {-|
  The actual changes to the notebook document.

  The changes describe single state changes to the notebook document.
  So if there are two changes c1 (at array index 0) and c2 (at array
  index 1) for a notebook in state S then c1 moves the notebook from
  S to S' and c2 from S' to S''. So c1 is computed on the state S and
  c2 is computed on the state S'.

  To mirror the content of a notebook using change events use the following approach:
  - start with the same initial content
  - apply the 'notebookDocument/didChange' notifications in the order you receive them.
  - apply the `NotebookChangeEvent`s in a single notification in the order
    you receive them.
  -}
  _change :: Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent.NotebookDocumentChangeEvent
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeNotebookDocumentParams)

instance Aeson.ToJSON DidChangeNotebookDocumentParams where
  toJSON (DidChangeNotebookDocumentParams arg0 arg1) = Aeson.object $ concat $  [["notebookDocument" Aeson..= arg0]
    ,["change" Aeson..= arg1]]

instance Aeson.FromJSON DidChangeNotebookDocumentParams where
  parseJSON = Aeson.withObject "DidChangeNotebookDocumentParams" $ \arg -> DidChangeNotebookDocumentParams <$> arg Aeson..: "notebookDocument" <*> arg Aeson..: "change"
