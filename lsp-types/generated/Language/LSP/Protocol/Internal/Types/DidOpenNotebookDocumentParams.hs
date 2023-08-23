-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidOpenNotebookDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocument
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentItem
import qualified Language.LSP.Protocol.Types.Common

{-|
The params sent in an open notebook document notification.

@since 3.17.0
-}
data DidOpenNotebookDocumentParams = DidOpenNotebookDocumentParams 
  { {-|
  The notebook document that got opened.
  -}
  _notebookDocument :: Language.LSP.Protocol.Internal.Types.NotebookDocument.NotebookDocument
  , {-|
  The text documents that represent the content
  of a notebook cell.
  -}
  _cellTextDocuments :: [Language.LSP.Protocol.Internal.Types.TextDocumentItem.TextDocumentItem]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidOpenNotebookDocumentParams)

instance Aeson.ToJSON DidOpenNotebookDocumentParams where
  toJSON (DidOpenNotebookDocumentParams arg0 arg1) = Aeson.object $ concat $  [["notebookDocument" Aeson..= arg0]
    ,["cellTextDocuments" Aeson..= arg1]]

instance Aeson.FromJSON DidOpenNotebookDocumentParams where
  parseJSON = Aeson.withObject "DidOpenNotebookDocumentParams" $ \arg -> DidOpenNotebookDocumentParams <$> arg Aeson..: "notebookDocument" <*> arg Aeson..: "cellTextDocuments"
