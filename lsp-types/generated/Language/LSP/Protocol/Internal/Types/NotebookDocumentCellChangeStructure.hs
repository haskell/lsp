{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChangeStructure where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentItem
import qualified Language.LSP.Protocol.Types.Common

{-|
Structural changes to cells in a notebook document.

@since 3.18.0
@proposed
-}
data NotebookDocumentCellChangeStructure = NotebookDocumentCellChangeStructure 
  { {-|
  The change to the cell array.
  -}
  array :: Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange.NotebookCellArrayChange
  , {-|
  Additional opened cell text documents.
  -}
  didOpen :: (Maybe [Language.LSP.Protocol.Internal.Types.TextDocumentItem.TextDocumentItem])
  , {-|
  Additional closed cell text documents.
  -}
  didClose :: (Maybe [Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentCellChangeStructure)

instance Aeson.ToJSON NotebookDocumentCellChangeStructure where
  toJSON (NotebookDocumentCellChangeStructure arg0 arg1 arg2) = Aeson.object $ concat $  [["array" Aeson..= arg0]
    ,"didOpen" Language.LSP.Protocol.Types.Common..=? arg1
    ,"didClose" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON NotebookDocumentCellChangeStructure where
  parseJSON = Aeson.withObject "NotebookDocumentCellChangeStructure" $ \arg -> NotebookDocumentCellChangeStructure <$> arg Aeson..: "array" <*> arg Language.LSP.Protocol.Types.Common..:!? "didOpen" <*> arg Language.LSP.Protocol.Types.Common..:!? "didClose"
