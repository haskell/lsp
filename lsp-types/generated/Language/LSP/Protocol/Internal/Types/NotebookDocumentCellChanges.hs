{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChanges where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookCell
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChangeStructure
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentCellContentChanges
import qualified Language.LSP.Protocol.Types.Common

{-|
Cell changes to a notebook document.

@since 3.18.0
@proposed
-}
data NotebookDocumentCellChanges = NotebookDocumentCellChanges 
  { {-|
  Changes to the cell structure to add or
  remove cells.
  -}
  _structure :: (Maybe Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChangeStructure.NotebookDocumentCellChangeStructure)
  , {-|
  Changes to notebook cells properties like its
  kind, execution summary or metadata.
  -}
  _data_ :: (Maybe [Language.LSP.Protocol.Internal.Types.NotebookCell.NotebookCell])
  , {-|
  Changes to the text content of notebook cells.
  -}
  _textContent :: (Maybe [Language.LSP.Protocol.Internal.Types.NotebookDocumentCellContentChanges.NotebookDocumentCellContentChanges])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentCellChanges)

instance Aeson.ToJSON NotebookDocumentCellChanges where
  toJSON (NotebookDocumentCellChanges arg0 arg1 arg2) = Aeson.object $ concat $  ["structure" Language.LSP.Protocol.Types.Common..=? arg0
    ,"data" Language.LSP.Protocol.Types.Common..=? arg1
    ,"textContent" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON NotebookDocumentCellChanges where
  parseJSON = Aeson.withObject "NotebookDocumentCellChanges" $ \arg -> NotebookDocumentCellChanges <$> arg Language.LSP.Protocol.Types.Common..:!? "structure" <*> arg Language.LSP.Protocol.Types.Common..:!? "data" <*> arg Language.LSP.Protocol.Types.Common..:!? "textContent"
