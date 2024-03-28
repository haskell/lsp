{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookCell
import qualified Language.LSP.Protocol.Types.Common

{-|
A change describing how to move a `NotebookCell`
array from state S to S'.

@since 3.17.0
-}
data NotebookCellArrayChange = NotebookCellArrayChange 
  { {-|
  The start oftest of the cell that changed.
  -}
  start :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The deleted cells
  -}
  deleteCount :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The new cells, if any
  -}
  cells :: (Maybe [Language.LSP.Protocol.Internal.Types.NotebookCell.NotebookCell])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookCellArrayChange)

instance Aeson.ToJSON NotebookCellArrayChange where
  toJSON (NotebookCellArrayChange arg0 arg1 arg2) = Aeson.object $ concat $  [["start" Aeson..= arg0]
    ,["deleteCount" Aeson..= arg1]
    ,"cells" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON NotebookCellArrayChange where
  parseJSON = Aeson.withObject "NotebookCellArrayChange" $ \arg -> NotebookCellArrayChange <$> arg Aeson..: "start" <*> arg Aeson..: "deleteCount" <*> arg Language.LSP.Protocol.Types.Common..:!? "cells"
