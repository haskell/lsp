{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SelectionRange where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A selection range represents a part of a selection hierarchy. A selection range
may have a parent selection range that contains it.
-}
data SelectionRange = SelectionRange 
  { {-|
  The `Range` of this selection range.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
  -}
  _parent :: (Maybe Language.LSP.Protocol.Internal.Types.SelectionRange.SelectionRange)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SelectionRange)

instance Aeson.ToJSON SelectionRange where
  toJSON (SelectionRange arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"parent" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON SelectionRange where
  parseJSON = Aeson.withObject "SelectionRange" $ \arg -> SelectionRange <$> arg Aeson..: "range" <*> arg Aeson..:! "parent"
