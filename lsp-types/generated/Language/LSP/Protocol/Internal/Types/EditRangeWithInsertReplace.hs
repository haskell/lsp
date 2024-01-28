{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.EditRangeWithInsertReplace where

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
Edit range variant that includes ranges for insert and replace operations.

@since 3.18.0
@proposed
-}
data EditRangeWithInsertReplace = EditRangeWithInsertReplace 
  { {-|

  -}
  _insert :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|

  -}
  _replace :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON EditRangeWithInsertReplace)

instance Aeson.ToJSON EditRangeWithInsertReplace where
  toJSON (EditRangeWithInsertReplace arg0 arg1) = Aeson.object $ concat $  [["insert" Aeson..= arg0]
    ,["replace" Aeson..= arg1]]

instance Aeson.FromJSON EditRangeWithInsertReplace where
  parseJSON = Aeson.withObject "EditRangeWithInsertReplace" $ \arg -> EditRangeWithInsertReplace <$> arg Aeson..: "insert" <*> arg Aeson..: "replace"
