{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LinkedEditingRanges where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
The result of a linked editing range request.

@since 3.16.0
-}
data LinkedEditingRanges = LinkedEditingRanges 
  { {-|
  A list of ranges that can be edited together. The ranges must have
  identical length and contain identical text content. The ranges cannot overlap.
  -}
  _ranges :: [Language.LSP.Protocol.Internal.Types.Range.Range]
  , {-|
  An optional word pattern (regular expression) that describes valid contents for
  the given ranges. If no pattern is provided, the client configuration's word
  pattern will be used.
  -}
  _wordPattern :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON LinkedEditingRanges)

instance Aeson.ToJSON LinkedEditingRanges where
  toJSON (LinkedEditingRanges arg0 arg1) = Aeson.object $ concat $  [["ranges" Aeson..= arg0]
    ,"wordPattern" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON LinkedEditingRanges where
  parseJSON = Aeson.withObject "LinkedEditingRanges" $ \arg -> LinkedEditingRanges <$> arg Aeson..: "ranges" <*> arg Language.LSP.Protocol.Types.Common..:!? "wordPattern"
