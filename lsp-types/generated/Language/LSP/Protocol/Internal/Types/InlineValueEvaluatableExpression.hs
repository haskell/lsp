{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueEvaluatableExpression where

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
Provide an inline value through an expression evaluation.
If only a range is specified, the expression will be extracted from the underlying document.
An optional expression can be used to override the extracted expression.

@since 3.17.0
-}
data InlineValueEvaluatableExpression = InlineValueEvaluatableExpression 
  { {-|
  The document range for which the inline value applies.
  The range is used to extract the evaluatable expression from the underlying document.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  If specified the expression overrides the extracted expression.
  -}
  _expression :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineValueEvaluatableExpression)

instance Aeson.ToJSON InlineValueEvaluatableExpression where
  toJSON (InlineValueEvaluatableExpression arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"expression" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON InlineValueEvaluatableExpression where
  parseJSON = Aeson.withObject "InlineValueEvaluatableExpression" $ \arg -> InlineValueEvaluatableExpression <$> arg Aeson..: "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "expression"
