{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueVariableLookup where

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
Provide inline value through a variable lookup.
If only a range is specified, the variable name will be extracted from the underlying document.
An optional variable name can be used to override the extracted name.

@since 3.17.0
-}
data InlineValueVariableLookup = InlineValueVariableLookup 
  { {-|
  The document range for which the inline value applies.
  The range is used to extract the variable name from the underlying document.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  If specified the name of the variable to look up.
  -}
  variableName :: (Maybe Data.Text.Text)
  , {-|
  How to perform the lookup.
  -}
  caseSensitiveLookup :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineValueVariableLookup)

instance Aeson.ToJSON InlineValueVariableLookup where
  toJSON (InlineValueVariableLookup arg0 arg1 arg2) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"variableName" Language.LSP.Protocol.Types.Common..=? arg1
    ,["caseSensitiveLookup" Aeson..= arg2]]

instance Aeson.FromJSON InlineValueVariableLookup where
  parseJSON = Aeson.withObject "InlineValueVariableLookup" $ \arg -> InlineValueVariableLookup <$> arg Aeson..: "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "variableName" <*> arg Aeson..: "caseSensitiveLookup"
