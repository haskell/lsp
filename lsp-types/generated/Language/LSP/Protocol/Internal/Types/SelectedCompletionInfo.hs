{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SelectedCompletionInfo where

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
Describes the currently selected completion item.

@since 3.18.0
-}
data SelectedCompletionInfo = SelectedCompletionInfo 
  { {-|
  The range that will be replaced if this completion item is accepted.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The text the range will be replaced with if this completion is accepted.
  -}
  _text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SelectedCompletionInfo)

instance Aeson.ToJSON SelectedCompletionInfo where
  toJSON (SelectedCompletionInfo arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["text" Aeson..= arg1]]

instance Aeson.FromJSON SelectedCompletionInfo where
  parseJSON = Aeson.withObject "SelectedCompletionInfo" $ \arg -> SelectedCompletionInfo <$> arg Aeson..: "range" <*> arg Aeson..: "text"
