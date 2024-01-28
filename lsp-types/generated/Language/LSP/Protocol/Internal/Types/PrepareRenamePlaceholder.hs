{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PrepareRenamePlaceholder where

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
@since 3.18.0
@proposed
-}
data PrepareRenamePlaceholder = PrepareRenamePlaceholder 
  { {-|

  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|

  -}
  _placeholder :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON PrepareRenamePlaceholder)

instance Aeson.ToJSON PrepareRenamePlaceholder where
  toJSON (PrepareRenamePlaceholder arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["placeholder" Aeson..= arg1]]

instance Aeson.FromJSON PrepareRenamePlaceholder where
  parseJSON = Aeson.withObject "PrepareRenamePlaceholder" $ \arg -> PrepareRenamePlaceholder <$> arg Aeson..: "range" <*> arg Aeson..: "placeholder"
