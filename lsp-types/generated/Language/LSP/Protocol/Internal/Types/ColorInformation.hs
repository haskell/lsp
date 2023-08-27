{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ColorInformation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Color
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a color range from a document.
-}
data ColorInformation = ColorInformation 
  { {-|
  The range in the document where this color appears.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The actual color value for this color range.
  -}
  _color :: Language.LSP.Protocol.Internal.Types.Color.Color
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ColorInformation)

instance Aeson.ToJSON ColorInformation where
  toJSON (ColorInformation arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["color" Aeson..= arg1]]

instance Aeson.FromJSON ColorInformation where
  parseJSON = Aeson.withObject "ColorInformation" $ \arg -> ColorInformation <$> arg Aeson..: "range" <*> arg Aeson..: "color"
