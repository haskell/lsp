-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Color where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a color in RGBA space.
-}
data Color = Color 
  { {-|
  The red component of this color in the range [0-1].
  -}
  _red :: Float
  , {-|
  The green component of this color in the range [0-1].
  -}
  _green :: Float
  , {-|
  The blue component of this color in the range [0-1].
  -}
  _blue :: Float
  , {-|
  The alpha component of this color in the range [0-1].
  -}
  _alpha :: Float
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON Color where
  toJSON (Color arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["red" Aeson..= arg0]
    ,["green" Aeson..= arg1]
    ,["blue" Aeson..= arg2]
    ,["alpha" Aeson..= arg3]]

instance Aeson.FromJSON Color where
  parseJSON = Aeson.withObject "Color" $ \arg -> Color <$> arg Aeson..: "red" <*> arg Aeson..: "green" <*> arg Aeson..: "blue" <*> arg Aeson..: "alpha"
