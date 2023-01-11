-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ConfigurationParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ConfigurationItem
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a configuration request.

-}
data ConfigurationParams = ConfigurationParams 
  { {-|

  -}
  _items :: [Language.LSP.Protocol.Internal.Types.ConfigurationItem.ConfigurationItem]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ConfigurationParams where
  toJSON (ConfigurationParams arg0) = Aeson.object $ concat $  [["items" Aeson..= arg0]]

instance Aeson.FromJSON ConfigurationParams where
  parseJSON = Aeson.withObject "ConfigurationParams" $ \arg -> ConfigurationParams <$> arg Aeson..: "items"