-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeConfigurationParams where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a change configuration notification.

-}
data DidChangeConfigurationParams = DidChangeConfigurationParams 
  { {-|
  The actual changed settings

  -}
  _settings :: Data.Aeson.Value
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DidChangeConfigurationParams where
  toJSON (DidChangeConfigurationParams arg0) = Aeson.object $ concat $  [["settings" Aeson..= arg0]]

instance Aeson.FromJSON DidChangeConfigurationParams where
  parseJSON = Aeson.withObject "DidChangeConfigurationParams" $ \arg -> DidChangeConfigurationParams <$> arg Aeson..: "settings"