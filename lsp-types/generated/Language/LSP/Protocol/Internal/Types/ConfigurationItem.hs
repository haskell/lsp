-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ConfigurationItem where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data ConfigurationItem = ConfigurationItem 
  { {-|
  The scope to get the configuration section for.

  -}
  _scopeUri :: (Maybe Data.Text.Text)
  , {-|
  The configuration section asked for.

  -}
  _section :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ConfigurationItem where
  toJSON (ConfigurationItem arg0 arg1) = Aeson.object $ concat $  ["scopeUri" Language.LSP.Protocol.Types.Common..=? arg0
    ,"section" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ConfigurationItem where
  parseJSON = Aeson.withObject "ConfigurationItem" $ \arg -> ConfigurationItem <$> arg Aeson..:! "scopeUri" <*> arg Aeson..:! "section"