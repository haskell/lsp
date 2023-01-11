-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DefinitionClientCapabilities where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `DefinitionRequest`.

-}
data DefinitionClientCapabilities = DefinitionClientCapabilities 
  { {-|
  Whether definition supports dynamic registration.

  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports additional metadata in the form of definition links.

  @since 3.14.0

  -}
  _linkSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DefinitionClientCapabilities where
  toJSON (DefinitionClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"linkSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DefinitionClientCapabilities where
  parseJSON = Aeson.withObject "DefinitionClientCapabilities" $ \arg -> DefinitionClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "linkSupport"