-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TypeDefinitionClientCapabilities where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Since 3.6.0

-}
data TypeDefinitionClientCapabilities = TypeDefinitionClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `TypeDefinitionRegistrationOptions` return value
  for the corresponding server capability as well.

  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports additional metadata in the form of definition links.

  Since 3.14.0

  -}
  _linkSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON TypeDefinitionClientCapabilities where
  toJSON (TypeDefinitionClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"linkSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON TypeDefinitionClientCapabilities where
  parseJSON = Aeson.withObject "TypeDefinitionClientCapabilities" $ \arg -> TypeDefinitionClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "linkSupport"