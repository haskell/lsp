-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyClientCapabilities where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0

-}
data CallHierarchyClientCapabilities = CallHierarchyClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  return value for the corresponding server capability as well.

  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CallHierarchyClientCapabilities where
  toJSON (CallHierarchyClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON CallHierarchyClientCapabilities where
  parseJSON = Aeson.withObject "CallHierarchyClientCapabilities" $ \arg -> CallHierarchyClientCapabilities <$> arg Aeson..:! "dynamicRegistration"