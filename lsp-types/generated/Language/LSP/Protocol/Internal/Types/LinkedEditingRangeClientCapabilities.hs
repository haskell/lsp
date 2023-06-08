-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LinkedEditingRangeClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities for the linked editing range request.

@since 3.16.0
-}
data LinkedEditingRangeClientCapabilities = LinkedEditingRangeClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  return value for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON LinkedEditingRangeClientCapabilities where
  toJSON (LinkedEditingRangeClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON LinkedEditingRangeClientCapabilities where
  parseJSON = Aeson.withObject "LinkedEditingRangeClientCapabilities" $ \arg -> LinkedEditingRangeClientCapabilities <$> arg Aeson..:! "dynamicRegistration"