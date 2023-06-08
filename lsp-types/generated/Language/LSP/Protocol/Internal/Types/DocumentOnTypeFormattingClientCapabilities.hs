-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities of a `DocumentOnTypeFormattingRequest`.
-}
data DocumentOnTypeFormattingClientCapabilities = DocumentOnTypeFormattingClientCapabilities 
  { {-|
  Whether on type formatting supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentOnTypeFormattingClientCapabilities where
  toJSON (DocumentOnTypeFormattingClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DocumentOnTypeFormattingClientCapabilities where
  parseJSON = Aeson.withObject "DocumentOnTypeFormattingClientCapabilities" $ \arg -> DocumentOnTypeFormattingClientCapabilities <$> arg Aeson..:! "dynamicRegistration"
