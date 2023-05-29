-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
The client capabilities of a `ExecuteCommandRequest`.

-}
data ExecuteCommandClientCapabilities = ExecuteCommandClientCapabilities 
  { {-|
  Execute command supports dynamic registration.

  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON ExecuteCommandClientCapabilities where
  toJSON (ExecuteCommandClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ExecuteCommandClientCapabilities where
  parseJSON = Aeson.withObject "ExecuteCommandClientCapabilities" $ \arg -> ExecuteCommandClientCapabilities <$> arg Aeson..:! "dynamicRegistration"