{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MonikerClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities specific to the moniker request.

@since 3.16.0
-}
data MonikerClientCapabilities = MonikerClientCapabilities 
  { {-|
  Whether moniker supports dynamic registration. If this is set to `true`
  the client supports the new `MonikerRegistrationOptions` return value
  for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON MonikerClientCapabilities)

instance Aeson.ToJSON MonikerClientCapabilities where
  toJSON (MonikerClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON MonikerClientCapabilities where
  parseJSON = Aeson.withObject "MonikerClientCapabilities" $ \arg -> MonikerClientCapabilities <$> arg Aeson..:! "dynamicRegistration"
