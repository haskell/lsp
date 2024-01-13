{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities where

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

-}
data DidChangeConfigurationClientCapabilities = DidChangeConfigurationClientCapabilities 
  { {-|
  Did change configuration notification supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeConfigurationClientCapabilities)

instance Aeson.ToJSON DidChangeConfigurationClientCapabilities where
  toJSON (DidChangeConfigurationClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DidChangeConfigurationClientCapabilities where
  parseJSON = Aeson.withObject "DidChangeConfigurationClientCapabilities" $ \arg -> DidChangeConfigurationClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration"
