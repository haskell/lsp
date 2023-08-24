{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ImplementationClientCapabilities where

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
@since 3.6.0
-}
data ImplementationClientCapabilities = ImplementationClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `ImplementationRegistrationOptions` return value
  for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports additional metadata in the form of definition links.

  @since 3.14.0
  -}
  _linkSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ImplementationClientCapabilities)

instance Aeson.ToJSON ImplementationClientCapabilities where
  toJSON (ImplementationClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"linkSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ImplementationClientCapabilities where
  parseJSON = Aeson.withObject "ImplementationClientCapabilities" $ \arg -> ImplementationClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "linkSupport"
