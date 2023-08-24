{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentColorClientCapabilities where

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
data DocumentColorClientCapabilities = DocumentColorClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `DocumentColorRegistrationOptions` return value
  for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentColorClientCapabilities)

instance Aeson.ToJSON DocumentColorClientCapabilities where
  toJSON (DocumentColorClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DocumentColorClientCapabilities where
  parseJSON = Aeson.withObject "DocumentColorClientCapabilities" $ \arg -> DocumentColorClientCapabilities <$> arg Aeson..:! "dynamicRegistration"
