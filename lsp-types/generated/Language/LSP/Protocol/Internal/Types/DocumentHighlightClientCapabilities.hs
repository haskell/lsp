-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentHighlightClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `DocumentHighlightRequest`.
-}
data DocumentHighlightClientCapabilities = DocumentHighlightClientCapabilities 
  { {-|
  Whether document highlight supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON DocumentHighlightClientCapabilities where
  toJSON (DocumentHighlightClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DocumentHighlightClientCapabilities where
  parseJSON = Aeson.withObject "DocumentHighlightClientCapabilities" $ \arg -> DocumentHighlightClientCapabilities <$> arg Aeson..:! "dynamicRegistration"
