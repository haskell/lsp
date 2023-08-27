{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlayHintClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Inlay hint client capabilities.

@since 3.17.0
-}
data InlayHintClientCapabilities = InlayHintClientCapabilities 
  { {-|
  Whether inlay hints support dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Indicates which properties a client can resolve lazily on an inlay
  hint.
  -}
  _resolveSupport :: (Maybe (Row.Rec ("properties" Row..== [Data.Text.Text] Row..+ Row.Empty)))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlayHintClientCapabilities)

instance Aeson.ToJSON InlayHintClientCapabilities where
  toJSON (InlayHintClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON InlayHintClientCapabilities where
  parseJSON = Aeson.withObject "InlayHintClientCapabilities" $ \arg -> InlayHintClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "resolveSupport"
