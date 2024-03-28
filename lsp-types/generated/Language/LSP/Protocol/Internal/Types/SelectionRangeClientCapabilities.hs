{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SelectionRangeClientCapabilities where

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
data SelectionRangeClientCapabilities = SelectionRangeClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration for selection range providers. If this is set to `true`
  the client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server
  capability as well.
  -}
  dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SelectionRangeClientCapabilities)

instance Aeson.ToJSON SelectionRangeClientCapabilities where
  toJSON (SelectionRangeClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON SelectionRangeClientCapabilities where
  parseJSON = Aeson.withObject "SelectionRangeClientCapabilities" $ \arg -> SelectionRangeClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration"
