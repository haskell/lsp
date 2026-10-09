{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineCompletionClientCapabilities where

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
Client capabilities specific to inline completions.

@since 3.18.0
-}
data InlineCompletionClientCapabilities = InlineCompletionClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration for inline completion providers.
  -}
  _dynamicRegistration :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineCompletionClientCapabilities)

instance Aeson.ToJSON InlineCompletionClientCapabilities where
  toJSON (InlineCompletionClientCapabilities arg0) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON InlineCompletionClientCapabilities where
  parseJSON = Aeson.withObject "InlineCompletionClientCapabilities" $ \arg -> InlineCompletionClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration"
