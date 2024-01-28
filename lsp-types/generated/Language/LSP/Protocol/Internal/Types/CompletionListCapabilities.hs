{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionListCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
The client supports the following `CompletionList` specific
capabilities.

@since 3.17.0
-}
data CompletionListCapabilities = CompletionListCapabilities 
  { {-|
  The client supports the following itemDefaults on
  a completion list.

  The value lists the supported property names of the
  `CompletionList.itemDefaults` object. If omitted
  no properties are supported.

  @since 3.17.0
  -}
  _itemDefaults :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionListCapabilities)

instance Aeson.ToJSON CompletionListCapabilities where
  toJSON (CompletionListCapabilities arg0) = Aeson.object $ concat $  ["itemDefaults" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON CompletionListCapabilities where
  parseJSON = Aeson.withObject "CompletionListCapabilities" $ \arg -> CompletionListCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "itemDefaults"
