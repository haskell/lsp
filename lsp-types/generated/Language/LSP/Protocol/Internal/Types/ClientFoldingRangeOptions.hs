{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientFoldingRangeOptions where

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
@since 3.18.0
@proposed
-}
data ClientFoldingRangeOptions = ClientFoldingRangeOptions 
  { {-|
  If set, the client signals that it supports setting collapsedText on
  folding ranges to display custom labels instead of the default text.

  @since 3.17.0
  -}
  _collapsedText :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientFoldingRangeOptions)

instance Aeson.ToJSON ClientFoldingRangeOptions where
  toJSON (ClientFoldingRangeOptions arg0) = Aeson.object $ concat $  ["collapsedText" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientFoldingRangeOptions where
  parseJSON = Aeson.withObject "ClientFoldingRangeOptions" $ \arg -> ClientFoldingRangeOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "collapsedText"
