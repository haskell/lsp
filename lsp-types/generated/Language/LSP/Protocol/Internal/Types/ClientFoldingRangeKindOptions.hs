{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientFoldingRangeKindOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientFoldingRangeKindOptions = ClientFoldingRangeKindOptions 
  { {-|
  The folding range kind values the client supports. When this
  property exists the client also guarantees that it will
  handle values outside its set gracefully and falls back
  to a default value when unknown.
  -}
  valueSet :: (Maybe [Language.LSP.Protocol.Internal.Types.FoldingRangeKind.FoldingRangeKind])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientFoldingRangeKindOptions)

instance Aeson.ToJSON ClientFoldingRangeKindOptions where
  toJSON (ClientFoldingRangeKindOptions arg0) = Aeson.object $ concat $  ["valueSet" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientFoldingRangeKindOptions where
  parseJSON = Aeson.withObject "ClientFoldingRangeKindOptions" $ \arg -> ClientFoldingRangeKindOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "valueSet"
