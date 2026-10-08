{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ApplyKind where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Defines how values from a set of defaults and an individual item will be
merged.

@since 3.18.0
-}
data ApplyKind = 
    {-|
  The value from the individual item (if provided and not `null`) will be
  used instead of the default.
  -}
  ApplyKind_Replace
  | {-|
  The value from the item will be merged with the default.

  The specific rules for mergeing values are defined against each field
  that supports merging.
  -}
  ApplyKind_Merge
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum ApplyKind)
  deriving Pretty via (ViaJSON ApplyKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum ApplyKind where
  knownValues = Data.Set.fromList [ApplyKind_Replace,ApplyKind_Merge]
  type EnumBaseType ApplyKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType ApplyKind_Replace = 1
  toEnumBaseType ApplyKind_Merge = 2
  fromEnumBaseType 1 = pure ApplyKind_Replace
  fromEnumBaseType 2 = pure ApplyKind_Merge
  fromEnumBaseType _ = Nothing


