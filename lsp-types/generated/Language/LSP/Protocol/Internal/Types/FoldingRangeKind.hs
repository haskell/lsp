{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FoldingRangeKind where

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
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A set of predefined range kinds.
-}
data FoldingRangeKind = 
    {-|
  Folding range for a comment
  -}
  FoldingRangeKind_Comment
  | {-|
  Folding range for an import or include
  -}
  FoldingRangeKind_Imports
  | {-|
  Folding range for a region (e.g. `#region`)
  -}
  FoldingRangeKind_Region
  | FoldingRangeKind_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum FoldingRangeKind)
  deriving Pretty via (ViaJSON FoldingRangeKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum FoldingRangeKind where
  knownValues = Data.Set.fromList [FoldingRangeKind_Comment
    ,FoldingRangeKind_Imports
    ,FoldingRangeKind_Region]
  type EnumBaseType FoldingRangeKind = Data.Text.Text
  toEnumBaseType FoldingRangeKind_Comment = "comment"
  toEnumBaseType FoldingRangeKind_Imports = "imports"
  toEnumBaseType FoldingRangeKind_Region = "region"
  toEnumBaseType (FoldingRangeKind_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum FoldingRangeKind where
  fromOpenEnumBaseType "comment" = FoldingRangeKind_Comment
  fromOpenEnumBaseType "imports" = FoldingRangeKind_Imports
  fromOpenEnumBaseType "region" = FoldingRangeKind_Region
  fromOpenEnumBaseType arg = FoldingRangeKind_Custom arg


