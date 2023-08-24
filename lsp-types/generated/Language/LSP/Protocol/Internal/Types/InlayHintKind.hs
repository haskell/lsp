-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlayHintKind where

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
Inlay hint kinds.

@since 3.17.0
-}
data InlayHintKind = 
    {-|
  An inlay hint that for a type annotation.
  -}
  InlayHintKind_Type
  | {-|
  An inlay hint that is for a parameter.
  -}
  InlayHintKind_Parameter
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum InlayHintKind Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON InlayHintKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum InlayHintKind where
  knownValues = Data.Set.fromList [InlayHintKind_Type,InlayHintKind_Parameter]
  type EnumBaseType InlayHintKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType InlayHintKind_Type = 1
  toEnumBaseType InlayHintKind_Parameter = 2
  fromEnumBaseType 1 = pure InlayHintKind_Type
  fromEnumBaseType 2 = pure InlayHintKind_Parameter
  fromEnumBaseType _ = Nothing


