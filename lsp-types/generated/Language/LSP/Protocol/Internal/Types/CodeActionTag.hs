{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionTag where

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
Code action tags are extra annotations that tweak the behavior of a code action.

@since 3.18.0
-}
data CodeActionTag = 
    {-|
  Marks the code action as LLM-generated.
  -}
  CodeActionTag_LLMGenerated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CodeActionTag)
  deriving Pretty via (ViaJSON CodeActionTag)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CodeActionTag where
  knownValues = Data.Set.fromList [CodeActionTag_LLMGenerated]
  type EnumBaseType CodeActionTag = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType CodeActionTag_LLMGenerated = 1
  fromEnumBaseType 1 = pure CodeActionTag_LLMGenerated
  fromEnumBaseType _ = Nothing


