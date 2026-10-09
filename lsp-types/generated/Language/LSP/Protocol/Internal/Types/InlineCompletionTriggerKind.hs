{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineCompletionTriggerKind where

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
Describes how an `InlineCompletionItemProvider` was triggered.

@since 3.18.0
-}
data InlineCompletionTriggerKind = 
    {-|
  Completion was triggered explicitly by a user gesture.
  -}
  InlineCompletionTriggerKind_Invoked
  | {-|
  Completion was triggered automatically while editing.
  -}
  InlineCompletionTriggerKind_Automatic
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum InlineCompletionTriggerKind)
  deriving Pretty via (ViaJSON InlineCompletionTriggerKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum InlineCompletionTriggerKind where
  knownValues = Data.Set.fromList [InlineCompletionTriggerKind_Invoked
    ,InlineCompletionTriggerKind_Automatic]
  type EnumBaseType InlineCompletionTriggerKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType InlineCompletionTriggerKind_Invoked = 1
  toEnumBaseType InlineCompletionTriggerKind_Automatic = 2
  fromEnumBaseType 1 = pure InlineCompletionTriggerKind_Invoked
  fromEnumBaseType 2 = pure InlineCompletionTriggerKind_Automatic
  fromEnumBaseType _ = Nothing


