{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionTriggerKind where

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
How a completion was triggered
-}
data CompletionTriggerKind = 
    {-|
  Completion was triggered by typing an identifier (24x7 code
  complete), manual invocation (e.g Ctrl+Space) or via API.
  -}
  CompletionTriggerKind_Invoked
  | {-|
  Completion was triggered by a trigger character specified by
  the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
  -}
  CompletionTriggerKind_TriggerCharacter
  | {-|
  Completion was re-triggered as current completion list is incomplete
  -}
  CompletionTriggerKind_TriggerForIncompleteCompletions
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CompletionTriggerKind)
  deriving Pretty via (ViaJSON CompletionTriggerKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CompletionTriggerKind where
  knownValues = Data.Set.fromList [CompletionTriggerKind_Invoked
    ,CompletionTriggerKind_TriggerCharacter
    ,CompletionTriggerKind_TriggerForIncompleteCompletions]
  type EnumBaseType CompletionTriggerKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType CompletionTriggerKind_Invoked = 1
  toEnumBaseType CompletionTriggerKind_TriggerCharacter = 2
  toEnumBaseType CompletionTriggerKind_TriggerForIncompleteCompletions = 3
  fromEnumBaseType 1 = pure CompletionTriggerKind_Invoked
  fromEnumBaseType 2 = pure CompletionTriggerKind_TriggerCharacter
  fromEnumBaseType 3 = pure CompletionTriggerKind_TriggerForIncompleteCompletions
  fromEnumBaseType _ = Nothing


