-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionTriggerKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
The reason why code actions were requested.

@since 3.17.0

-}
data CodeActionTriggerKind = 
    {-|
  Code actions were explicitly requested by the user or by an extension.

  -}
  CodeActionTriggerKind_Invoked
  | {-|
  Code actions were requested automatically.

  This typically happens when current selection in a file changes, but can
  also be triggered when file content changes.

  -}
  CodeActionTriggerKind_Automatic
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CodeActionTriggerKind Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CodeActionTriggerKind where
  knownValues = Data.Set.fromList [CodeActionTriggerKind_Invoked
    ,CodeActionTriggerKind_Automatic]
  type EnumBaseType CodeActionTriggerKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType CodeActionTriggerKind_Invoked = 1
  toEnumBaseType CodeActionTriggerKind_Automatic = 2
  fromEnumBaseType 1 = pure CodeActionTriggerKind_Invoked
  fromEnumBaseType 2 = pure CodeActionTriggerKind_Automatic
  fromEnumBaseType _ = Nothing

