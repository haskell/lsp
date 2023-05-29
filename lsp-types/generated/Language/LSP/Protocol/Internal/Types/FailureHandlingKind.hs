-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FailureHandlingKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|

-}
data FailureHandlingKind = 
    {-|
  Applying the workspace change is simply aborted if one of the changes provided
  fails. All operations executed before the failing operation stay executed.
  -}
  FailureHandlingKind_Abort
  | {-|
  All operations are executed transactional. That means they either all
  succeed or no changes at all are applied to the workspace.
  -}
  FailureHandlingKind_Transactional
  | {-|
  If the workspace edit contains only textual file changes they are executed transactional.
  If resource changes (create, rename or delete file) are part of the change the failure
  handling strategy is abort.
  -}
  FailureHandlingKind_TextOnlyTransactional
  | {-|
  The client tries to undo the operations already executed. But there is no
  guarantee that this is succeeding.
  -}
  FailureHandlingKind_Undo
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum FailureHandlingKind Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum FailureHandlingKind where
  knownValues = Data.Set.fromList [FailureHandlingKind_Abort
    ,FailureHandlingKind_Transactional
    ,FailureHandlingKind_TextOnlyTransactional
    ,FailureHandlingKind_Undo]
  type EnumBaseType FailureHandlingKind = Data.Text.Text
  toEnumBaseType FailureHandlingKind_Abort = "abort"
  toEnumBaseType FailureHandlingKind_Transactional = "transactional"
  toEnumBaseType FailureHandlingKind_TextOnlyTransactional = "textOnlyTransactional"
  toEnumBaseType FailureHandlingKind_Undo = "undo"
  fromEnumBaseType "abort" = pure FailureHandlingKind_Abort
  fromEnumBaseType "transactional" = pure FailureHandlingKind_Transactional
  fromEnumBaseType "textOnlyTransactional" = pure FailureHandlingKind_TextOnlyTransactional
  fromEnumBaseType "undo" = pure FailureHandlingKind_Undo
  fromEnumBaseType _ = Nothing

