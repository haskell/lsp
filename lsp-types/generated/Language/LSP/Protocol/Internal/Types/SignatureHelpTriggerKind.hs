{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpTriggerKind where

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
How a signature help was triggered.

@since 3.15.0
-}
data SignatureHelpTriggerKind = 
    {-|
  Signature help was invoked manually by the user or by a command.
  -}
  SignatureHelpTriggerKind_Invoked
  | {-|
  Signature help was triggered by a trigger character.
  -}
  SignatureHelpTriggerKind_TriggerCharacter
  | {-|
  Signature help was triggered by the cursor moving or by the document content changing.
  -}
  SignatureHelpTriggerKind_ContentChange
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum SignatureHelpTriggerKind)
  deriving Pretty via (ViaJSON SignatureHelpTriggerKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum SignatureHelpTriggerKind where
  knownValues = Data.Set.fromList [SignatureHelpTriggerKind_Invoked
    ,SignatureHelpTriggerKind_TriggerCharacter
    ,SignatureHelpTriggerKind_ContentChange]
  type EnumBaseType SignatureHelpTriggerKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType SignatureHelpTriggerKind_Invoked = 1
  toEnumBaseType SignatureHelpTriggerKind_TriggerCharacter = 2
  toEnumBaseType SignatureHelpTriggerKind_ContentChange = 3
  fromEnumBaseType 1 = pure SignatureHelpTriggerKind_Invoked
  fromEnumBaseType 2 = pure SignatureHelpTriggerKind_TriggerCharacter
  fromEnumBaseType 3 = pure SignatureHelpTriggerKind_ContentChange
  fromEnumBaseType _ = Nothing


