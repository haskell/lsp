-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MessageType where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
The message type
-}
data MessageType = 
    {-|
  An error message.
  -}
  MessageType_Error
  | {-|
  A warning message.
  -}
  MessageType_Warning
  | {-|
  An information message.
  -}
  MessageType_Info
  | {-|
  A log message.
  -}
  MessageType_Log
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum MessageType Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum MessageType where
  knownValues = Data.Set.fromList [MessageType_Error
    ,MessageType_Warning
    ,MessageType_Info
    ,MessageType_Log]
  type EnumBaseType MessageType = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType MessageType_Error = 1
  toEnumBaseType MessageType_Warning = 2
  toEnumBaseType MessageType_Info = 3
  toEnumBaseType MessageType_Log = 4
  fromEnumBaseType 1 = pure MessageType_Error
  fromEnumBaseType 2 = pure MessageType_Warning
  fromEnumBaseType 3 = pure MessageType_Info
  fromEnumBaseType 4 = pure MessageType_Log
  fromEnumBaseType _ = Nothing


