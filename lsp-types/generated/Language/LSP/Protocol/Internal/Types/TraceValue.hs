{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TraceValue where

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

-}
data TraceValue = 
    {-|
  Turn tracing off.
  -}
  TraceValue_Off
  | {-|
  Trace messages only.
  -}
  TraceValue_Messages
  | {-|
  Verbose message tracing.
  -}
  TraceValue_Verbose
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum TraceValue)
  deriving Pretty via (ViaJSON TraceValue)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum TraceValue where
  knownValues = Data.Set.fromList [TraceValue_Off
    ,TraceValue_Messages
    ,TraceValue_Verbose]
  type EnumBaseType TraceValue = Data.Text.Text
  toEnumBaseType TraceValue_Off = "off"
  toEnumBaseType TraceValue_Messages = "messages"
  toEnumBaseType TraceValue_Verbose = "verbose"
  fromEnumBaseType "off" = pure TraceValue_Off
  fromEnumBaseType "messages" = pure TraceValue_Messages
  fromEnumBaseType "verbose" = pure TraceValue_Verbose
  fromEnumBaseType _ = Nothing


