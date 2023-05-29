-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticTag where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
The diagnostic tags.

@since 3.15.0
-}
data DiagnosticTag = 
    {-|
  Unused or unnecessary code.

  Clients are allowed to render diagnostics with this tag faded out instead of having
  an error squiggle.
  -}
  DiagnosticTag_Unnecessary
  | {-|
  Deprecated or obsolete code.

  Clients are allowed to rendered diagnostics with this tag strike through.
  -}
  DiagnosticTag_Deprecated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum DiagnosticTag Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum DiagnosticTag where
  knownValues = Data.Set.fromList [DiagnosticTag_Unnecessary
    ,DiagnosticTag_Deprecated]
  type EnumBaseType DiagnosticTag = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType DiagnosticTag_Unnecessary = 1
  toEnumBaseType DiagnosticTag_Deprecated = 2
  fromEnumBaseType 1 = pure DiagnosticTag_Unnecessary
  fromEnumBaseType 2 = pure DiagnosticTag_Deprecated
  fromEnumBaseType _ = Nothing

