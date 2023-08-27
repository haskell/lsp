{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticSeverity where

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
The diagnostic's severity.
-}
data DiagnosticSeverity = 
    {-|
  Reports an error.
  -}
  DiagnosticSeverity_Error
  | {-|
  Reports a warning.
  -}
  DiagnosticSeverity_Warning
  | {-|
  Reports an information.
  -}
  DiagnosticSeverity_Information
  | {-|
  Reports a hint.
  -}
  DiagnosticSeverity_Hint
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum DiagnosticSeverity Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON DiagnosticSeverity)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum DiagnosticSeverity where
  knownValues = Data.Set.fromList [DiagnosticSeverity_Error
    ,DiagnosticSeverity_Warning
    ,DiagnosticSeverity_Information
    ,DiagnosticSeverity_Hint]
  type EnumBaseType DiagnosticSeverity = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType DiagnosticSeverity_Error = 1
  toEnumBaseType DiagnosticSeverity_Warning = 2
  toEnumBaseType DiagnosticSeverity_Information = 3
  toEnumBaseType DiagnosticSeverity_Hint = 4
  fromEnumBaseType 1 = pure DiagnosticSeverity_Error
  fromEnumBaseType 2 = pure DiagnosticSeverity_Warning
  fromEnumBaseType 3 = pure DiagnosticSeverity_Information
  fromEnumBaseType 4 = pure DiagnosticSeverity_Hint
  fromEnumBaseType _ = Nothing


