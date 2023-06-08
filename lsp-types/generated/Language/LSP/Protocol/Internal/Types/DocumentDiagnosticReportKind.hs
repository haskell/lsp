-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
The document diagnostic report kinds.

@since 3.17.0
-}
data DocumentDiagnosticReportKind = 
    {-|
  A diagnostic report with a full
  set of problems.
  -}
  DocumentDiagnosticReportKind_Full
  | {-|
  A report indicating that the last
  returned report is still accurate.
  -}
  DocumentDiagnosticReportKind_Unchanged
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum DocumentDiagnosticReportKind Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum DocumentDiagnosticReportKind where
  knownValues = Data.Set.fromList [DocumentDiagnosticReportKind_Full
    ,DocumentDiagnosticReportKind_Unchanged]
  type EnumBaseType DocumentDiagnosticReportKind = Data.Text.Text
  toEnumBaseType DocumentDiagnosticReportKind_Full = "full"
  toEnumBaseType DocumentDiagnosticReportKind_Unchanged = "unchanged"
  fromEnumBaseType "full" = pure DocumentDiagnosticReportKind_Full
  fromEnumBaseType "unchanged" = pure DocumentDiagnosticReportKind_Unchanged
  fromEnumBaseType _ = Nothing


