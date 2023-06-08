-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceDocumentDiagnosticReport where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceUnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common

{-|
A workspace diagnostic document report.

@since 3.17.0
-}
newtype WorkspaceDocumentDiagnosticReport = WorkspaceDocumentDiagnosticReport (Language.LSP.Protocol.Internal.Types.WorkspaceFullDocumentDiagnosticReport.WorkspaceFullDocumentDiagnosticReport Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.WorkspaceUnchangedDocumentDiagnosticReport.WorkspaceUnchangedDocumentDiagnosticReport)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)