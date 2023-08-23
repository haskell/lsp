-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.RelatedFullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.RelatedUnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common

{-|
The result of a document diagnostic pull request. A report can
either be a full report containing all diagnostics for the
requested document or an unchanged report indicating that nothing
has changed in terms of diagnostics in comparison to the last
pull request.

@since 3.17.0
-}
newtype DocumentDiagnosticReport = DocumentDiagnosticReport (Language.LSP.Protocol.Internal.Types.RelatedFullDocumentDiagnosticReport.RelatedFullDocumentDiagnosticReport Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.RelatedUnchangedDocumentDiagnosticReport.RelatedUnchangedDocumentDiagnosticReport)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentDiagnosticReport)
