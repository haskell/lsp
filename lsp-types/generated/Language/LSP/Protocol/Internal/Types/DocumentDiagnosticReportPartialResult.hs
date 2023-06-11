-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentDiagnosticReportPartialResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Map
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A partial result for a document diagnostic report.

@since 3.17.0
-}
data DocumentDiagnosticReportPartialResult = DocumentDiagnosticReportPartialResult 
  { {-|

  -}
  _relatedDocuments :: (Data.Map.Map Language.LSP.Protocol.Types.Uri.Uri (Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport.FullDocumentDiagnosticReport Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport.UnchangedDocumentDiagnosticReport))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON DocumentDiagnosticReportPartialResult where
  toJSON (DocumentDiagnosticReportPartialResult arg0) = Aeson.object $ concat $  [["relatedDocuments" Aeson..= arg0]]

instance Aeson.FromJSON DocumentDiagnosticReportPartialResult where
  parseJSON = Aeson.withObject "DocumentDiagnosticReportPartialResult" $ \arg -> DocumentDiagnosticReportPartialResult <$> arg Aeson..: "relatedDocuments"
