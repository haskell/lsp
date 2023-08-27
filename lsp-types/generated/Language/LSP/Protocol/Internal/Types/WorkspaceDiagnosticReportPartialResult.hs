{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReportPartialResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common

{-|
A partial result for a workspace diagnostic report.

@since 3.17.0
-}
data WorkspaceDiagnosticReportPartialResult = WorkspaceDiagnosticReportPartialResult 
  { {-|

  -}
  _items :: [Language.LSP.Protocol.Internal.Types.WorkspaceDocumentDiagnosticReport.WorkspaceDocumentDiagnosticReport]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceDiagnosticReportPartialResult)

instance Aeson.ToJSON WorkspaceDiagnosticReportPartialResult where
  toJSON (WorkspaceDiagnosticReportPartialResult arg0) = Aeson.object $ concat $  [["items" Aeson..= arg0]]

instance Aeson.FromJSON WorkspaceDiagnosticReportPartialResult where
  parseJSON = Aeson.withObject "WorkspaceDiagnosticReportPartialResult" $ \arg -> WorkspaceDiagnosticReportPartialResult <$> arg Aeson..: "items"
