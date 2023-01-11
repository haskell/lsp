-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticReport where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common

{-|
A workspace diagnostic report.

@since 3.17.0

-}
data WorkspaceDiagnosticReport = WorkspaceDiagnosticReport 
  { {-|

  -}
  _items :: [Language.LSP.Protocol.Internal.Types.WorkspaceDocumentDiagnosticReport.WorkspaceDocumentDiagnosticReport]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON WorkspaceDiagnosticReport where
  toJSON (WorkspaceDiagnosticReport arg0) = Aeson.object $ concat $  [["items" Aeson..= arg0]]

instance Aeson.FromJSON WorkspaceDiagnosticReport where
  parseJSON = Aeson.withObject "WorkspaceDiagnosticReport" $ \arg -> WorkspaceDiagnosticReport <$> arg Aeson..: "items"