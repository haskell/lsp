-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceUnchangedDocumentDiagnosticReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
An unchanged document diagnostic report for a workspace diagnostic result.

@since 3.17.0
-}
data WorkspaceUnchangedDocumentDiagnosticReport = WorkspaceUnchangedDocumentDiagnosticReport 
  { {-|
  A document diagnostic report indicating
  no changes to the last result. A server can
  only return `unchanged` if result ids are
  provided.
  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "unchanged")
  , {-|
  A result id which will be sent on the next
  diagnostic request for the same document.
  -}
  _resultId :: Data.Text.Text
  , {-|
  The URI for which diagnostic information is reported.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The version number for which the diagnostics are reported.
  If the document is not marked as open `null` can be provided.
  -}
  _version :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON WorkspaceUnchangedDocumentDiagnosticReport where
  toJSON (WorkspaceUnchangedDocumentDiagnosticReport arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["resultId" Aeson..= arg1]
    ,["uri" Aeson..= arg2]
    ,["version" Aeson..= arg3]]

instance Aeson.FromJSON WorkspaceUnchangedDocumentDiagnosticReport where
  parseJSON = Aeson.withObject "WorkspaceUnchangedDocumentDiagnosticReport" $ \arg -> WorkspaceUnchangedDocumentDiagnosticReport <$> arg Aeson..: "kind" <*> arg Aeson..: "resultId" <*> arg Aeson..: "uri" <*> arg Aeson..: "version"
