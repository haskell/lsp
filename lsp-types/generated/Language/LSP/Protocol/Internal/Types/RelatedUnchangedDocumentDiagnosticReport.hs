{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RelatedUnchangedDocumentDiagnosticReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Map
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
An unchanged diagnostic report with a set of related documents.

@since 3.17.0
-}
data RelatedUnchangedDocumentDiagnosticReport = RelatedUnchangedDocumentDiagnosticReport 
  { {-|
  A document diagnostic report indicating
  no changes to the last result. A server can
  only return `unchanged` if result ids are
  provided.
  -}
  kind :: (Language.LSP.Protocol.Types.Singletons.AString "unchanged")
  , {-|
  A result id which will be sent on the next
  diagnostic request for the same document.
  -}
  resultId :: Data.Text.Text
  , {-|
  Diagnostics of related documents. This information is useful
  in programming languages where code in a file A can generate
  diagnostics in a file B which A depends on. An example of
  such a language is C/C++ where marco definitions in a file
  a.cpp and result in errors in a header file b.hpp.

  @since 3.17.0
  -}
  relatedDocuments :: (Maybe (Data.Map.Map Language.LSP.Protocol.Types.Uri.Uri (Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport.FullDocumentDiagnosticReport Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport.UnchangedDocumentDiagnosticReport)))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RelatedUnchangedDocumentDiagnosticReport)

instance Aeson.ToJSON RelatedUnchangedDocumentDiagnosticReport where
  toJSON (RelatedUnchangedDocumentDiagnosticReport arg0 arg1 arg2) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["resultId" Aeson..= arg1]
    ,"relatedDocuments" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON RelatedUnchangedDocumentDiagnosticReport where
  parseJSON = Aeson.withObject "RelatedUnchangedDocumentDiagnosticReport" $ \arg -> RelatedUnchangedDocumentDiagnosticReport <$> arg Aeson..: "kind" <*> arg Aeson..: "resultId" <*> arg Language.LSP.Protocol.Types.Common..:!? "relatedDocuments"
