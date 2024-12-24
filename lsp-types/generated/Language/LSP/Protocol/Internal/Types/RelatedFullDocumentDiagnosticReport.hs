{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RelatedFullDocumentDiagnosticReport where

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
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
A full diagnostic report with a set of related documents.

@since 3.17.0
-}
data RelatedFullDocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport 
  { {-|
  A full document diagnostic report.
  -}
  kind :: (Language.LSP.Protocol.Types.Singletons.AString "full")
  , {-|
  An optional result id. If provided it will
  be sent on the next diagnostic request for the
  same document.
  -}
  resultId :: (Maybe Data.Text.Text)
  , {-|
  The actual items.
  -}
  items :: [Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic]
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
  deriving Pretty via (ViaJSON RelatedFullDocumentDiagnosticReport)

instance Aeson.ToJSON RelatedFullDocumentDiagnosticReport where
  toJSON (RelatedFullDocumentDiagnosticReport arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,"resultId" Language.LSP.Protocol.Types.Common..=? arg1
    ,["items" Aeson..= arg2]
    ,"relatedDocuments" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON RelatedFullDocumentDiagnosticReport where
  parseJSON = Aeson.withObject "RelatedFullDocumentDiagnosticReport" $ \arg -> RelatedFullDocumentDiagnosticReport <$> arg Aeson..: "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "resultId" <*> arg Aeson..: "items" <*> arg Language.LSP.Protocol.Types.Common..:!? "relatedDocuments"
