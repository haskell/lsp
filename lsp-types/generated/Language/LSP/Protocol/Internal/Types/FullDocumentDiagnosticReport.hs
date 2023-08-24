-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FullDocumentDiagnosticReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons

{-|
A diagnostic report with a full set of problems.

@since 3.17.0
-}
data FullDocumentDiagnosticReport = FullDocumentDiagnosticReport 
  { {-|
  A full document diagnostic report.
  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "full")
  , {-|
  An optional result id. If provided it will
  be sent on the next diagnostic request for the
  same document.
  -}
  _resultId :: (Maybe Data.Text.Text)
  , {-|
  The actual items.
  -}
  _items :: [Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FullDocumentDiagnosticReport)

instance Aeson.ToJSON FullDocumentDiagnosticReport where
  toJSON (FullDocumentDiagnosticReport arg0 arg1 arg2) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,"resultId" Language.LSP.Protocol.Types.Common..=? arg1
    ,["items" Aeson..= arg2]]

instance Aeson.FromJSON FullDocumentDiagnosticReport where
  parseJSON = Aeson.withObject "FullDocumentDiagnosticReport" $ \arg -> FullDocumentDiagnosticReport <$> arg Aeson..: "kind" <*> arg Aeson..:! "resultId" <*> arg Aeson..: "items"
