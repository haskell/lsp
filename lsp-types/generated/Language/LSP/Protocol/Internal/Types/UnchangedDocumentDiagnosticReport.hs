-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.UnchangedDocumentDiagnosticReport where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons

{-|
A diagnostic report indicating that the last returned
report is still accurate.

@since 3.17.0
-}
data UnchangedDocumentDiagnosticReport = UnchangedDocumentDiagnosticReport 
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
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON UnchangedDocumentDiagnosticReport where
  toJSON (UnchangedDocumentDiagnosticReport arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["resultId" Aeson..= arg1]]

instance Aeson.FromJSON UnchangedDocumentDiagnosticReport where
  parseJSON = Aeson.withObject "UnchangedDocumentDiagnosticReport" $ \arg -> UnchangedDocumentDiagnosticReport <$> arg Aeson..: "kind" <*> arg Aeson..: "resultId"
