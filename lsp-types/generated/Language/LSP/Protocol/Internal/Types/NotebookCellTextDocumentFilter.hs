{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookCellTextDocumentFilter where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
A notebook cell text document filter denotes a cell text
document by different properties.

@since 3.17.0
-}
data NotebookCellTextDocumentFilter = NotebookCellTextDocumentFilter 
  { {-|
  A filter that matches against the notebook
  containing the notebook cell. If a string
  value is provided it matches against the
  notebook type. '*' matches every notebook.
  -}
  _notebook :: (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter)
  , {-|
  A language id like `python`.

  Will be matched against the language id of the
  notebook cell document. '*' matches every language.
  -}
  _language :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookCellTextDocumentFilter)

instance Aeson.ToJSON NotebookCellTextDocumentFilter where
  toJSON (NotebookCellTextDocumentFilter arg0 arg1) = Aeson.object $ concat $  [["notebook" Aeson..= arg0]
    ,"language" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookCellTextDocumentFilter where
  parseJSON = Aeson.withObject "NotebookCellTextDocumentFilter" $ \arg -> NotebookCellTextDocumentFilter <$> arg Aeson..: "notebook" <*> arg Language.LSP.Protocol.Types.Common..:!? "language"
