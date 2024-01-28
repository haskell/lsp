{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithNotebook where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellLanguage
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data NotebookDocumentFilterWithNotebook = NotebookDocumentFilterWithNotebook 
  { {-|
  The notebook to be synced If a string
  value is provided it matches against the
  notebook type. '*' matches every notebook.
  -}
  _notebook :: (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter)
  , {-|
  The cells of the matching notebook to be synced.
  -}
  _cells :: (Maybe [Language.LSP.Protocol.Internal.Types.NotebookCellLanguage.NotebookCellLanguage])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentFilterWithNotebook)

instance Aeson.ToJSON NotebookDocumentFilterWithNotebook where
  toJSON (NotebookDocumentFilterWithNotebook arg0 arg1) = Aeson.object $ concat $  [["notebook" Aeson..= arg0]
    ,"cells" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookDocumentFilterWithNotebook where
  parseJSON = Aeson.withObject "NotebookDocumentFilterWithNotebook" $ \arg -> NotebookDocumentFilterWithNotebook <$> arg Aeson..: "notebook" <*> arg Language.LSP.Protocol.Types.Common..:!? "cells"
