{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookCell where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ExecutionSummary
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellKind
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A notebook cell.

A cell's document URI must be unique across ALL notebook
cells and can therefore be used to uniquely identify a
notebook cell or the cell's text document.

@since 3.17.0
-}
data NotebookCell = NotebookCell 
  { {-|
  The cell's kind
  -}
  kind :: Language.LSP.Protocol.Internal.Types.NotebookCellKind.NotebookCellKind
  , {-|
  The URI of the cell's text document
  content.
  -}
  document :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Additional metadata stored with the cell.

  Note: should always be an object literal (e.g. LSPObject)
  -}
  metadata :: (Maybe Data.Aeson.Object)
  , {-|
  Additional execution summary information
  if supported by the client.
  -}
  executionSummary :: (Maybe Language.LSP.Protocol.Internal.Types.ExecutionSummary.ExecutionSummary)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookCell)

instance Aeson.ToJSON NotebookCell where
  toJSON (NotebookCell arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["document" Aeson..= arg1]
    ,"metadata" Language.LSP.Protocol.Types.Common..=? arg2
    ,"executionSummary" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON NotebookCell where
  parseJSON = Aeson.withObject "NotebookCell" $ \arg -> NotebookCell <$> arg Aeson..: "kind" <*> arg Aeson..: "document" <*> arg Language.LSP.Protocol.Types.Common..:!? "metadata" <*> arg Language.LSP.Protocol.Types.Common..:!? "executionSummary"
