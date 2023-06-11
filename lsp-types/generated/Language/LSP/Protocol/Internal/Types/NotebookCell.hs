-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookCell where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
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
  _kind :: Language.LSP.Protocol.Internal.Types.NotebookCellKind.NotebookCellKind
  , {-|
  The URI of the cell's text document
  content.
  -}
  _document :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Additional metadata stored with the cell.

  Note: should always be an object literal (e.g. LSPObject)
  -}
  _metadata :: (Maybe Data.Aeson.Object)
  , {-|
  Additional execution summary information
  if supported by the client.
  -}
  _executionSummary :: (Maybe Language.LSP.Protocol.Internal.Types.ExecutionSummary.ExecutionSummary)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON NotebookCell where
  toJSON (NotebookCell arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["document" Aeson..= arg1]
    ,"metadata" Language.LSP.Protocol.Types.Common..=? arg2
    ,"executionSummary" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON NotebookCell where
  parseJSON = Aeson.withObject "NotebookCell" $ \arg -> NotebookCell <$> arg Aeson..: "kind" <*> arg Aeson..: "document" <*> arg Aeson..:! "metadata" <*> arg Aeson..:! "executionSummary"
