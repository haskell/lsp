-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookCell
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentItem
import qualified Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
A change event for a notebook document.

@since 3.17.0
-}
data NotebookDocumentChangeEvent = NotebookDocumentChangeEvent 
  { {-|
  The changed meta data if any.

  Note: should always be an object literal (e.g. LSPObject)
  -}
  _metadata :: (Maybe Data.Aeson.Object)
  , {-|
  Changes to cells
  -}
  _cells :: (Maybe (Row.Rec ("structure" Row..== (Maybe (Row.Rec ("array" Row..== Language.LSP.Protocol.Internal.Types.NotebookCellArrayChange.NotebookCellArrayChange Row..+ ("didOpen" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.TextDocumentItem.TextDocumentItem]) Row..+ ("didClose" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier]) Row..+ Row.Empty))))) Row..+ ("data" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.NotebookCell.NotebookCell]) Row..+ ("textContent" Row..== (Maybe [(Row.Rec ("document" Row..== Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier.VersionedTextDocumentIdentifier Row..+ ("changes" Row..== [Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent.TextDocumentContentChangeEvent] Row..+ Row.Empty)))]) Row..+ Row.Empty)))))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON NotebookDocumentChangeEvent where
  toJSON (NotebookDocumentChangeEvent arg0 arg1) = Aeson.object $ concat $  ["metadata" Language.LSP.Protocol.Types.Common..=? arg0
    ,"cells" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookDocumentChangeEvent where
  parseJSON = Aeson.withObject "NotebookDocumentChangeEvent" $ \arg -> NotebookDocumentChangeEvent <$> arg Aeson..:! "metadata" <*> arg Aeson..:! "cells"
