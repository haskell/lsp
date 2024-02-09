{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentChangeEvent where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChanges
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
  _cells :: (Maybe Language.LSP.Protocol.Internal.Types.NotebookDocumentCellChanges.NotebookDocumentCellChanges)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentChangeEvent)

instance Aeson.ToJSON NotebookDocumentChangeEvent where
  toJSON (NotebookDocumentChangeEvent arg0 arg1) = Aeson.object $ concat $  ["metadata" Language.LSP.Protocol.Types.Common..=? arg0
    ,"cells" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookDocumentChangeEvent where
  parseJSON = Aeson.withObject "NotebookDocumentChangeEvent" $ \arg -> NotebookDocumentChangeEvent <$> arg Language.LSP.Protocol.Types.Common..:!? "metadata" <*> arg Language.LSP.Protocol.Types.Common..:!? "cells"
