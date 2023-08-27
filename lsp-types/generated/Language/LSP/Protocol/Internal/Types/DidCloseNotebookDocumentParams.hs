{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidCloseNotebookDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The params sent in a close notebook document notification.

@since 3.17.0
-}
data DidCloseNotebookDocumentParams = DidCloseNotebookDocumentParams 
  { {-|
  The notebook document that got closed.
  -}
  _notebookDocument :: Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier.NotebookDocumentIdentifier
  , {-|
  The text documents that represent the content
  of a notebook cell that got closed.
  -}
  _cellTextDocuments :: [Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidCloseNotebookDocumentParams)

instance Aeson.ToJSON DidCloseNotebookDocumentParams where
  toJSON (DidCloseNotebookDocumentParams arg0 arg1) = Aeson.object $ concat $  [["notebookDocument" Aeson..= arg0]
    ,["cellTextDocuments" Aeson..= arg1]]

instance Aeson.FromJSON DidCloseNotebookDocumentParams where
  parseJSON = Aeson.withObject "DidCloseNotebookDocumentParams" $ \arg -> DidCloseNotebookDocumentParams <$> arg Aeson..: "notebookDocument" <*> arg Aeson..: "cellTextDocuments"
