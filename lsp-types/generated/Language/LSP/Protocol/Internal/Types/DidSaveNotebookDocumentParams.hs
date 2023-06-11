-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidSaveNotebookDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The params sent in a save notebook document notification.

@since 3.17.0
-}
data DidSaveNotebookDocumentParams = DidSaveNotebookDocumentParams 
  { {-|
  The notebook document that got saved.
  -}
  _notebookDocument :: Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier.NotebookDocumentIdentifier
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON DidSaveNotebookDocumentParams where
  toJSON (DidSaveNotebookDocumentParams arg0) = Aeson.object $ concat $  [["notebookDocument" Aeson..= arg0]]

instance Aeson.FromJSON DidSaveNotebookDocumentParams where
  parseJSON = Aeson.withObject "DidSaveNotebookDocumentParams" $ \arg -> DidSaveNotebookDocumentParams <$> arg Aeson..: "notebookDocument"
