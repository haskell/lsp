-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
Options specific to a notebook plus its cells
to be synced to the server.

If a selector provides a notebook document
filter but no cell selector all cells of a
matching notebook document will be synced.

If a selector provides no notebook document
filter but only a cell selector all notebook
document that contain at least one matching
cell will be synced.

@since 3.17.0

-}
data NotebookDocumentSyncOptions = NotebookDocumentSyncOptions 
  { {-|
  The notebooks to be synced

  -}
  _notebookSelector :: [((Row.Rec ("notebook" Row..== (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter) Row..+ ("cells" Row..== (Maybe [(Row.Rec ("language" Row..== Data.Text.Text Row..+ Row.Empty))]) Row..+ Row.Empty))) Language.LSP.Protocol.Types.Common.|? (Row.Rec ("notebook" Row..== (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter)) Row..+ ("cells" Row..== [(Row.Rec ("language" Row..== Data.Text.Text Row..+ Row.Empty))] Row..+ Row.Empty))))]
  , {-|
  Whether save notification should be forwarded to
  the server. Will only be honored if mode === `notebook`.

  -}
  _save :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON NotebookDocumentSyncOptions where
  toJSON (NotebookDocumentSyncOptions arg0 arg1) = Aeson.object $ concat $  [["notebookSelector" Aeson..= arg0]
    ,"save" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookDocumentSyncOptions where
  parseJSON = Aeson.withObject "NotebookDocumentSyncOptions" $ \arg -> NotebookDocumentSyncOptions <$> arg Aeson..: "notebookSelector" <*> arg Aeson..:! "save"