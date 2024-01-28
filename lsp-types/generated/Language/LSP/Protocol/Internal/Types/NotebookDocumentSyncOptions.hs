{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithCells
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithNotebook
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
  _notebookSelector :: [(Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithNotebook.NotebookDocumentFilterWithNotebook Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithCells.NotebookDocumentFilterWithCells)]
  , {-|
  Whether save notification should be forwarded to
  the server. Will only be honored if mode === `notebook`.
  -}
  _save :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentSyncOptions)

instance Aeson.ToJSON NotebookDocumentSyncOptions where
  toJSON (NotebookDocumentSyncOptions arg0 arg1) = Aeson.object $ concat $  [["notebookSelector" Aeson..= arg0]
    ,"save" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON NotebookDocumentSyncOptions where
  parseJSON = Aeson.withObject "NotebookDocumentSyncOptions" $ \arg -> NotebookDocumentSyncOptions <$> arg Aeson..: "notebookSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "save"
