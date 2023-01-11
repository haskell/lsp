-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocument where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookCell
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A notebook document.

@since 3.17.0

-}
data NotebookDocument = NotebookDocument 
  { {-|
  The notebook document's uri.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The type of the notebook.

  -}
  _notebookType :: Data.Text.Text
  , {-|
  The version number of this document (it will increase after each
  change, including undo/redo).

  -}
  _version :: Language.LSP.Protocol.Types.Common.Int32
  , {-|
  Additional metadata stored with the notebook
  document.

  Note: should always be an object literal (e.g. LSPObject)

  -}
  _metadata :: (Maybe Data.Aeson.Object)
  , {-|
  The cells of a notebook.

  -}
  _cells :: [Language.LSP.Protocol.Internal.Types.NotebookCell.NotebookCell]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON NotebookDocument where
  toJSON (NotebookDocument arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["notebookType" Aeson..= arg1]
    ,["version" Aeson..= arg2]
    ,"metadata" Language.LSP.Protocol.Types.Common..=? arg3
    ,["cells" Aeson..= arg4]]

instance Aeson.FromJSON NotebookDocument where
  parseJSON = Aeson.withObject "NotebookDocument" $ \arg -> NotebookDocument <$> arg Aeson..: "uri" <*> arg Aeson..: "notebookType" <*> arg Aeson..: "version" <*> arg Aeson..:! "metadata" <*> arg Aeson..: "cells"