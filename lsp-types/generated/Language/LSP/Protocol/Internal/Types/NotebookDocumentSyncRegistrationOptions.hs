-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
Registration options specific to a notebook.

@since 3.17.0
-}
data NotebookDocumentSyncRegistrationOptions = NotebookDocumentSyncRegistrationOptions 
  { {-|
  The notebooks to be synced
  -}
  _notebookSelector :: [((Row.Rec ("notebook" Row..== (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter) Row..+ ("cells" Row..== (Maybe [(Row.Rec ("language" Row..== Data.Text.Text Row..+ Row.Empty))]) Row..+ Row.Empty))) Language.LSP.Protocol.Types.Common.|? (Row.Rec ("notebook" Row..== (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter.NotebookDocumentFilter)) Row..+ ("cells" Row..== [(Row.Rec ("language" Row..== Data.Text.Text Row..+ Row.Empty))] Row..+ Row.Empty))))]
  , {-|
  Whether save notification should be forwarded to
  the server. Will only be honored if mode === `notebook`.
  -}
  _save :: (Maybe Bool)
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON NotebookDocumentSyncRegistrationOptions where
  toJSON (NotebookDocumentSyncRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["notebookSelector" Aeson..= arg0]
    ,"save" Language.LSP.Protocol.Types.Common..=? arg1
    ,"id" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON NotebookDocumentSyncRegistrationOptions where
  parseJSON = Aeson.withObject "NotebookDocumentSyncRegistrationOptions" $ \arg -> NotebookDocumentSyncRegistrationOptions <$> arg Aeson..: "notebookSelector" <*> arg Aeson..:! "save" <*> arg Aeson..:! "id"
