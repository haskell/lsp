{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithCells
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithNotebook
import qualified Language.LSP.Protocol.Types.Common

{-|
Registration options specific to a notebook.

@since 3.17.0
-}
data NotebookDocumentSyncRegistrationOptions = NotebookDocumentSyncRegistrationOptions 
  { {-|
  The notebooks to be synced
  -}
  _notebookSelector :: [(Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithNotebook.NotebookDocumentFilterWithNotebook Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterWithCells.NotebookDocumentFilterWithCells)]
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
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentSyncRegistrationOptions)

instance Aeson.ToJSON NotebookDocumentSyncRegistrationOptions where
  toJSON (NotebookDocumentSyncRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["notebookSelector" Aeson..= arg0]
    ,"save" Language.LSP.Protocol.Types.Common..=? arg1
    ,"id" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON NotebookDocumentSyncRegistrationOptions where
  parseJSON = Aeson.withObject "NotebookDocumentSyncRegistrationOptions" $ \arg -> NotebookDocumentSyncRegistrationOptions <$> arg Aeson..: "notebookSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "save" <*> arg Language.LSP.Protocol.Types.Common..:!? "id"
