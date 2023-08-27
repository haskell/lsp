{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentSyncOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SaveOptions
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data TextDocumentSyncOptions = TextDocumentSyncOptions 
  { {-|
  Open and close notifications are sent to the server. If omitted open close notification should not
  be sent.
  -}
  _openClose :: (Maybe Bool)
  , {-|
  Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
  and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.
  -}
  _change :: (Maybe Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind.TextDocumentSyncKind)
  , {-|
  If present will save notifications are sent to the server. If omitted the notification should not be
  sent.
  -}
  _willSave :: (Maybe Bool)
  , {-|
  If present will save wait until requests are sent to the server. If omitted the request should not be
  sent.
  -}
  _willSaveWaitUntil :: (Maybe Bool)
  , {-|
  If present save notifications are sent to the server. If omitted the notification should not be
  sent.
  -}
  _save :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.SaveOptions.SaveOptions))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentSyncOptions)

instance Aeson.ToJSON TextDocumentSyncOptions where
  toJSON (TextDocumentSyncOptions arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["openClose" Language.LSP.Protocol.Types.Common..=? arg0
    ,"change" Language.LSP.Protocol.Types.Common..=? arg1
    ,"willSave" Language.LSP.Protocol.Types.Common..=? arg2
    ,"willSaveWaitUntil" Language.LSP.Protocol.Types.Common..=? arg3
    ,"save" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON TextDocumentSyncOptions where
  parseJSON = Aeson.withObject "TextDocumentSyncOptions" $ \arg -> TextDocumentSyncOptions <$> arg Aeson..:! "openClose" <*> arg Aeson..:! "change" <*> arg Aeson..:! "willSave" <*> arg Aeson..:! "willSaveWaitUntil" <*> arg Aeson..:! "save"
