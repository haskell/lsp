-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities 
  { {-|
  Whether text document synchronization supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports sending will save notifications.
  -}
  _willSave :: (Maybe Bool)
  , {-|
  The client supports sending a will save request and
  waits for a response providing text edits which will
  be applied to the document before it is saved.
  -}
  _willSaveWaitUntil :: (Maybe Bool)
  , {-|
  The client supports did save notifications.
  -}
  _didSave :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TextDocumentSyncClientCapabilities where
  toJSON (TextDocumentSyncClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"willSave" Language.LSP.Protocol.Types.Common..=? arg1
    ,"willSaveWaitUntil" Language.LSP.Protocol.Types.Common..=? arg2
    ,"didSave" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON TextDocumentSyncClientCapabilities where
  parseJSON = Aeson.withObject "TextDocumentSyncClientCapabilities" $ \arg -> TextDocumentSyncClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "willSave" <*> arg Aeson..:! "willSaveWaitUntil" <*> arg Aeson..:! "didSave"
