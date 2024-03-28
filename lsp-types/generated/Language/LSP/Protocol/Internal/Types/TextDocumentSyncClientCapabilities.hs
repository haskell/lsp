{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities 
  { {-|
  Whether text document synchronization supports dynamic registration.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports sending will save notifications.
  -}
  willSave :: (Maybe Bool)
  , {-|
  The client supports sending a will save request and
  waits for a response providing text edits which will
  be applied to the document before it is saved.
  -}
  willSaveWaitUntil :: (Maybe Bool)
  , {-|
  The client supports did save notifications.
  -}
  didSave :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentSyncClientCapabilities)

instance Aeson.ToJSON TextDocumentSyncClientCapabilities where
  toJSON (TextDocumentSyncClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"willSave" Language.LSP.Protocol.Types.Common..=? arg1
    ,"willSaveWaitUntil" Language.LSP.Protocol.Types.Common..=? arg2
    ,"didSave" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON TextDocumentSyncClientCapabilities where
  parseJSON = Aeson.withObject "TextDocumentSyncClientCapabilities" $ \arg -> TextDocumentSyncClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "willSave" <*> arg Language.LSP.Protocol.Types.Common..:!? "willSaveWaitUntil" <*> arg Language.LSP.Protocol.Types.Common..:!? "didSave"
