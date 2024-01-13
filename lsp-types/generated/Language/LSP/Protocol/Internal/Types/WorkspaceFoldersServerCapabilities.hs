{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WorkspaceFoldersServerCapabilities = WorkspaceFoldersServerCapabilities 
  { {-|
  The server has support for workspace folders
  -}
  _supported :: (Maybe Bool)
  , {-|
  Whether the server wants to receive workspace folder
  change notifications.

  If a string is provided the string is treated as an ID
  under which the notification is registered on the client
  side. The ID can be used to unregister for these events
  using the `client/unregisterCapability` request.
  -}
  _changeNotifications :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Bool))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceFoldersServerCapabilities)

instance Aeson.ToJSON WorkspaceFoldersServerCapabilities where
  toJSON (WorkspaceFoldersServerCapabilities arg0 arg1) = Aeson.object $ concat $  ["supported" Language.LSP.Protocol.Types.Common..=? arg0
    ,"changeNotifications" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON WorkspaceFoldersServerCapabilities where
  parseJSON = Aeson.withObject "WorkspaceFoldersServerCapabilities" $ \arg -> WorkspaceFoldersServerCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "supported" <*> arg Language.LSP.Protocol.Types.Common..:!? "changeNotifications"
