-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Types.Common

{-|
The workspace folder change event.

-}
data WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent 
  { {-|
  The array of added workspace folders

  -}
  _added :: [Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder]
  , {-|
  The array of the removed workspace folders

  -}
  _removed :: [Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkspaceFoldersChangeEvent where
  toJSON (WorkspaceFoldersChangeEvent arg0 arg1) = Aeson.object $ concat $  [["added" Aeson..= arg0]
    ,["removed" Aeson..= arg1]]

instance Aeson.FromJSON WorkspaceFoldersChangeEvent where
  parseJSON = Aeson.withObject "WorkspaceFoldersChangeEvent" $ \arg -> WorkspaceFoldersChangeEvent <$> arg Aeson..: "added" <*> arg Aeson..: "removed"