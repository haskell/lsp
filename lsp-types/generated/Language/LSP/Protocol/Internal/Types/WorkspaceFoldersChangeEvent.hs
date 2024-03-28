{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Types.Common

{-|
The workspace folder change event.
-}
data WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent 
  { {-|
  The array of added workspace folders
  -}
  added :: [Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder]
  , {-|
  The array of the removed workspace folders
  -}
  removed :: [Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceFoldersChangeEvent)

instance Aeson.ToJSON WorkspaceFoldersChangeEvent where
  toJSON (WorkspaceFoldersChangeEvent arg0 arg1) = Aeson.object $ concat $  [["added" Aeson..= arg0]
    ,["removed" Aeson..= arg1]]

instance Aeson.FromJSON WorkspaceFoldersChangeEvent where
  parseJSON = Aeson.withObject "WorkspaceFoldersChangeEvent" $ \arg -> WorkspaceFoldersChangeEvent <$> arg Aeson..: "added" <*> arg Aeson..: "removed"
