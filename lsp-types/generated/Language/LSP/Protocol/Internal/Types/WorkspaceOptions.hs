{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FileOperationOptions
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|
Defines workspace specific capabilities of the server.

@since 3.18.0
@proposed
-}
data WorkspaceOptions = WorkspaceOptions 
  { {-|
  The server supports workspace folder.

  @since 3.6.0
  -}
  _workspaceFolders :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceFoldersServerCapabilities.WorkspaceFoldersServerCapabilities)
  , {-|
  The server is interested in notifications/requests for operations on files.

  @since 3.16.0
  -}
  _fileOperations :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationOptions.FileOperationOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceOptions)

instance Aeson.ToJSON WorkspaceOptions where
  toJSON (WorkspaceOptions arg0 arg1) = Aeson.object $ concat $  ["workspaceFolders" Language.LSP.Protocol.Types.Common..=? arg0
    ,"fileOperations" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON WorkspaceOptions where
  parseJSON = Aeson.withObject "WorkspaceOptions" $ \arg -> WorkspaceOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workspaceFolders" <*> arg Language.LSP.Protocol.Types.Common..:!? "fileOperations"
