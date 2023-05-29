-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameFilesParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.FileRename
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in notifications/requests for user-initiated renames of
files.

@since 3.16.0
-}
data RenameFilesParams = RenameFilesParams 
  { {-|
  An array of all files/folders renamed in this operation. When a folder is renamed, only
  the folder will be included, and not its children.
  -}
  _files :: [Language.LSP.Protocol.Internal.Types.FileRename.FileRename]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON RenameFilesParams where
  toJSON (RenameFilesParams arg0) = Aeson.object $ concat $  [["files" Aeson..= arg0]]

instance Aeson.FromJSON RenameFilesParams where
  parseJSON = Aeson.withObject "RenameFilesParams" $ \arg -> RenameFilesParams <$> arg Aeson..: "files"