-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DeleteFilesParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.FileDelete
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in notifications/requests for user-initiated deletes of
files.

@since 3.16.0

-}
data DeleteFilesParams = DeleteFilesParams 
  { {-|
  An array of all files/folders deleted in this operation.

  -}
  _files :: [Language.LSP.Protocol.Internal.Types.FileDelete.FileDelete]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DeleteFilesParams where
  toJSON (DeleteFilesParams arg0) = Aeson.object $ concat $  [["files" Aeson..= arg0]]

instance Aeson.FromJSON DeleteFilesParams where
  parseJSON = Aeson.withObject "DeleteFilesParams" $ \arg -> DeleteFilesParams <$> arg Aeson..: "files"