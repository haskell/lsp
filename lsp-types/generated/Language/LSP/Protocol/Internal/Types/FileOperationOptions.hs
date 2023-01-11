-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Options for notifications/requests for user operations on files.

@since 3.16.0

-}
data FileOperationOptions = FileOperationOptions 
  { {-|
  The server is interested in receiving didCreateFiles notifications.

  -}
  _didCreate :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  , {-|
  The server is interested in receiving willCreateFiles requests.

  -}
  _willCreate :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  , {-|
  The server is interested in receiving didRenameFiles notifications.

  -}
  _didRename :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  , {-|
  The server is interested in receiving willRenameFiles requests.

  -}
  _willRename :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  , {-|
  The server is interested in receiving didDeleteFiles file notifications.

  -}
  _didDelete :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  , {-|
  The server is interested in receiving willDeleteFiles file requests.

  -}
  _willDelete :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions.FileOperationRegistrationOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON FileOperationOptions where
  toJSON (FileOperationOptions arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  ["didCreate" Language.LSP.Protocol.Types.Common..=? arg0
    ,"willCreate" Language.LSP.Protocol.Types.Common..=? arg1
    ,"didRename" Language.LSP.Protocol.Types.Common..=? arg2
    ,"willRename" Language.LSP.Protocol.Types.Common..=? arg3
    ,"didDelete" Language.LSP.Protocol.Types.Common..=? arg4
    ,"willDelete" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON FileOperationOptions where
  parseJSON = Aeson.withObject "FileOperationOptions" $ \arg -> FileOperationOptions <$> arg Aeson..:! "didCreate" <*> arg Aeson..:! "willCreate" <*> arg Aeson..:! "didRename" <*> arg Aeson..:! "willRename" <*> arg Aeson..:! "didDelete" <*> arg Aeson..:! "willDelete"