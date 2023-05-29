-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Capabilities relating to events from file operations by the user in the client.

These events do not come from the file system, they come from user operations
like renaming a file in the UI.

@since 3.16.0

-}
data FileOperationClientCapabilities = FileOperationClientCapabilities 
  { {-|
  Whether the client supports dynamic registration for file requests/notifications.

  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client has support for sending didCreateFiles notifications.

  -}
  _didCreate :: (Maybe Bool)
  , {-|
  The client has support for sending willCreateFiles requests.

  -}
  _willCreate :: (Maybe Bool)
  , {-|
  The client has support for sending didRenameFiles notifications.

  -}
  _didRename :: (Maybe Bool)
  , {-|
  The client has support for sending willRenameFiles requests.

  -}
  _willRename :: (Maybe Bool)
  , {-|
  The client has support for sending didDeleteFiles notifications.

  -}
  _didDelete :: (Maybe Bool)
  , {-|
  The client has support for sending willDeleteFiles requests.

  -}
  _willDelete :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON FileOperationClientCapabilities where
  toJSON (FileOperationClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"didCreate" Language.LSP.Protocol.Types.Common..=? arg1
    ,"willCreate" Language.LSP.Protocol.Types.Common..=? arg2
    ,"didRename" Language.LSP.Protocol.Types.Common..=? arg3
    ,"willRename" Language.LSP.Protocol.Types.Common..=? arg4
    ,"didDelete" Language.LSP.Protocol.Types.Common..=? arg5
    ,"willDelete" Language.LSP.Protocol.Types.Common..=? arg6]

instance Aeson.FromJSON FileOperationClientCapabilities where
  parseJSON = Aeson.withObject "FileOperationClientCapabilities" $ \arg -> FileOperationClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "didCreate" <*> arg Aeson..:! "willCreate" <*> arg Aeson..:! "didRename" <*> arg Aeson..:! "willRename" <*> arg Aeson..:! "didDelete" <*> arg Aeson..:! "willDelete"