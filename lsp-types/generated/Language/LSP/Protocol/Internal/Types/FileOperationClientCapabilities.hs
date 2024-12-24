{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities where

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
Capabilities relating to events from file operations by the user in the client.

These events do not come from the file system, they come from user operations
like renaming a file in the UI.

@since 3.16.0
-}
data FileOperationClientCapabilities = FileOperationClientCapabilities 
  { {-|
  Whether the client supports dynamic registration for file requests/notifications.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The client has support for sending didCreateFiles notifications.
  -}
  didCreate :: (Maybe Bool)
  , {-|
  The client has support for sending willCreateFiles requests.
  -}
  willCreate :: (Maybe Bool)
  , {-|
  The client has support for sending didRenameFiles notifications.
  -}
  didRename :: (Maybe Bool)
  , {-|
  The client has support for sending willRenameFiles requests.
  -}
  willRename :: (Maybe Bool)
  , {-|
  The client has support for sending didDeleteFiles notifications.
  -}
  didDelete :: (Maybe Bool)
  , {-|
  The client has support for sending willDeleteFiles requests.
  -}
  willDelete :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FileOperationClientCapabilities)

instance Aeson.ToJSON FileOperationClientCapabilities where
  toJSON (FileOperationClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"didCreate" Language.LSP.Protocol.Types.Common..=? arg1
    ,"willCreate" Language.LSP.Protocol.Types.Common..=? arg2
    ,"didRename" Language.LSP.Protocol.Types.Common..=? arg3
    ,"willRename" Language.LSP.Protocol.Types.Common..=? arg4
    ,"didDelete" Language.LSP.Protocol.Types.Common..=? arg5
    ,"willDelete" Language.LSP.Protocol.Types.Common..=? arg6]

instance Aeson.FromJSON FileOperationClientCapabilities where
  parseJSON = Aeson.withObject "FileOperationClientCapabilities" $ \arg -> FileOperationClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "didCreate" <*> arg Language.LSP.Protocol.Types.Common..:!? "willCreate" <*> arg Language.LSP.Protocol.Types.Common..:!? "didRename" <*> arg Language.LSP.Protocol.Types.Common..:!? "willRename" <*> arg Language.LSP.Protocol.Types.Common..:!? "didDelete" <*> arg Language.LSP.Protocol.Types.Common..:!? "willDelete"
