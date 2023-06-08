-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameFile where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.RenameFileOptions
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
Rename file operation
-}
data RenameFile = RenameFile 
  { {-|
  An optional annotation identifier describing the operation.

  @since 3.16.0
  -}
  _annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  , {-|
  A rename
  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "rename")
  , {-|
  The old (existing) location.
  -}
  _oldUri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The new location.
  -}
  _newUri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Rename options.
  -}
  _options :: (Maybe Language.LSP.Protocol.Internal.Types.RenameFileOptions.RenameFileOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON RenameFile where
  toJSON (RenameFile arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["annotationId" Language.LSP.Protocol.Types.Common..=? arg0
    ,["kind" Aeson..= arg1]
    ,["oldUri" Aeson..= arg2]
    ,["newUri" Aeson..= arg3]
    ,"options" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON RenameFile where
  parseJSON = Aeson.withObject "RenameFile" $ \arg -> RenameFile <$> arg Aeson..:! "annotationId" <*> arg Aeson..: "kind" <*> arg Aeson..: "oldUri" <*> arg Aeson..: "newUri" <*> arg Aeson..:! "options"