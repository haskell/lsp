-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CreateFile where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.CreateFileOptions
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
Create file operation.

-}
data CreateFile = CreateFile 
  { {-|
  An optional annotation identifier describing the operation.

  @since 3.16.0

  -}
  _annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  , {-|
  A create

  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "create")
  , {-|
  The resource to create.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Additional options

  -}
  _options :: (Maybe Language.LSP.Protocol.Internal.Types.CreateFileOptions.CreateFileOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CreateFile where
  toJSON (CreateFile arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["annotationId" Language.LSP.Protocol.Types.Common..=? arg0
    ,["kind" Aeson..= arg1]
    ,["uri" Aeson..= arg2]
    ,"options" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON CreateFile where
  parseJSON = Aeson.withObject "CreateFile" $ \arg -> CreateFile <$> arg Aeson..:! "annotationId" <*> arg Aeson..: "kind" <*> arg Aeson..: "uri" <*> arg Aeson..:! "options"