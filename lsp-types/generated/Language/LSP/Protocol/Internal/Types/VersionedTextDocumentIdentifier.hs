-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A text document identifier to denote a specific version of a text document.

-}
data VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier 
  { {-|
  The text document's uri.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The version number of this document.

  -}
  _version :: Language.LSP.Protocol.Types.Common.Int32
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON VersionedTextDocumentIdentifier where
  toJSON (VersionedTextDocumentIdentifier arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["version" Aeson..= arg1]]

instance Aeson.FromJSON VersionedTextDocumentIdentifier where
  parseJSON = Aeson.withObject "VersionedTextDocumentIdentifier" $ \arg -> VersionedTextDocumentIdentifier <$> arg Aeson..: "uri" <*> arg Aeson..: "version"