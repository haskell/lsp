-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A literal to identify a text document in the client.

-}
data TextDocumentIdentifier = TextDocumentIdentifier 
  { {-|
  The text document's uri.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TextDocumentIdentifier where
  toJSON (TextDocumentIdentifier arg0) = Aeson.object $ concat $  [["uri" Aeson..= arg0]]

instance Aeson.FromJSON TextDocumentIdentifier where
  parseJSON = Aeson.withObject "TextDocumentIdentifier" $ \arg -> TextDocumentIdentifier <$> arg Aeson..: "uri"