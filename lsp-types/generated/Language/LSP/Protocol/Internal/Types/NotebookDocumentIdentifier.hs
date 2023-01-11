-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentIdentifier where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A literal to identify a notebook document in the client.

@since 3.17.0

-}
data NotebookDocumentIdentifier = NotebookDocumentIdentifier 
  { {-|
  The notebook document's uri.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON NotebookDocumentIdentifier where
  toJSON (NotebookDocumentIdentifier arg0) = Aeson.object $ concat $  [["uri" Aeson..= arg0]]

instance Aeson.FromJSON NotebookDocumentIdentifier where
  parseJSON = Aeson.withObject "NotebookDocumentIdentifier" $ \arg -> NotebookDocumentIdentifier <$> arg Aeson..: "uri"