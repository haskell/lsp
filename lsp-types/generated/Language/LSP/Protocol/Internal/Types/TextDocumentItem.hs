-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentItem where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
An item to transfer a text document from the client to the
server.

-}
data TextDocumentItem = TextDocumentItem 
  { {-|
  The text document's uri.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The text document's language identifier.

  -}
  _languageId :: Data.Text.Text
  , {-|
  The version number of this document (it will increase after each
  change, including undo/redo).

  -}
  _version :: Language.LSP.Protocol.Types.Common.Int32
  , {-|
  The content of the opened text document.

  -}
  _text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON TextDocumentItem where
  toJSON (TextDocumentItem arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["languageId" Aeson..= arg1]
    ,["version" Aeson..= arg2]
    ,["text" Aeson..= arg3]]

instance Aeson.FromJSON TextDocumentItem where
  parseJSON = Aeson.withObject "TextDocumentItem" $ \arg -> TextDocumentItem <$> arg Aeson..: "uri" <*> arg Aeson..: "languageId" <*> arg Aeson..: "version" <*> arg Aeson..: "text"