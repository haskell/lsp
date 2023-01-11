-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidSaveTextDocumentParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in a save text document notification

-}
data DidSaveTextDocumentParams = DidSaveTextDocumentParams 
  { {-|
  The document that was saved.

  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  Optional the content when saved. Depends on the includeText value
  when the save notification was requested.

  -}
  _text :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DidSaveTextDocumentParams where
  toJSON (DidSaveTextDocumentParams arg0 arg1) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,"text" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DidSaveTextDocumentParams where
  parseJSON = Aeson.withObject "DidSaveTextDocumentParams" $ \arg -> DidSaveTextDocumentParams <$> arg Aeson..: "textDocument" <*> arg Aeson..:! "text"