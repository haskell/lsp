-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentEdit where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.AnnotatedTextEdit
import qualified Language.LSP.Protocol.Internal.Types.OptionalVersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
Describes textual changes on a text document. A TextDocumentEdit describes all changes
on a document version Si and after they are applied move the document to version Si+1.
So the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any
kind of ordering. However the edits must be non overlapping.

-}
data TextDocumentEdit = TextDocumentEdit 
  { {-|
  The text document to change.

  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.OptionalVersionedTextDocumentIdentifier.OptionalVersionedTextDocumentIdentifier
  , {-|
  The edits to be applied.

  @since 3.16.0 - support for AnnotatedTextEdit. This is guarded using a
  client capability.

  -}
  _edits :: [(Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.AnnotatedTextEdit.AnnotatedTextEdit)]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON TextDocumentEdit where
  toJSON (TextDocumentEdit arg0 arg1) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["edits" Aeson..= arg1]]

instance Aeson.FromJSON TextDocumentEdit where
  parseJSON = Aeson.withObject "TextDocumentEdit" $ \arg -> TextDocumentEdit <$> arg Aeson..: "textDocument" <*> arg Aeson..: "edits"