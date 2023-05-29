-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeTextDocumentParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The change text document notification's parameters.

-}
data DidChangeTextDocumentParams = DidChangeTextDocumentParams 
  { {-|
  The document that did change. The version number points
  to the version after all provided content changes have
  been applied.

  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier.VersionedTextDocumentIdentifier
  , {-|
  The actual content changes. The content changes describe single state changes
  to the document. So if there are two content changes c1 (at array index 0) and
  c2 (at array index 1) for a document in state S then c1 moves the document from
  S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
  on the state S'.

  To mirror the content of a document using change events use the following approach:
  - start with the same initial content
  - apply the 'textDocument/didChange' notifications in the order you receive them.
  - apply the `TextDocumentContentChangeEvent`s in a single notification in the order
    you receive them.

  -}
  _contentChanges :: [Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent.TextDocumentContentChangeEvent]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DidChangeTextDocumentParams where
  toJSON (DidChangeTextDocumentParams arg0 arg1) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["contentChanges" Aeson..= arg1]]

instance Aeson.FromJSON DidChangeTextDocumentParams where
  parseJSON = Aeson.withObject "DidChangeTextDocumentParams" $ \arg -> DidChangeTextDocumentParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "contentChanges"