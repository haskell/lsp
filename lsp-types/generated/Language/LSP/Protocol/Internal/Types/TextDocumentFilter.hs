-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentFilter where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
A document filter denotes a document by different properties like
the `TextDocument.languageId` of
its resource, or a glob-pattern that is applied to the `TextDocument.fileName`.

Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

@sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
@sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`

@since 3.17.0

-}
newtype TextDocumentFilter = TextDocumentFilter ((Row.Rec ("language" Row..== Data.Text.Text Row..+ ("scheme" Row..== (Maybe Data.Text.Text) Row..+ ("pattern" Row..== (Maybe Data.Text.Text) Row..+ Row.Empty)))) Language.LSP.Protocol.Types.Common.|? ((Row.Rec ("language" Row..== (Maybe Data.Text.Text) Row..+ ("scheme" Row..== Data.Text.Text Row..+ ("pattern" Row..== (Maybe Data.Text.Text) Row..+ Row.Empty)))) Language.LSP.Protocol.Types.Common.|? (Row.Rec ("language" Row..== (Maybe Data.Text.Text) Row..+ ("scheme" Row..== (Maybe Data.Text.Text) Row..+ ("pattern" Row..== Data.Text.Text Row..+ Row.Empty))))))
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)