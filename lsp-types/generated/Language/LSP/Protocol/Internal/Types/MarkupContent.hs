-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MarkupContent where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MarkupKind
import qualified Language.LSP.Protocol.Types.Common

{-|
A `MarkupContent` literal represents a string value which content is interpreted base on its
kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.

If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

Here is an example how such a string can be constructed using JavaScript / TypeScript:
```ts
let markdown: MarkdownContent = {
 kind: MarkupKind.Markdown,
 value: [
   '# Header',
   'Some text',
   '```typescript',
   'someCode();',
   '```'
 ].join('\n')
};
```

*Please Note* that clients might sanitize the return markdown. A client could decide to
remove HTML from the markdown to avoid script execution.

-}
data MarkupContent = MarkupContent 
  { {-|
  The type of the Markup

  -}
  _kind :: Language.LSP.Protocol.Internal.Types.MarkupKind.MarkupKind
  , {-|
  The content itself

  -}
  _value :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON MarkupContent where
  toJSON (MarkupContent arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["value" Aeson..= arg1]]

instance Aeson.FromJSON MarkupContent where
  parseJSON = Aeson.withObject "MarkupContent" $ \arg -> MarkupContent <$> arg Aeson..: "kind" <*> arg Aeson..: "value"