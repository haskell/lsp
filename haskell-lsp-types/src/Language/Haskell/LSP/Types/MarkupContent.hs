{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | A MarkupContent literal represents a string value which content can
-- be represented in different formats.
-- Currently plaintext and markdown are supported formats.
-- A MarkupContent is usually used in documentation properties of result
-- literals like CompletionItem or SignatureInformation.
module Language.Haskell.LSP.Types.MarkupContent where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                                      (Text)
import           Language.Haskell.LSP.Types.Constants

{-
/**
 * Describes the content type that a client supports in various
 * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
 *
 * Please note that `MarkupKinds` must not start with a `$`. This kinds
 * are reserved for internal usage.
 */
export namespace MarkupKind {
        /**
         * Plain text is supported as a content format
         */
        export const PlainText: 'plaintext' = 'plaintext';

        /**
         * Markdown is supported as a content format
         */
        export const Markdown: 'markdown' = 'markdown';
}
export type MarkupKind = 'plaintext' | 'markdown';
-}

-- |  Describes the content type that a client supports in various
-- result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
data MarkupKind = MkPlainText -- ^ Plain text is supported as a content format
                | MkMarkdown -- ^ Markdown is supported as a content format
  deriving (Read, Show, Eq)

instance ToJSON MarkupKind where
  toJSON MkPlainText = String "plaintext"
  toJSON MkMarkdown  = String "markdown"

instance FromJSON MarkupKind where
  parseJSON (String "plaintext") = pure MkPlainText
  parseJSON (String "markdown")  = pure MkMarkdown
  parseJSON _                    = mempty


{-
/**
 * A `MarkupContent` literal represents a string value which content is interpreted base on its
 * kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
 *
 * If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
 * See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
 *
 * Here is an example how such a string can be constructed using JavaScript / TypeScript:
 * ```ts
 * let markdown: MarkdownContent = {
 *  kind: MarkupKind.Markdown,
 *	value: [
 *		'# Header',
 *		'Some text',
 *		'```typescript',
 *		'someCode();',
 *		'```'
 *	].join('\n')
 * };
 * ```
 *
 * *Please Note* that clients might sanitize the return markdown. A client could decide to
 * remove HTML from the markdown to avoid script execution.
 */
export interface MarkupContent {
        /**
         * The type of the Markup
         */
        kind: MarkupKind;

        /**
         * The content itself
         */
        value: string;
}
-}

-- | A `MarkupContent` literal represents a string value which content is interpreted base on its
-- | kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
-- |
-- | If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
-- | See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
-- |
-- | Here is an example how such a string can be constructed using JavaScript / TypeScript:
-- | ```ts
-- | let markdown: MarkdownContent = {
-- |  kind: MarkupKind.Markdown,
-- |	value: [
-- |		'# Header',
-- |		'Some text',
-- |		'```typescript',
-- |		'someCode();',
-- |		'```'
-- |	].join('\n')
-- | };
-- | ```
-- |
-- | *Please Note* that clients might sanitize the return markdown. A client could decide to
-- | remove HTML from the markdown to avoid script execution.
data MarkupContent =
  MarkupContent
    { _kind  :: MarkupKind -- ^ The type of the Markup
    , _value :: Text -- ^ The content itself
    }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''MarkupContent

-- ---------------------------------------------------------------------

-- | Create a 'MarkupContent' containing a quoted language string only.
markedUpContent :: Text -> Text -> MarkupContent
markedUpContent lang quote
 = MarkupContent MkMarkdown ("```" <> lang <> "\n" <> quote <> "\n```\n")

-- ---------------------------------------------------------------------

-- | Create a 'MarkupContent' containing unquoted text
unmarkedUpContent :: Text -> MarkupContent
unmarkedUpContent str = MarkupContent MkPlainText str

-- ---------------------------------------------------------------------

instance Semigroup MarkupContent where
  MarkupContent MkPlainText s1 <> MarkupContent MkPlainText s2 = MarkupContent MkPlainText (s1 <> s2)
  MarkupContent MkMarkdown  s1 <> MarkupContent _           s2 = MarkupContent MkMarkdown  (s1 <> s2)
  MarkupContent _           s1 <> MarkupContent MkMarkdown  s2 = MarkupContent MkMarkdown  (s1 <> s2)

instance Monoid MarkupContent where
  mempty = MarkupContent MkPlainText ""

-- ---------------------------------------------------------------------
