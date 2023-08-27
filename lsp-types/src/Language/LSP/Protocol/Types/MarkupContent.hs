{-# OPTIONS_GHC -Wno-orphans #-}

-- | Additional instances and utilities for 'MarkupContent'.
module Language.LSP.Protocol.Types.MarkupContent where

import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Internal.Types.MarkupContent
import Language.LSP.Protocol.Internal.Types.MarkupKind

-- | Create a 'MarkupContent' containing plain text.
mkPlainText :: Text -> MarkupContent
mkPlainText = MarkupContent MarkupKind_PlainText

-- | Create a 'MarkupContent' containing markdown.
mkMarkdown :: Text -> MarkupContent
mkMarkdown = MarkupContent MarkupKind_Markdown

-- | Create a 'MarkupContent' containing a language-annotated code block only.
mkMarkdownCodeBlock :: Text -> Text -> MarkupContent
mkMarkdownCodeBlock lang quote =
  MarkupContent MarkupKind_Markdown ("\n```" <> lang <> "\n" <> quote <> "\n```\n")

-- | Markdown for a section separator in Markdown, being a horizontal line.
sectionSeparator :: Text
sectionSeparator = "* * *\n"

{- | Given some plaintext, convert it into some equivalent markdown text.
 This is not *quite* the identity function.
-}
plainTextToMarkdown :: Text -> Text
-- Line breaks in markdown paragraphs are ignored unless the line ends with two spaces.
-- In order to respect the line breaks in the original plaintext, we stick two spaces on the end of every line.
plainTextToMarkdown = T.unlines . fmap (<> "  ") . T.lines

instance Semigroup MarkupContent where
  MarkupContent MarkupKind_PlainText s1 <> MarkupContent MarkupKind_PlainText s2 = MarkupContent MarkupKind_PlainText (s1 `mappend` s2)
  MarkupContent MarkupKind_Markdown s1 <> MarkupContent MarkupKind_Markdown s2 = MarkupContent MarkupKind_Markdown (s1 `mappend` s2)
  MarkupContent MarkupKind_PlainText s1 <> MarkupContent MarkupKind_Markdown s2 = MarkupContent MarkupKind_Markdown (plainTextToMarkdown s1 `mappend` s2)
  MarkupContent MarkupKind_Markdown s1 <> MarkupContent MarkupKind_PlainText s2 = MarkupContent MarkupKind_Markdown (s1 `mappend` plainTextToMarkdown s2)

instance Monoid MarkupContent where
  mempty = MarkupContent MarkupKind_PlainText ""

instance IsString MarkupContent where
  fromString = mkPlainText . T.pack
