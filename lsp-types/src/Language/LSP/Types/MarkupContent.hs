{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Language.LSP.Types.MarkupContent where

import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Language.LSP.Types.Internal.Generated

-- | Create a 'MarkupContent' containing a quoted language string only.
markedUpContent :: Text -> Text -> MarkupContent
markedUpContent lang quote
 = MarkupContent MarkupKind_Markdown ("\n```" <> lang <> "\n" <> quote <> "\n```\n")

-- | Create a 'MarkupContent' containing unquoted text
unmarkedUpContent :: Text -> MarkupContent
unmarkedUpContent str = MarkupContent MarkupKind_PlainText str

-- | Markdown for a section separator in Markdown, being a horizontal line
sectionSeparator :: Text
sectionSeparator = "* * *\n"

-- | Given some plaintext, convert it into some equivalent markdown text.
-- This is not *quite* the identity function.
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
