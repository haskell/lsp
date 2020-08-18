{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.DocumentFilter where

import           Data.Aeson.TH
import           Data.Text                      ( Text )
import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------

data DocumentFilter =
  DocumentFilter
    { -- | A language id, like `typescript`.
      _language :: Maybe Text
      -- | A Uri scheme, like @file@ or @untitled@.
    , _scheme   :: Maybe Text
    , -- | A glob pattern, like `*.{ts,js}`.
      --
      -- Glob patterns can have the following syntax:
	    -- - @*@ to match one or more characters in a path segment
	    -- - @?@ to match on one character in a path segment
	    -- - @**@ to match any number of path segments, including none
	    -- - @{}@ to group conditions (e.g. @**​/*.{ts,js}@ matches all TypeScript and JavaScript files)
	    -- - @[]@ to declare a range of characters to match in a path segment (e.g., @example.[0-9]@ to match on @example.0@, @example.1@, …)
	    -- - @[!...]@ to negate a range of characters to match in a path segment (e.g., @example.[!0-9]@ to match on @example.a@, @example.b@, but not @example.0@)
      _pattern  :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentFilter

{-
A document selector is the combination of one or many document filters.

export type DocumentSelector = DocumentFilter[];
-}
type DocumentSelector = List DocumentFilter
