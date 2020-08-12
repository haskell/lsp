{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.DocumentFilter where

import           Data.Aeson.TH
import           Data.Text                      ( Text )
import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

DocumentFilter
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#new-documentfilter

A document filter denotes a document through properties like language, schema or
pattern. Examples are a filter that applies to TypeScript files on disk or a
filter the applies to JSON files with name package.json:

    { language: 'typescript', scheme: 'file' }
    { language: 'json', pattern: '**/package.json' }

export interface DocumentFilter {
        /**
         * A language id, like `typescript`.
         */
        language?: string;

        /**
         * A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
         */
        scheme?: string;

        /**
         * A glob pattern, like `*.{ts,js}`.
         */
        pattern?: string;
}
-}
data DocumentFilter =
  DocumentFilter
    { _language :: Maybe Text
    , _scheme   :: Maybe Text
    , _pattern  :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentFilter

{-
A document selector is the combination of one or many document filters.

export type DocumentSelector = DocumentFilter[];
-}
type DocumentSelector = List DocumentFilter
