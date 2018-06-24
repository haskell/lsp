{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.TextDocumentIdentifier where

import           Data.Aeson.TH
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.Uri

-- ---------------------------------------------------------------------
{-
TextDocumentIdentifier

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentidentifier

Text documents are identified using a URI. On the protocol level, URIs are
passed as strings. The corresponding JSON structure looks like this:

interface TextDocumentIdentifier {
    /**
     * The text document's URI.
     */
    uri: string;
}
-}
data TextDocumentIdentifier =
  TextDocumentIdentifier
    { _uri :: Uri
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentIdentifier