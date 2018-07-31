{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.TextDocument where

import           Data.Aeson.TH
import           Data.Text                      ( Text )
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.Location
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

{-
TextDocumentItem

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentitem

    New: An item to transfer a text document from the client to the server.

interface TextDocumentItem {
    /**
     * The text document's URI.
     */
    uri: string;

    /**
     * The text document's language identifier.
     */
    languageId: string;

    /**
     * The version number of this document (it will strictly increase after each
     * change, including undo/redo).
     */
    version: number;

    /**
     * The content of the opened text document.
     */
    text: string;
}
-}

data TextDocumentItem =
  TextDocumentItem {
    _uri        :: Uri
  , _languageId :: Text
  , _version    :: Int
  , _text       :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentItem

-- ---------------------------------------------------------------------
{-
TextDocumentPositionParams

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentpositionparams

    Changed: Was TextDocumentPosition in 1.0 with inlined parameters


interface TextDocumentPositionParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position inside the text document.
     */
    position: Position;
}

-}
data TextDocumentPositionParams =
  TextDocumentPositionParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentPositionParams
