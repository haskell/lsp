{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.LSP.Types.DocumentHighlight where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------
{-
Document Highlights Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-highlights-request

The document highlight request is sent from the client to the server to resolve
a document highlights for a given text document position. For programming
languages this usually highlights all references to the symbol scoped to this
file. However we kept 'textDocument/documentHighlight' and
'textDocument/references' separate requests since the first one is allowed to be
more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read or Write
whereas fuzzy or textual matches use Textas the kind.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/documentHighlight'
    params: TextDocumentPositionParams

Response

    result: DocumentHighlight[] defined as follows:

/**
 * A document highlight is a range inside a text document which deserves
 * special attention. Usually a document highlight is visualized by changing
 * the background color of its range.
 *
 */
interface DocumentHighlight {
    /**
     * The range this highlight applies to.
     */
    range: Range;

    /**
     * The highlight kind, default is DocumentHighlightKind.Text.
     */
    kind?: number;
}

/**
 * A document highlight kind.
 */
enum DocumentHighlightKind {
    /**
     * A textual occurrance.
     */
    Text = 1,

    /**
     * Read-access of a symbol, like reading a variable.
     */
    Read = 2,

    /**
     * Write-access of a symbol, like writing to a variable.
     */
    Write = 3
}

    error: code and message set in case an exception happens during the document
           highlight request.

Registration Options: TextDocumentRegistrationOptions

-}

data DocumentHighlightKind = HkText | HkRead | HkWrite
  deriving (Read,Show,Eq)

instance ToJSON DocumentHighlightKind where
  toJSON HkText  = Number 1
  toJSON HkRead  = Number 2
  toJSON HkWrite = Number 3

instance FromJSON DocumentHighlightKind where
  parseJSON (Number 1) = pure HkText
  parseJSON (Number 2) = pure HkRead
  parseJSON (Number 3) = pure HkWrite
  parseJSON _            = mempty

-- -------------------------------------

data DocumentHighlight =
  DocumentHighlight
    { _range :: Range
    , _kind  :: Maybe DocumentHighlightKind
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentHighlight

data DocumentHighlightOptions =
  DocumentHighlightOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    } deriving (Read,Show,Eq)

deriveJSONExtendFields lspOptions ''DocumentHighlightOptions ["_workDoneProgressOptions"]
