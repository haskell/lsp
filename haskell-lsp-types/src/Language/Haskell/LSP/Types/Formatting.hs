{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Language.Haskell.LSP.Types.Formatting where

import Data.Aeson.TH
import Data.Text (Text)
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Utils

{-
Document Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-formatting-request

The document formatting request is sent from the server to the client to format
a whole document.

Request

    method: 'textDocument/formatting'
    params: DocumentFormattingParams defined as follows

interface DocumentFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The format options.
     */
    options: FormattingOptions;
}

/**
 * Value-object describing what options formatting should use.
 */
interface FormattingOptions {
    /**
     * Size of a tab in spaces.
     */
    tabSize: number;

    /**
     * Prefer spaces over tabs.
     */
    insertSpaces: boolean;

    /**
     * Signature for further properties.
     */
    [key: string]: boolean | number | string;
}

Response

    result: TextEdit[] describing the modification to the document to be
            formatted.
    error: code and message set in case an exception happens during the
           formatting request.

Registration Options: TextDocumentRegistrationOptions
-}

data FormattingOptions =
  FormattingOptions
    { _tabSize      :: Int
    , _insertSpaces :: Bool -- ^ Prefer spaces over tabs
    -- Note: May be more properties
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''FormattingOptions

data DocumentFormattingParams =
  DocumentFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _options      :: FormattingOptions
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DocumentFormattingParams

data DocumentFormattingOptions =
  DocumentFormattingOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentFormattingOptions ["_workDoneProgressOptions"]

data DocumentFormattingRegistrationOptions =
  DocumentFormattingRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _documentFormattingOptions       :: DocumentFormattingOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentFormattingRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_documentFormattingOptions"
  ]

-- ---------------------------------------------------------------------
{-
Document Range Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-range-formatting-request

The document range formatting request is sent from the client to the server to
format a given range in a document.

Request

    method: 'textDocument/rangeFormatting',
    params: DocumentRangeFormattingParams defined as follows

interface DocumentRangeFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The range to format
     */
    range: Range;

    /**
     * The format options
     */
    options: FormattingOptions;
}

Response

    result: TextEdit[] describing the modification to the document to be
            formatted.
    error: code and message set in case an exception happens during the range
           formatting request.
-}

data DocumentRangeFormattingParams =
  DocumentRangeFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _range        :: Range
    , _options      :: FormattingOptions
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentRangeFormattingParams

data DocumentRangeFormattingOptions =
  DocumentRangeFormattingOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentRangeFormattingOptions ["_workDoneProgressOptions"]

data DocumentRangeFormattingRegistrationOptions =
  DocumentRangeFormattingRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _documentRangeFormattingOptions  :: DocumentRangeFormattingOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentRangeFormattingRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_documentRangeFormattingOptions"
  ]

-- ---------------------------------------------------------------------
{-
Document on Type Formatting Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-on-type-formatting-request

The document on type formatting request is sent from the client to the server to
format parts of the document during typing.

Request

    method: 'textDocument/onTypeFormatting'
    params: DocumentOnTypeFormattingParams defined as follows

interface DocumentOnTypeFormattingParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position at which this request was sent.
     */
    position: Position;

    /**
     * The character that has been typed.
     */
    ch: string;

    /**
     * The format options.
     */
    options: FormattingOptions;
}

Response

    result: TextEdit[] describing the modification to the document.
    error: code and message set in case an exception happens during the range
           formatting request.

Registration Options: DocumentOnTypeFormattingRegistrationOptions defined as follows:

export interface DocumentOnTypeFormattingRegistrationOptions extends TextDocumentRegistrationOptions {
        /**
         * A character on which formatting should be triggered, like `}`.
         */
        firstTriggerCharacter: string;
        /**
         * More trigger characters.
         */
        moreTriggerCharacter?: string[]
}
-}

data DocumentOnTypeFormattingParams =
  DocumentOnTypeFormattingParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _ch           :: Text
    , _options      :: FormattingOptions
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentOnTypeFormattingParams


-- ---------------------------------------------------------------------
{-
/**
 * Format document on type options
 */
interface DocumentOnTypeFormattingOptions {
    /**
     * A character on which formatting should be triggered, like `}`.
     */
    firstTriggerCharacter: string;
    /**
     * More trigger characters.
     */
    moreTriggerCharacter?: string[]
}
-}

data DocumentOnTypeFormattingOptions =
  DocumentOnTypeFormattingOptions
    { -- | A character on which formatting should be triggered, like @}@.
      _firstTriggerCharacter :: Text
    , -- | More trigger characters.
      _moreTriggerCharacter  :: Maybe [Text]
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentOnTypeFormattingOptions

data DocumentOnTypeFormattingRegistrationOptions =
  DocumentOnTypeFormattingRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
     -- This doesn't extend WorkDoneProgressOptions -- is this an oversight in the spec?
     -- https://github.com/microsoft/language-server-protocol/issues/987
    , _documentOnTypeFormattingOptions  :: DocumentOnTypeFormattingOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''DocumentOnTypeFormattingRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_documentOnTypeFormattingOptions"
  ]
