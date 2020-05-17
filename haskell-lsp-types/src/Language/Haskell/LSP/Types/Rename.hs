{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.LSP.Types.Rename where

import Data.Aeson.TH
import Data.Text (Text)
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------
{-
Rename Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#rename-request

The rename request is sent from the client to the server to perform a
workspace-wide rename of a symbol.

Request

    method: 'textDocument/rename'
    params: RenameParams defined as follows

interface RenameParams {
    /**
     * The document to format.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position at which this request was sent.
     */
    position: Position;

    /**
     * The new name of the symbol. If the given name is not valid the
     * request must return a [ResponseError](#ResponseError) with an
     * appropriate message set.
     */
    newName: string;
}

Response

    result: WorkspaceEdit describing the modification to the workspace.
    error: code and message set in case an exception happens during the rename
           request.

Registration Options: TextDocumentRegistrationOptions

-}
data RenameParams =
  RenameParams
    { _textDocument :: TextDocumentIdentifier
    , _position     :: Position
    , _newName      :: Text
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RenameParams


-- {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/alanz/mysrc/github/alanz/haskell-lsp/src/HieVscode.hs\"},\"position\":{\"line\":37,\"character\":17},\"newName\":\"getArgs'\"}}

-- ---------------------------------------------------------------------
{-
Prepare Rename Request

Since version 3.12.0

The prepare rename request is sent from the client to the server to setup
and test the validity of a rename operation at a given location.

Request:

    method: ‘textDocument/prepareRename’
    params: TextDocumentPositionParams

Response:

    result: Range | { range: Range, placeholder: string } | null describing
            the range of the string to rename and optionally a placeholder
            text of the string content to be renamed. If null is returned
            then it is deemed that a ‘textDocument/rename’ request is not
            valid at the given position.
    error: code and message set in case an exception happens during the
           prepare rename request.

-}

-- {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/alanz/mysrc/github/alanz/haskell-lsp/src/HieVscode.hs\"},\"position\":{\"line\":37,\"character\":17},\"newName\":\"getArgs'\"}}

data RangeWithPlaceholder =
  RangeWithPlaceholder
    {
      _range :: Range
    , _placeholder :: Text
    } deriving Eq

deriveJSON lspOptions { sumEncoding = UntaggedValue } ''RangeWithPlaceholder

data RangeOrRangeWithPlaceholder = RangeWithPlaceholderValue RangeWithPlaceholder
                                 | RangeValue Range
                                 deriving Eq

deriveJSON lspOptions { sumEncoding = UntaggedValue } ''RangeOrRangeWithPlaceholder


-- ---------------------------------------------------------------------
{-
New in 3.12
----------

/**
 * Rename options
 */
export interface RenameOptions {
        /**
         * Renames should be checked and tested before being executed.
         */
        prepareProvider?: boolean;
}
-}

data RenameOptions =
  RenameOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
      -- | Renames should be checked and tested before being executed.
    , _prepareProvider         :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSONExtendFields lspOptions ''RenameOptions ["_workDoneProgressOptions"]

data RenameRegistrationOptions =
  RenameRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _renameOptions                   :: RenameOptions
    } deriving (Read,Show,Eq)

deriveJSONExtendFields lspOptions ''RenameRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_renameOptions"
  ]
