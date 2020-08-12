{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
module Language.Haskell.LSP.Types.Location where

import           Control.DeepSeq
import           Data.Aeson.TH
import           GHC.Generics
import           Language.Haskell.LSP.Types.Uri
import           Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------

{-
The current protocol is talored for textual documents which content can be
represented as a string. There is currently no support for binary documents.
Positions inside a document (see Position definition below) are expressed as a
zero-based line and character offset. To ensure that both client and server
split the string into the same line representation the protocol specs the
following end of line sequences: '\n', '\r\n' and '\r'.

export const EOL: string[] = ['\n', '\r\n', '\r'];
-}
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#position

Position in a text document expressed as zero-based line and character offset. A
position is between two characters like an 'insert' cursor in a editor.

interface Position {
    /**
     * Line position in a document (zero-based).
     */
    line: number;

    /**
     * Character offset on a line in a document (zero-based).
     */
    character: number;
}
-}
data Position =
  Position
    { _line      :: Int
    , _character :: Int
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Position
deriveJSON lspOptions ''Position

-- ---------------------------------------------------------------------
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#range

A range in a text document expressed as (zero-based) start and end positions. A
range is comparable to a selection in an editor. Therefore the end position is
exclusive.

interface Range {
    /**
     * The range's start position.
     */
    start: Position;

    /**
     * The range's end position.
     */
    end: Position;
}
-}

data Range =
  Range
    { _start :: Position
    , _end   :: Position
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Range
deriveJSON lspOptions ''Range

-- ---------------------------------------------------------------------
{-
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#location

Represents a location inside a resource, such as a line inside a text file.

interface Location {
    uri: string;
    range: Range;
}
-}

data Location =
  Location
    { _uri   :: Uri
    , _range :: Range
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Location
deriveJSON lspOptions ''Location

-- ---------------------------------------------------------------------

-- | A helper function for creating ranges.
-- prop> mkRange l c l' c' = Range (Position l c) (Position l' c')
mkRange :: Int -> Int -> Int -> Int -> Range
mkRange l c l' c' = Range (Position l c) (Position l' c')
