{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Haskell.LSP.Types.SelectionRange where

import           Data.Aeson.TH

import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Message
import           Language.Haskell.LSP.Types.Progress
import           Language.Haskell.LSP.Types.TextDocument

data SelectionRangeParams =
  SelectionRangeParams
  { _textDocument  :: TextDocumentIdentifier
    -- ^ The text document.
  , _positions     :: [Position]
  -- ^ The positions inside the text document.
  , _workDoneToken :: Maybe ProgressToken
    -- ^ An optional token that a server can use to report work done
    -- progress.
  } deriving (Read, Show, Eq)

$(deriveJSON lspOptions ''SelectionRangeParams)

-- | Represents a selection range.
data SelectionRange =
  SelectionRange
  { _range  :: Range
  -- ^ The range ('Range') of this selection range.
  , _parent :: Maybe SelectionRange
  -- ^ The parent selection range containing this range. Therefore
  -- @_range (_parent this)@ must contain @_range this@.
  } deriving (Read, Show, Eq)

deriveJSON lspOptions ''SelectionRange

type SelectionRangeRequest
  = RequestMessage ClientMethod SelectionRangeParams (List SelectionRange)
type SelectionRangeResponse = ResponseMessage (List SelectionRange)
