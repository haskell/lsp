{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LSP.Types.Location where

import           Control.DeepSeq
import           Data.Aeson.TH
import           Data.Hashable
import           GHC.Generics              hiding (UInt)
import           Language.LSP.Types.Common
import           Language.LSP.Types.Uri
import           Language.LSP.Types.Utils

-- ---------------------------------------------------------------------

-- | A position in a document. Note that the character offsets in a line
-- are given in UTF-16 code units, /not/ Unicode code points.
data Position =
  Position
    { -- | Line position in a document (zero-based).
      _line      :: UInt
      -- | Character offset on a line in a document (zero-based). Assuming that
      -- the line is represented as a string, the @character@ value represents the
      -- gap between the @character@ and @character + 1@.
    , _character :: UInt
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Position
deriveJSON lspOptions ''Position

instance Hashable Position

-- ---------------------------------------------------------------------

data Range =
  Range
    { _start :: Position -- ^ The range's start position. (inclusive)
    , _end   :: Position -- ^ The range's end position. (exclusive, see: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range )
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Range
deriveJSON lspOptions ''Range

instance Hashable Range

-- ---------------------------------------------------------------------

data Location =
  Location
    { _uri   :: Uri
    , _range :: Range
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Location
deriveJSON lspOptions ''Location

instance Hashable Location

-- ---------------------------------------------------------------------

-- | Represents a link between a source and a target location.
data LocationLink =
  LocationLink
  { -- | Span of the origin of this link.
    -- Used as the underlined span for mouse interaction. Defaults to the word
    -- range at the mouse position.
    _originSelectionRange :: Maybe Range
    -- | The target resource identifier of this link.
  , _targetUri            :: Uri
    -- | The full target range of this link. If the target for example is a
    -- symbol then target range is the range enclosing this symbol not including
    -- leading/trailing whitespace but everything else like comments. This
    -- information is typically used to highlight the range in the editor.
  , _targetRange          :: Range
    -- | The range that should be selected and revealed when this link is being
    -- followed, e.g the name of a function. Must be contained by the the
    -- 'targetRange'. See also @DocumentSymbol._range@
  , _targetSelectionRange :: Range
  } deriving (Read, Show, Eq)
deriveJSON lspOptions ''LocationLink

-- ---------------------------------------------------------------------

-- | A helper function for creating ranges.
-- prop> mkRange l c l' c' = Range (Position l c) (Position l' c')
mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange l c l' c' = Range (Position l c) (Position l' c')

-- | 'isSubrangeOf' returns true if for every 'Position' in the first 'Range', it's also in the second 'Range'.
isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf smallRange range = _start smallRange >= _start range && _end smallRange <= _end range

-- | 'positionInRange' returns true if the given 'Position' is in the 'Range'.
positionInRange :: Position -> Range -> Bool
positionInRange p (Range sp ep) = sp <= p && p < ep -- Range's end position is exclusive.
