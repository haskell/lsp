-- | Additional and utilities for 'Position' and 'Range'.
module Language.LSP.Protocol.Types.Location where

import Language.LSP.Protocol.Internal.Types.Position
import Language.LSP.Protocol.Internal.Types.Range
import Language.LSP.Protocol.Types.Common

{- | A helper function for creating ranges.
 prop> mkRange l c l' c' = Range (Position l c) (Position l' c')
-}
mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange l c l' c' = Range (Position l c) (Position l' c')

-- | 'isSubrangeOf' returns true if for every 'Position' in the first 'Range', it's also in the second 'Range'.
isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf smallRange range = _start smallRange >= _start range && _end smallRange <= _end range

-- | 'positionInRange' returns true if the given 'Position' is in the 'Range'.
positionInRange :: Position -> Range -> Bool
positionInRange p (Range sp ep) = sp <= p && p < ep -- Range's end position is exclusive.
