{-# LANGUAGE DuplicateRecordFields #-}
module Language.LSP.Types.Location where

import           Control.Lens
import           Language.LSP.Types.Common
import           Language.LSP.Types.Internal.Generated
import           Language.LSP.Types.Internal.Lenses

-- | A helper function for creating ranges.
mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange l c l' c' = Range (Position l c) (Position l' c')

-- | 'isSubrangeOf' returns true if for every 'Position' in the first 'Range', it's also in the second 'Range'.
isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf small big = small ^. start >= big ^. start && small ^. end <= big ^. end

-- | 'positionInRange' returns true if the given 'Position' is in the 'Range'.
positionInRange :: Position -> Range -> Bool
positionInRange p (Range sp ep) = sp <= p && p < ep -- Range's end position is exclusive!
