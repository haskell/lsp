{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LocationLink where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Represents the connection of two locations. Provides additional metadata over normal `Location`,
including an origin range.
-}
data LocationLink = LocationLink 
  { {-|
  Span of the origin of this link.

  Used as the underlined span for mouse interaction. Defaults to the word range at
  the definition position.
  -}
  _originSelectionRange :: (Maybe Language.LSP.Protocol.Internal.Types.Range.Range)
  , {-|
  The target resource identifier of this link.
  -}
  _targetUri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The full target range of this link. If the target for example is a symbol then target range is the
  range enclosing this symbol not including leading/trailing whitespace but everything else
  like comments. This information is typically used to highlight the range in the editor.
  -}
  _targetRange :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The range that should be selected and revealed when this link is being followed, e.g the name of a function.
  Must be contained by the `targetRange`. See also `DocumentSymbol#range`
  -}
  _targetSelectionRange :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON LocationLink)

instance Aeson.ToJSON LocationLink where
  toJSON (LocationLink arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["originSelectionRange" Language.LSP.Protocol.Types.Common..=? arg0
    ,["targetUri" Aeson..= arg1]
    ,["targetRange" Aeson..= arg2]
    ,["targetSelectionRange" Aeson..= arg3]]

instance Aeson.FromJSON LocationLink where
  parseJSON = Aeson.withObject "LocationLink" $ \arg -> LocationLink <$> arg Aeson..:! "originSelectionRange" <*> arg Aeson..: "targetUri" <*> arg Aeson..: "targetRange" <*> arg Aeson..: "targetSelectionRange"
