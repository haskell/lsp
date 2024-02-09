{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LocationUriOnly where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Location with only uri and does not include range.

@since 3.18.0
@proposed
-}
data LocationUriOnly = LocationUriOnly 
  { {-|

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON LocationUriOnly)

instance Aeson.ToJSON LocationUriOnly where
  toJSON (LocationUriOnly arg0) = Aeson.object $ concat $  [["uri" Aeson..= arg0]]

instance Aeson.FromJSON LocationUriOnly where
  parseJSON = Aeson.withObject "LocationUriOnly" $ \arg -> LocationUriOnly <$> arg Aeson..: "uri"
