{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Location where

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
Represents a location inside a resource, such as a line
inside a text file.
-}
data Location = Location 
  { {-|

  -}
  uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|

  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON Location)

instance Aeson.ToJSON Location where
  toJSON (Location arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["range" Aeson..= arg1]]

instance Aeson.FromJSON Location where
  parseJSON = Aeson.withObject "Location" $ \arg -> Location <$> arg Aeson..: "uri" <*> arg Aeson..: "range"
