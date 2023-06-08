-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Location where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
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
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|

  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Location where
  toJSON (Location arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["range" Aeson..= arg1]]

instance Aeson.FromJSON Location where
  parseJSON = Aeson.withObject "Location" $ \arg -> Location <$> arg Aeson..: "uri" <*> arg Aeson..: "range"