{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSymbolResolveOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientSymbolResolveOptions = ClientSymbolResolveOptions 
  { {-|
  The properties that a client can resolve lazily. Usually
  `location.range`
  -}
  _properties :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSymbolResolveOptions)

instance Aeson.ToJSON ClientSymbolResolveOptions where
  toJSON (ClientSymbolResolveOptions arg0) = Aeson.object $ concat $  [["properties" Aeson..= arg0]]

instance Aeson.FromJSON ClientSymbolResolveOptions where
  parseJSON = Aeson.withObject "ClientSymbolResolveOptions" $ \arg -> ClientSymbolResolveOptions <$> arg Aeson..: "properties"
