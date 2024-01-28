{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCodeActionResolveOptions where

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
data ClientCodeActionResolveOptions = ClientCodeActionResolveOptions 
  { {-|
  The properties that a client can resolve lazily.
  -}
  _properties :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCodeActionResolveOptions)

instance Aeson.ToJSON ClientCodeActionResolveOptions where
  toJSON (ClientCodeActionResolveOptions arg0) = Aeson.object $ concat $  [["properties" Aeson..= arg0]]

instance Aeson.FromJSON ClientCodeActionResolveOptions where
  parseJSON = Aeson.withObject "ClientCodeActionResolveOptions" $ \arg -> ClientCodeActionResolveOptions <$> arg Aeson..: "properties"
