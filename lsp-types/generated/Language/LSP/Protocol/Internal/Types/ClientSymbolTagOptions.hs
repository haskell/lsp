{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSymbolTagOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientSymbolTagOptions = ClientSymbolTagOptions 
  { {-|
  The tags supported by the client.
  -}
  _valueSet :: [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSymbolTagOptions)

instance Aeson.ToJSON ClientSymbolTagOptions where
  toJSON (ClientSymbolTagOptions arg0) = Aeson.object $ concat $  [["valueSet" Aeson..= arg0]]

instance Aeson.FromJSON ClientSymbolTagOptions where
  parseJSON = Aeson.withObject "ClientSymbolTagOptions" $ \arg -> ClientSymbolTagOptions <$> arg Aeson..: "valueSet"
