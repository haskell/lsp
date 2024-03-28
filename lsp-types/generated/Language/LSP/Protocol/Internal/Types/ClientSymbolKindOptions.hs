{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSymbolKindOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientSymbolKindOptions = ClientSymbolKindOptions 
  { {-|
  The symbol kind values the client supports. When this
  property exists the client also guarantees that it will
  handle values outside its set gracefully and falls back
  to a default value when unknown.

  If this property is not present the client only supports
  the symbol kinds from `File` to `Array` as defined in
  the initial version of the protocol.
  -}
  valueSet :: (Maybe [Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSymbolKindOptions)

instance Aeson.ToJSON ClientSymbolKindOptions where
  toJSON (ClientSymbolKindOptions arg0) = Aeson.object $ concat $  ["valueSet" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientSymbolKindOptions where
  parseJSON = Aeson.withObject "ClientSymbolKindOptions" $ \arg -> ClientSymbolKindOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "valueSet"
