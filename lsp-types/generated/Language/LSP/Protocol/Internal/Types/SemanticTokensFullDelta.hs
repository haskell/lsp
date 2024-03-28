{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensFullDelta where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Semantic tokens options to support deltas for full documents

@since 3.18.0
@proposed
-}
data SemanticTokensFullDelta = SemanticTokensFullDelta 
  { {-|
  The server supports deltas for full documents.
  -}
  delta :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensFullDelta)

instance Aeson.ToJSON SemanticTokensFullDelta where
  toJSON (SemanticTokensFullDelta arg0) = Aeson.object $ concat $  ["delta" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON SemanticTokensFullDelta where
  parseJSON = Aeson.withObject "SemanticTokensFullDelta" $ \arg -> SemanticTokensFullDelta <$> arg Language.LSP.Protocol.Types.Common..:!? "delta"
