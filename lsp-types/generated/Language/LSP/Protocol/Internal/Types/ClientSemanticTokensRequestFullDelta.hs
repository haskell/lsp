{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestFullDelta where

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
@since 3.18.0
@proposed
-}
data ClientSemanticTokensRequestFullDelta = ClientSemanticTokensRequestFullDelta 
  { {-|
  The client will send the `textDocument/semanticTokens/full/delta` request if
  the server provides a corresponding handler.
  -}
  delta :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSemanticTokensRequestFullDelta)

instance Aeson.ToJSON ClientSemanticTokensRequestFullDelta where
  toJSON (ClientSemanticTokensRequestFullDelta arg0) = Aeson.object $ concat $  ["delta" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientSemanticTokensRequestFullDelta where
  parseJSON = Aeson.withObject "ClientSemanticTokensRequestFullDelta" $ \arg -> ClientSemanticTokensRequestFullDelta <$> arg Language.LSP.Protocol.Types.Common..:!? "delta"
