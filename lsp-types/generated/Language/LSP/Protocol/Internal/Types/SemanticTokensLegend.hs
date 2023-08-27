{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensLegend where

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
@since 3.16.0
-}
data SemanticTokensLegend = SemanticTokensLegend 
  { {-|
  The token types a server uses.
  -}
  _tokenTypes :: [Data.Text.Text]
  , {-|
  The token modifiers a server uses.
  -}
  _tokenModifiers :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensLegend)

instance Aeson.ToJSON SemanticTokensLegend where
  toJSON (SemanticTokensLegend arg0 arg1) = Aeson.object $ concat $  [["tokenTypes" Aeson..= arg0]
    ,["tokenModifiers" Aeson..= arg1]]

instance Aeson.FromJSON SemanticTokensLegend where
  parseJSON = Aeson.withObject "SemanticTokensLegend" $ \arg -> SemanticTokensLegend <$> arg Aeson..: "tokenTypes" <*> arg Aeson..: "tokenModifiers"
