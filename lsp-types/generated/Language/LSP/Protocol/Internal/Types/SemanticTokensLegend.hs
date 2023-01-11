-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensLegend where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
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

instance Aeson.ToJSON SemanticTokensLegend where
  toJSON (SemanticTokensLegend arg0 arg1) = Aeson.object $ concat $  [["tokenTypes" Aeson..= arg0]
    ,["tokenModifiers" Aeson..= arg1]]

instance Aeson.FromJSON SemanticTokensLegend where
  parseJSON = Aeson.withObject "SemanticTokensLegend" $ \arg -> SemanticTokensLegend <$> arg Aeson..: "tokenTypes" <*> arg Aeson..: "tokenModifiers"