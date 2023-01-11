-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensPartialResult where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0

-}
data SemanticTokensPartialResult = SemanticTokensPartialResult 
  { {-|

  -}
  _data_ :: [Language.LSP.Protocol.Types.Common.UInt]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SemanticTokensPartialResult where
  toJSON (SemanticTokensPartialResult arg0) = Aeson.object $ concat $  [["data" Aeson..= arg0]]

instance Aeson.FromJSON SemanticTokensPartialResult where
  parseJSON = Aeson.withObject "SemanticTokensPartialResult" $ \arg -> SemanticTokensPartialResult <$> arg Aeson..: "data"