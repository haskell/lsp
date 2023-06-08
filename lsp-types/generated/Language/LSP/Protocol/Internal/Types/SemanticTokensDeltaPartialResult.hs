-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaPartialResult where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensDeltaPartialResult = SemanticTokensDeltaPartialResult 
  { {-|

  -}
  _edits :: [Language.LSP.Protocol.Internal.Types.SemanticTokensEdit.SemanticTokensEdit]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SemanticTokensDeltaPartialResult where
  toJSON (SemanticTokensDeltaPartialResult arg0) = Aeson.object $ concat $  [["edits" Aeson..= arg0]]

instance Aeson.FromJSON SemanticTokensDeltaPartialResult where
  parseJSON = Aeson.withObject "SemanticTokensDeltaPartialResult" $ \arg -> SemanticTokensDeltaPartialResult <$> arg Aeson..: "edits"