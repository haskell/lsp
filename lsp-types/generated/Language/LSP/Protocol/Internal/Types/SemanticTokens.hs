-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokens where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokens = SemanticTokens 
  { {-|
  An optional result id. If provided and clients support delta updating
  the client will include the result id in the next semantic token request.
  A server can then instead of computing all semantic tokens again simply
  send a delta.
  -}
  _resultId :: (Maybe Data.Text.Text)
  , {-|
  The actual tokens.
  -}
  _data_ :: [Language.LSP.Protocol.Types.Common.UInt]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SemanticTokens where
  toJSON (SemanticTokens arg0 arg1) = Aeson.object $ concat $  ["resultId" Language.LSP.Protocol.Types.Common..=? arg0
    ,["data" Aeson..= arg1]]

instance Aeson.FromJSON SemanticTokens where
  parseJSON = Aeson.withObject "SemanticTokens" $ \arg -> SemanticTokens <$> arg Aeson..:! "resultId" <*> arg Aeson..: "data"