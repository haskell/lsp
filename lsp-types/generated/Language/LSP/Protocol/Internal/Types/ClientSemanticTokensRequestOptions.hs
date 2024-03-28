{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestFullDelta
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientSemanticTokensRequestOptions = ClientSemanticTokensRequestOptions 
  { {-|
  The client will send the `textDocument/semanticTokens/range` request if
  the server provides a corresponding handler.
  -}
  range :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec Row.Empty)))
  , {-|
  The client will send the `textDocument/semanticTokens/full` request if
  the server provides a corresponding handler.
  -}
  full :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.ClientSemanticTokensRequestFullDelta.ClientSemanticTokensRequestFullDelta))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSemanticTokensRequestOptions)

instance Aeson.ToJSON ClientSemanticTokensRequestOptions where
  toJSON (ClientSemanticTokensRequestOptions arg0 arg1) = Aeson.object $ concat $  ["range" Language.LSP.Protocol.Types.Common..=? arg0
    ,"full" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ClientSemanticTokensRequestOptions where
  parseJSON = Aeson.withObject "ClientSemanticTokensRequestOptions" $ \arg -> ClientSemanticTokensRequestOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "full"
