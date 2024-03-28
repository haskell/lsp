{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensDeltaParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensDeltaParams = SemanticTokensDeltaParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.
  -}
  partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The text document.
  -}
  textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The result id of a previous response. The result Id can either point to a full response
  or a delta response depending on what was received last.
  -}
  previousResultId :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensDeltaParams)

instance Aeson.ToJSON SemanticTokensDeltaParams where
  toJSON (SemanticTokensDeltaParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["textDocument" Aeson..= arg2]
    ,["previousResultId" Aeson..= arg3]]

instance Aeson.FromJSON SemanticTokensDeltaParams where
  parseJSON = Aeson.withObject "SemanticTokensDeltaParams" $ \arg -> SemanticTokensDeltaParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Language.LSP.Protocol.Types.Common..:!? "partialResultToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..: "previousResultId"
