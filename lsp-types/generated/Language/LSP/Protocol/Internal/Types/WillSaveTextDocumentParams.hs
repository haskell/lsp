{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSaveReason
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in a will save text document notification.
-}
data WillSaveTextDocumentParams = WillSaveTextDocumentParams 
  { {-|
  The document that will be saved.
  -}
  textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The 'TextDocumentSaveReason'.
  -}
  reason :: Language.LSP.Protocol.Internal.Types.TextDocumentSaveReason.TextDocumentSaveReason
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WillSaveTextDocumentParams)

instance Aeson.ToJSON WillSaveTextDocumentParams where
  toJSON (WillSaveTextDocumentParams arg0 arg1) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["reason" Aeson..= arg1]]

instance Aeson.FromJSON WillSaveTextDocumentParams where
  parseJSON = Aeson.withObject "WillSaveTextDocumentParams" $ \arg -> WillSaveTextDocumentParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "reason"
