-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WillSaveTextDocumentParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
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
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The 'TextDocumentSaveReason'.
  -}
  _reason :: Language.LSP.Protocol.Internal.Types.TextDocumentSaveReason.TextDocumentSaveReason
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WillSaveTextDocumentParams where
  toJSON (WillSaveTextDocumentParams arg0 arg1) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["reason" Aeson..= arg1]]

instance Aeson.FromJSON WillSaveTextDocumentParams where
  parseJSON = Aeson.withObject "WillSaveTextDocumentParams" $ \arg -> WillSaveTextDocumentParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "reason"