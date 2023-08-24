-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidOpenTextDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentItem
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in an open text document notification
-}
data DidOpenTextDocumentParams = DidOpenTextDocumentParams 
  { {-|
  The document that was opened.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentItem.TextDocumentItem
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidOpenTextDocumentParams)

instance Aeson.ToJSON DidOpenTextDocumentParams where
  toJSON (DidOpenTextDocumentParams arg0) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]]

instance Aeson.FromJSON DidOpenTextDocumentParams where
  parseJSON = Aeson.withObject "DidOpenTextDocumentParams" $ \arg -> DidOpenTextDocumentParams <$> arg Aeson..: "textDocument"
