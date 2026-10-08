{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentResult where

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
Result of the `workspace/textDocumentContent` request.

@since 3.18.0
-}
data TextDocumentContentResult = TextDocumentContentResult 
  { {-|
  The text content of the text document. Please note, that the content of
  any subsequent open notifications for the text document might differ
  from the returned content due to whitespace and line ending
  normalizations done on the client
  -}
  _text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentResult)

instance Aeson.ToJSON TextDocumentContentResult where
  toJSON (TextDocumentContentResult arg0) = Aeson.object $ concat $  [["text" Aeson..= arg0]]

instance Aeson.FromJSON TextDocumentContentResult where
  parseJSON = Aeson.withObject "TextDocumentContentResult" $ \arg -> TextDocumentContentResult <$> arg Aeson..: "text"
