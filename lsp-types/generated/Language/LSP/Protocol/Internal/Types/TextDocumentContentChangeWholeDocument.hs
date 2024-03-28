{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeWholeDocument where

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
@since 3.18.0
@proposed
-}
data TextDocumentContentChangeWholeDocument = TextDocumentContentChangeWholeDocument 
  { {-|
  The new text of the whole document.
  -}
  text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentChangeWholeDocument)

instance Aeson.ToJSON TextDocumentContentChangeWholeDocument where
  toJSON (TextDocumentContentChangeWholeDocument arg0) = Aeson.object $ concat $  [["text" Aeson..= arg0]]

instance Aeson.FromJSON TextDocumentContentChangeWholeDocument where
  parseJSON = Aeson.withObject "TextDocumentContentChangeWholeDocument" $ \arg -> TextDocumentContentChangeWholeDocument <$> arg Aeson..: "text"
