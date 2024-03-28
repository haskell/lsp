{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentFilterPattern where

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
A document filter where `pattern` is required field.

@since 3.18.0
@proposed
-}
data TextDocumentFilterPattern = TextDocumentFilterPattern 
  { {-|
  A language id, like `typescript`.
  -}
  language :: (Maybe Data.Text.Text)
  , {-|
  A Uri `Uri.scheme`, like `file` or `untitled`.
  -}
  scheme :: (Maybe Data.Text.Text)
  , {-|
  A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.
  -}
  pattern :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentFilterPattern)

instance Aeson.ToJSON TextDocumentFilterPattern where
  toJSON (TextDocumentFilterPattern arg0 arg1 arg2) = Aeson.object $ concat $  ["language" Language.LSP.Protocol.Types.Common..=? arg0
    ,"scheme" Language.LSP.Protocol.Types.Common..=? arg1
    ,["pattern" Aeson..= arg2]]

instance Aeson.FromJSON TextDocumentFilterPattern where
  parseJSON = Aeson.withObject "TextDocumentFilterPattern" $ \arg -> TextDocumentFilterPattern <$> arg Language.LSP.Protocol.Types.Common..:!? "language" <*> arg Language.LSP.Protocol.Types.Common..:!? "scheme" <*> arg Aeson..: "pattern"
