{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentFilterLanguage where

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
A document filter where `language` is required field.

@since 3.18.0
@proposed
-}
data TextDocumentFilterLanguage = TextDocumentFilterLanguage 
  { {-|
  A language id, like `typescript`.
  -}
  language :: Data.Text.Text
  , {-|
  A Uri `Uri.scheme`, like `file` or `untitled`.
  -}
  scheme :: (Maybe Data.Text.Text)
  , {-|
  A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.
  -}
  pattern :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentFilterLanguage)

instance Aeson.ToJSON TextDocumentFilterLanguage where
  toJSON (TextDocumentFilterLanguage arg0 arg1 arg2) = Aeson.object $ concat $  [["language" Aeson..= arg0]
    ,"scheme" Language.LSP.Protocol.Types.Common..=? arg1
    ,"pattern" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON TextDocumentFilterLanguage where
  parseJSON = Aeson.withObject "TextDocumentFilterLanguage" $ \arg -> TextDocumentFilterLanguage <$> arg Aeson..: "language" <*> arg Language.LSP.Protocol.Types.Common..:!? "scheme" <*> arg Language.LSP.Protocol.Types.Common..:!? "pattern"
