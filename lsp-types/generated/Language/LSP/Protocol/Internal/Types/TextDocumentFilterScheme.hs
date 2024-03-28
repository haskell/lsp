{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentFilterScheme where

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
A document filter where `scheme` is required field.

@since 3.18.0
@proposed
-}
data TextDocumentFilterScheme = TextDocumentFilterScheme 
  { {-|
  A language id, like `typescript`.
  -}
  language :: (Maybe Data.Text.Text)
  , {-|
  A Uri `Uri.scheme`, like `file` or `untitled`.
  -}
  scheme :: Data.Text.Text
  , {-|
  A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.
  -}
  pattern :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentFilterScheme)

instance Aeson.ToJSON TextDocumentFilterScheme where
  toJSON (TextDocumentFilterScheme arg0 arg1 arg2) = Aeson.object $ concat $  ["language" Language.LSP.Protocol.Types.Common..=? arg0
    ,["scheme" Aeson..= arg1]
    ,"pattern" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON TextDocumentFilterScheme where
  parseJSON = Aeson.withObject "TextDocumentFilterScheme" $ \arg -> TextDocumentFilterScheme <$> arg Language.LSP.Protocol.Types.Common..:!? "language" <*> arg Aeson..: "scheme" <*> arg Language.LSP.Protocol.Types.Common..:!? "pattern"
