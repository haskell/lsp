{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentChangePartial where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data TextDocumentContentChangePartial = TextDocumentContentChangePartial 
  { {-|
  The range of the document that changed.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The optional length of the range that got replaced.

  @deprecated use range instead.
  -}
  rangeLength :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  The new text for the provided range.
  -}
  text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentChangePartial)

instance Aeson.ToJSON TextDocumentContentChangePartial where
  toJSON (TextDocumentContentChangePartial arg0 arg1 arg2) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"rangeLength" Language.LSP.Protocol.Types.Common..=? arg1
    ,["text" Aeson..= arg2]]

instance Aeson.FromJSON TextDocumentContentChangePartial where
  parseJSON = Aeson.withObject "TextDocumentContentChangePartial" $ \arg -> TextDocumentContentChangePartial <$> arg Aeson..: "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "rangeLength" <*> arg Aeson..: "text"
