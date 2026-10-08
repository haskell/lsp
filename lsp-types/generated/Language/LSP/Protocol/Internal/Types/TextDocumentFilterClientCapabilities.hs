{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentFilterClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data TextDocumentFilterClientCapabilities = TextDocumentFilterClientCapabilities 
  { {-|
  The client supports Relative Patterns.

  @since 3.18.0
  -}
  _relativePatternSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentFilterClientCapabilities)

instance Aeson.ToJSON TextDocumentFilterClientCapabilities where
  toJSON (TextDocumentFilterClientCapabilities arg0) = Aeson.object $ concat $  ["relativePatternSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON TextDocumentFilterClientCapabilities where
  parseJSON = Aeson.withObject "TextDocumentFilterClientCapabilities" $ \arg -> TextDocumentFilterClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "relativePatternSupport"
