{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineCompletionList where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.InlineCompletionItem
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a collection of `InlineCompletionItem` to be presented in the editor.

@since 3.18.0
-}
data InlineCompletionList = InlineCompletionList 
  { {-|
  The inline completion items
  -}
  _items :: [Language.LSP.Protocol.Internal.Types.InlineCompletionItem.InlineCompletionItem]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineCompletionList)

instance Aeson.ToJSON InlineCompletionList where
  toJSON (InlineCompletionList arg0) = Aeson.object $ concat $  [["items" Aeson..= arg0]]

instance Aeson.FromJSON InlineCompletionList where
  parseJSON = Aeson.withObject "InlineCompletionList" $ \arg -> InlineCompletionList <$> arg Aeson..: "items"
