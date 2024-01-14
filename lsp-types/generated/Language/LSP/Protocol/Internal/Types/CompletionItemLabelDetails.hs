{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItemLabelDetails where

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
Additional details for a completion item label.

@since 3.17.0
-}
data CompletionItemLabelDetails = CompletionItemLabelDetails 
  { {-|
  An optional string which is rendered less prominently directly after `CompletionItem.label`,
  without any spacing. Should be used for function signatures and type annotations.
  -}
  _detail :: (Maybe Data.Text.Text)
  , {-|
  An optional string which is rendered less prominently after `CompletionItem.detail`. Should be used
  for fully qualified names and file paths.
  -}
  _description :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionItemLabelDetails)

instance Aeson.ToJSON CompletionItemLabelDetails where
  toJSON (CompletionItemLabelDetails arg0 arg1) = Aeson.object $ concat $  ["detail" Language.LSP.Protocol.Types.Common..=? arg0
    ,"description" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CompletionItemLabelDetails where
  parseJSON = Aeson.withObject "CompletionItemLabelDetails" $ \arg -> CompletionItemLabelDetails <$> arg Language.LSP.Protocol.Types.Common..:!? "detail" <*> arg Language.LSP.Protocol.Types.Common..:!? "description"
