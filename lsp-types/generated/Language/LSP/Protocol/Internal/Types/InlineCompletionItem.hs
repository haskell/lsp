{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineCompletionItem where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.StringValue
import qualified Language.LSP.Protocol.Types.Common

{-|
An inline completion item represents a text snippet that is proposed inline to complete text that is being typed.

@since 3.18.0
-}
data InlineCompletionItem = InlineCompletionItem 
  { {-|
  The text to replace the range with. Must be set.
  -}
  _insertText :: (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.StringValue.StringValue)
  , {-|
  A text that is used to decide if this inline completion should be shown. When `falsy` the `InlineCompletionItem.insertText` is used.
  -}
  _filterText :: (Maybe Data.Text.Text)
  , {-|
  The range to replace. Must begin and end on the same line.
  -}
  _range :: (Maybe Language.LSP.Protocol.Internal.Types.Range.Range)
  , {-|
  An optional `Command` that is executed *after* inserting this completion.
  -}
  _command :: (Maybe Language.LSP.Protocol.Internal.Types.Command.Command)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineCompletionItem)

instance Aeson.ToJSON InlineCompletionItem where
  toJSON (InlineCompletionItem arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["insertText" Aeson..= arg0]
    ,"filterText" Language.LSP.Protocol.Types.Common..=? arg1
    ,"range" Language.LSP.Protocol.Types.Common..=? arg2
    ,"command" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON InlineCompletionItem where
  parseJSON = Aeson.withObject "InlineCompletionItem" $ \arg -> InlineCompletionItem <$> arg Aeson..: "insertText" <*> arg Language.LSP.Protocol.Types.Common..:!? "filterText" <*> arg Language.LSP.Protocol.Types.Common..:!? "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "command"
