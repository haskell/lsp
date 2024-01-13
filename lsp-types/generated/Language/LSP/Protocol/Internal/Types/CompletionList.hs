{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionList where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.CompletionItem
import qualified Language.LSP.Protocol.Internal.Types.InsertTextFormat
import qualified Language.LSP.Protocol.Internal.Types.InsertTextMode
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a collection of `CompletionItem` to be presented
in the editor.
-}
data CompletionList = CompletionList 
  { {-|
  This list it not complete. Further typing results in recomputing this list.

  Recomputed lists have all their items replaced (not appended) in the
  incomplete completion sessions.
  -}
  _isIncomplete :: Bool
  , {-|
  In many cases the items of an actual completion result share the same
  value for properties like `commitCharacters` or the range of a text
  edit. A completion list can therefore define item defaults which will
  be used if a completion item itself doesn't specify the value.

  If a completion list specifies a default value and a completion item
  also specifies a corresponding value the one from the item is used.

  Servers are only allowed to return default values if the client
  signals support for this via the `completionList.itemDefaults`
  capability.

  @since 3.17.0
  -}
  _itemDefaults :: (Maybe (Row.Rec ("commitCharacters" Row..== (Maybe [Data.Text.Text]) Row..+ ("editRange" Row..== (Maybe (Language.LSP.Protocol.Internal.Types.Range.Range Language.LSP.Protocol.Types.Common.|? (Row.Rec ("insert" Row..== Language.LSP.Protocol.Internal.Types.Range.Range Row..+ ("replace" Row..== Language.LSP.Protocol.Internal.Types.Range.Range Row..+ Row.Empty))))) Row..+ ("insertTextFormat" Row..== (Maybe Language.LSP.Protocol.Internal.Types.InsertTextFormat.InsertTextFormat) Row..+ ("insertTextMode" Row..== (Maybe Language.LSP.Protocol.Internal.Types.InsertTextMode.InsertTextMode) Row..+ ("data" Row..== (Maybe Data.Aeson.Value) Row..+ Row.Empty)))))))
  , {-|
  The completion items.
  -}
  _items :: [Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionList)

instance Aeson.ToJSON CompletionList where
  toJSON (CompletionList arg0 arg1 arg2) = Aeson.object $ concat $  [["isIncomplete" Aeson..= arg0]
    ,"itemDefaults" Language.LSP.Protocol.Types.Common..=? arg1
    ,["items" Aeson..= arg2]]

instance Aeson.FromJSON CompletionList where
  parseJSON = Aeson.withObject "CompletionList" $ \arg -> CompletionList <$> arg Aeson..: "isIncomplete" <*> arg Language.LSP.Protocol.Types.Common..:!? "itemDefaults" <*> arg Aeson..: "items"
