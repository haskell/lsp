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
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CompletionItem
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemApplyKinds
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemDefaults
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
  also specifies a corresponding value, the rules for combining these are
  defined by `applyKinds` (if the client supports it), defaulting to
  ApplyKind.Replace.

  Servers are only allowed to return default values if the client
  signals support for this via the `completionList.itemDefaults`
  capability.

  @since 3.17.0
  -}
  _itemDefaults :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionItemDefaults.CompletionItemDefaults)
  , {-|
  Specifies how fields from a completion item should be combined with those
  from `completionList.itemDefaults`.

  If unspecified, all fields will be treated as ApplyKind.Replace.

  If a field's value is ApplyKind.Replace, the value from a completion item
  (if provided and not `null`) will always be used instead of the value
  from `completionItem.itemDefaults`.

  If a field's value is ApplyKind.Merge, the values will be merged using
  the rules defined against each field below.

  Servers are only allowed to return `applyKind` if the client
  signals support for this via the `completionList.applyKindSupport`
  capability.

  @since 3.18.0
  -}
  _applyKind :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionItemApplyKinds.CompletionItemApplyKinds)
  , {-|
  The completion items.
  -}
  _items :: [Language.LSP.Protocol.Internal.Types.CompletionItem.CompletionItem]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionList)

instance Aeson.ToJSON CompletionList where
  toJSON (CompletionList arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["isIncomplete" Aeson..= arg0]
    ,"itemDefaults" Language.LSP.Protocol.Types.Common..=? arg1
    ,"applyKind" Language.LSP.Protocol.Types.Common..=? arg2
    ,["items" Aeson..= arg3]]

instance Aeson.FromJSON CompletionList where
  parseJSON = Aeson.withObject "CompletionList" $ \arg -> CompletionList <$> arg Aeson..: "isIncomplete" <*> arg Language.LSP.Protocol.Types.Common..:!? "itemDefaults" <*> arg Language.LSP.Protocol.Types.Common..:!? "applyKind" <*> arg Aeson..: "items"
