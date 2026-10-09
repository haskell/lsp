{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItemApplyKinds where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ApplyKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Specifies how fields from a completion item should be combined with those
from `completionList.itemDefaults`.

If unspecified, all fields will be treated as ApplyKind.Replace.

If a field's value is ApplyKind.Replace, the value from a completion item (if
provided and not `null`) will always be used instead of the value from
`completionItem.itemDefaults`.

If a field's value is ApplyKind.Merge, the values will be merged using the rules
defined against each field below.

Servers are only allowed to return `applyKind` if the client
signals support for this via the `completionList.applyKindSupport`
capability.

@since 3.18.0
-}
data CompletionItemApplyKinds = CompletionItemApplyKinds 
  { {-|
  Specifies whether commitCharacters on a completion will replace or be
  merged with those in `completionList.itemDefaults.commitCharacters`.

  If ApplyKind.Replace, the commit characters from the completion item will
  always be used unless not provided, in which case those from
  `completionList.itemDefaults.commitCharacters` will be used. An
  empty list can be used if a completion item does not have any commit
  characters and also should not use those from
  `completionList.itemDefaults.commitCharacters`.

  If ApplyKind.Merge the commitCharacters for the completion will be the
  union of all values in both `completionList.itemDefaults.commitCharacters`
  and the completion's own `commitCharacters`.

  @since 3.18.0
  -}
  _commitCharacters :: (Maybe Language.LSP.Protocol.Internal.Types.ApplyKind.ApplyKind)
  , {-|
  Specifies whether the `data` field on a completion will replace or
  be merged with data from `completionList.itemDefaults.data`.

  If ApplyKind.Replace, the data from the completion item will be used if
  provided (and not `null`), otherwise
  `completionList.itemDefaults.data` will be used. An empty object can
  be used if a completion item does not have any data but also should
  not use the value from `completionList.itemDefaults.data`.

  If ApplyKind.Merge, a shallow merge will be performed between
  `completionList.itemDefaults.data` and the completion's own data
  using the following rules:

  - If a completion's `data` field is not provided (or `null`), the
    entire `data` field from `completionList.itemDefaults.data` will be
    used as-is.
  - If a completion's `data` field is provided, each field will
    overwrite the field of the same name in
    `completionList.itemDefaults.data` but no merging of nested fields
    within that value will occur.

  @since 3.18.0
  -}
  _data_ :: (Maybe Language.LSP.Protocol.Internal.Types.ApplyKind.ApplyKind)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionItemApplyKinds)

instance Aeson.ToJSON CompletionItemApplyKinds where
  toJSON (CompletionItemApplyKinds arg0 arg1) = Aeson.object $ concat $  ["commitCharacters" Language.LSP.Protocol.Types.Common..=? arg0
    ,"data" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CompletionItemApplyKinds where
  parseJSON = Aeson.withObject "CompletionItemApplyKinds" $ \arg -> CompletionItemApplyKinds <$> arg Language.LSP.Protocol.Types.Common..:!? "commitCharacters" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
