{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyItem where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Represents programming constructs like functions or constructors in the context
of call hierarchy.

@since 3.16.0
-}
data CallHierarchyItem = CallHierarchyItem 
  { {-|
  The name of this item.
  -}
  name :: Data.Text.Text
  , {-|
  The kind of this item.
  -}
  kind :: Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind
  , {-|
  Tags for this item.
  -}
  tags :: (Maybe [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag])
  , {-|
  More detail for this item, e.g. the signature of a function.
  -}
  detail :: (Maybe Data.Text.Text)
  , {-|
  The resource identifier of this item.
  -}
  uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
  Must be contained by the `CallHierarchyItem.range`.
  -}
  selectionRange :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  A data entry field that is preserved between a call hierarchy prepare and
  incoming calls or outgoing calls requests.
  -}
  data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CallHierarchyItem)

instance Aeson.ToJSON CallHierarchyItem where
  toJSON (CallHierarchyItem arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,["kind" Aeson..= arg1]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg2
    ,"detail" Language.LSP.Protocol.Types.Common..=? arg3
    ,["uri" Aeson..= arg4]
    ,["range" Aeson..= arg5]
    ,["selectionRange" Aeson..= arg6]
    ,"data" Language.LSP.Protocol.Types.Common..=? arg7]

instance Aeson.FromJSON CallHierarchyItem where
  parseJSON = Aeson.withObject "CallHierarchyItem" $ \arg -> CallHierarchyItem <$> arg Aeson..: "name" <*> arg Aeson..: "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "tags" <*> arg Language.LSP.Protocol.Types.Common..:!? "detail" <*> arg Aeson..: "uri" <*> arg Aeson..: "range" <*> arg Aeson..: "selectionRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
