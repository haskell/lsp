{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItemDefaults where

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
import qualified Language.LSP.Protocol.Internal.Types.EditRangeWithInsertReplace
import qualified Language.LSP.Protocol.Internal.Types.InsertTextFormat
import qualified Language.LSP.Protocol.Internal.Types.InsertTextMode
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
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
data CompletionItemDefaults = CompletionItemDefaults 
  { {-|
  A default commit character set.

  @since 3.17.0
  -}
  commitCharacters :: (Maybe [Data.Text.Text])
  , {-|
  A default edit range.

  @since 3.17.0
  -}
  editRange :: (Maybe (Language.LSP.Protocol.Internal.Types.Range.Range Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.EditRangeWithInsertReplace.EditRangeWithInsertReplace))
  , {-|
  A default insert text format.

  @since 3.17.0
  -}
  insertTextFormat :: (Maybe Language.LSP.Protocol.Internal.Types.InsertTextFormat.InsertTextFormat)
  , {-|
  A default insert text mode.

  @since 3.17.0
  -}
  insertTextMode :: (Maybe Language.LSP.Protocol.Internal.Types.InsertTextMode.InsertTextMode)
  , {-|
  A default data value.

  @since 3.17.0
  -}
  data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionItemDefaults)

instance Aeson.ToJSON CompletionItemDefaults where
  toJSON (CompletionItemDefaults arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["commitCharacters" Language.LSP.Protocol.Types.Common..=? arg0
    ,"editRange" Language.LSP.Protocol.Types.Common..=? arg1
    ,"insertTextFormat" Language.LSP.Protocol.Types.Common..=? arg2
    ,"insertTextMode" Language.LSP.Protocol.Types.Common..=? arg3
    ,"data" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON CompletionItemDefaults where
  parseJSON = Aeson.withObject "CompletionItemDefaults" $ \arg -> CompletionItemDefaults <$> arg Language.LSP.Protocol.Types.Common..:!? "commitCharacters" <*> arg Language.LSP.Protocol.Types.Common..:!? "editRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextFormat" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextMode" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
