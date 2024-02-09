{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientCompletionItemInsertTextModeOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientCompletionItemResolveOptions
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemTagOptions
import qualified Language.LSP.Protocol.Internal.Types.MarkupKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientCompletionItemOptions = ClientCompletionItemOptions 
  { {-|
  Client supports snippets as insert text.

  A snippet can define tab stops and placeholders with `$1`, `$2`
  and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  the end of the snippet. Placeholders with equal identifiers are linked,
  that is typing in one will update others too.
  -}
  _snippetSupport :: (Maybe Bool)
  , {-|
  Client supports commit characters on a completion item.
  -}
  _commitCharactersSupport :: (Maybe Bool)
  , {-|
  Client supports the following content formats for the documentation
  property. The order describes the preferred format of the client.
  -}
  _documentationFormat :: (Maybe [Language.LSP.Protocol.Internal.Types.MarkupKind.MarkupKind])
  , {-|
  Client supports the deprecated property on a completion item.
  -}
  _deprecatedSupport :: (Maybe Bool)
  , {-|
  Client supports the preselect property on a completion item.
  -}
  _preselectSupport :: (Maybe Bool)
  , {-|
  Client supports the tag property on a completion item. Clients supporting
  tags have to handle unknown tags gracefully. Clients especially need to
  preserve unknown tags when sending a completion item back to the server in
  a resolve call.

  @since 3.15.0
  -}
  _tagSupport :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionItemTagOptions.CompletionItemTagOptions)
  , {-|
  Client support insert replace edit to control different behavior if a
  completion item is inserted in the text or should replace text.

  @since 3.16.0
  -}
  _insertReplaceSupport :: (Maybe Bool)
  , {-|
  Indicates which properties a client can resolve lazily on a completion
  item. Before version 3.16.0 only the predefined properties `documentation`
  and `details` could be resolved lazily.

  @since 3.16.0
  -}
  _resolveSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientCompletionItemResolveOptions.ClientCompletionItemResolveOptions)
  , {-|
  The client supports the `insertTextMode` property on
  a completion item to override the whitespace handling mode
  as defined by the client (see `insertTextMode`).

  @since 3.16.0
  -}
  _insertTextModeSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientCompletionItemInsertTextModeOptions.ClientCompletionItemInsertTextModeOptions)
  , {-|
  The client has support for completion item label
  details (see also `CompletionItemLabelDetails`).

  @since 3.17.0
  -}
  _labelDetailsSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCompletionItemOptions)

instance Aeson.ToJSON ClientCompletionItemOptions where
  toJSON (ClientCompletionItemOptions arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) = Aeson.object $ concat $  ["snippetSupport" Language.LSP.Protocol.Types.Common..=? arg0
    ,"commitCharactersSupport" Language.LSP.Protocol.Types.Common..=? arg1
    ,"documentationFormat" Language.LSP.Protocol.Types.Common..=? arg2
    ,"deprecatedSupport" Language.LSP.Protocol.Types.Common..=? arg3
    ,"preselectSupport" Language.LSP.Protocol.Types.Common..=? arg4
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg5
    ,"insertReplaceSupport" Language.LSP.Protocol.Types.Common..=? arg6
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg7
    ,"insertTextModeSupport" Language.LSP.Protocol.Types.Common..=? arg8
    ,"labelDetailsSupport" Language.LSP.Protocol.Types.Common..=? arg9]

instance Aeson.FromJSON ClientCompletionItemOptions where
  parseJSON = Aeson.withObject "ClientCompletionItemOptions" $ \arg -> ClientCompletionItemOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "snippetSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "commitCharactersSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentationFormat" <*> arg Language.LSP.Protocol.Types.Common..:!? "deprecatedSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "preselectSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "tagSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertReplaceSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "resolveSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextModeSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "labelDetailsSupport"
