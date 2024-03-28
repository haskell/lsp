{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptionsKind
import qualified Language.LSP.Protocol.Internal.Types.CompletionListCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InsertTextMode
import qualified Language.LSP.Protocol.Types.Common

{-|
Completion client capabilities
-}
data CompletionClientCapabilities = CompletionClientCapabilities 
  { {-|
  Whether completion supports dynamic registration.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports the following `CompletionItem` specific
  capabilities.
  -}
  completionItem :: (Maybe Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptions.ClientCompletionItemOptions)
  , {-|

  -}
  completionItemKind :: (Maybe Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptionsKind.ClientCompletionItemOptionsKind)
  , {-|
  Defines how the client handles whitespace and indentation
  when accepting a completion item that uses multi line
  text in either `insertText` or `textEdit`.

  @since 3.17.0
  -}
  insertTextMode :: (Maybe Language.LSP.Protocol.Internal.Types.InsertTextMode.InsertTextMode)
  , {-|
  The client supports to send additional context information for a
  `textDocument/completion` request.
  -}
  contextSupport :: (Maybe Bool)
  , {-|
  The client supports the following `CompletionList` specific
  capabilities.

  @since 3.17.0
  -}
  completionList :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionListCapabilities.CompletionListCapabilities)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionClientCapabilities)

instance Aeson.ToJSON CompletionClientCapabilities where
  toJSON (CompletionClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"completionItem" Language.LSP.Protocol.Types.Common..=? arg1
    ,"completionItemKind" Language.LSP.Protocol.Types.Common..=? arg2
    ,"insertTextMode" Language.LSP.Protocol.Types.Common..=? arg3
    ,"contextSupport" Language.LSP.Protocol.Types.Common..=? arg4
    ,"completionList" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON CompletionClientCapabilities where
  parseJSON = Aeson.withObject "CompletionClientCapabilities" $ \arg -> CompletionClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "completionItem" <*> arg Language.LSP.Protocol.Types.Common..:!? "completionItemKind" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextMode" <*> arg Language.LSP.Protocol.Types.Common..:!? "contextSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "completionList"
