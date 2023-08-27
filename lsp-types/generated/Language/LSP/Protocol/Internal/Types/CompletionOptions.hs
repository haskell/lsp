{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Completion options.
-}
data CompletionOptions = CompletionOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  Most tools trigger completion request automatically without explicitly requesting
  it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
  starts to type an identifier. For example if the user types `c` in a JavaScript file
  code complete will automatically pop up present `console` besides others as a
  completion item. Characters that make up identifiers don't need to be listed here.

  If code complete should automatically be trigger on characters not being valid inside
  an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
  -}
  _triggerCharacters :: (Maybe [Data.Text.Text])
  , {-|
  The list of all possible characters that commit a completion. This field can be used
  if clients don't support individual commit characters per completion item. See
  `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`

  If a server provides both `allCommitCharacters` and commit characters on an individual
  completion item the ones on the completion item win.

  @since 3.2.0
  -}
  _allCommitCharacters :: (Maybe [Data.Text.Text])
  , {-|
  The server provides support to resolve additional
  information for a completion item.
  -}
  _resolveProvider :: (Maybe Bool)
  , {-|
  The server supports the following `CompletionItem` specific
  capabilities.

  @since 3.17.0
  -}
  _completionItem :: (Maybe (Row.Rec ("labelDetailsSupport" Row..== (Maybe Bool) Row..+ Row.Empty)))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionOptions)

instance Aeson.ToJSON CompletionOptions where
  toJSON (CompletionOptions arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"triggerCharacters" Language.LSP.Protocol.Types.Common..=? arg1
    ,"allCommitCharacters" Language.LSP.Protocol.Types.Common..=? arg2
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg3
    ,"completionItem" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON CompletionOptions where
  parseJSON = Aeson.withObject "CompletionOptions" $ \arg -> CompletionOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "triggerCharacters" <*> arg Aeson..:! "allCommitCharacters" <*> arg Aeson..:! "resolveProvider" <*> arg Aeson..:! "completionItem"
