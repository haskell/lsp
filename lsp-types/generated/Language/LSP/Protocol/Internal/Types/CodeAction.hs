{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeAction where

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
import qualified Language.LSP.Protocol.Internal.Types.CodeActionDisabled
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
A code action represents a change that can be performed in code, e.g. to fix a problem or
to refactor code.

A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
-}
data CodeAction = CodeAction 
  { {-|
  A short, human-readable, title for this code action.
  -}
  title :: Data.Text.Text
  , {-|
  The kind of the code action.

  Used to filter code actions.
  -}
  kind :: (Maybe Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind)
  , {-|
  The diagnostics that this code action resolves.
  -}
  diagnostics :: (Maybe [Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic])
  , {-|
  Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
  by keybindings.

  A quick fix should be marked preferred if it properly addresses the underlying error.
  A refactoring should be marked preferred if it is the most reasonable choice of actions to take.

  @since 3.15.0
  -}
  isPreferred :: (Maybe Bool)
  , {-|
  Marks that the code action cannot currently be applied.

  Clients should follow the following guidelines regarding disabled code actions:

    - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)
      code action menus.

    - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type
      of code action, such as refactorings.

    - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)
      that auto applies a code action and only disabled code actions are returned, the client should show the user an
      error message with `reason` in the editor.

  @since 3.16.0
  -}
  disabled :: (Maybe Language.LSP.Protocol.Internal.Types.CodeActionDisabled.CodeActionDisabled)
  , {-|
  The workspace edit this code action performs.
  -}
  edit :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit)
  , {-|
  A command this code action executes. If a code action
  provides an edit and a command, first the edit is
  executed and then the command.
  -}
  command :: (Maybe Language.LSP.Protocol.Internal.Types.Command.Command)
  , {-|
  A data entry field that is preserved on a code action between
  a `textDocument/codeAction` and a `codeAction/resolve` request.

  @since 3.16.0
  -}
  data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeAction)

instance Aeson.ToJSON CodeAction where
  toJSON (CodeAction arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) = Aeson.object $ concat $  [["title" Aeson..= arg0]
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg1
    ,"diagnostics" Language.LSP.Protocol.Types.Common..=? arg2
    ,"isPreferred" Language.LSP.Protocol.Types.Common..=? arg3
    ,"disabled" Language.LSP.Protocol.Types.Common..=? arg4
    ,"edit" Language.LSP.Protocol.Types.Common..=? arg5
    ,"command" Language.LSP.Protocol.Types.Common..=? arg6
    ,"data" Language.LSP.Protocol.Types.Common..=? arg7]

instance Aeson.FromJSON CodeAction where
  parseJSON = Aeson.withObject "CodeAction" $ \arg -> CodeAction <$> arg Aeson..: "title" <*> arg Language.LSP.Protocol.Types.Common..:!? "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "diagnostics" <*> arg Language.LSP.Protocol.Types.Common..:!? "isPreferred" <*> arg Language.LSP.Protocol.Types.Common..:!? "disabled" <*> arg Language.LSP.Protocol.Types.Common..:!? "edit" <*> arg Language.LSP.Protocol.Types.Common..:!? "command" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
