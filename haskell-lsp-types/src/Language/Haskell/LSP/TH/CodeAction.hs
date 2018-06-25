{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.CodeAction where

import           Control.Applicative
import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Language.Haskell.LSP.TH.Command
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.Diagnostic
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Message
import           Language.Haskell.LSP.TH.TextDocumentIdentifier
import           Language.Haskell.LSP.TH.WorkspaceEdit


{-
Code Action Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#code-action-request

The code action request is sent from the client to the server tocompute commands
for a given text document and range. These commands are typically code fixes to
either fix problems or to beautify/refactor code. The result of a
textDocument/codeAction request is an array of Command literals which are
typically presented in the user interface. When the command is selected the
server should be contacted again (via the workspace/executeCommand) request to
execute the command.

Since version 3.8.0: support for CodeAction litarals to enable the following
scenarios:

the ability to directly return a workspace edit from e code action request. This
avoids having another server roundtrip to execute an actual code action. However
server providers should be aware that if the code action is expensive to compute
or the edits are huge it might still be beneficial if the result is imply a
command and the actual edit is only computed when needed. the ability to group
code actions using a kind. Clients are allowed to ignore that information.
However it allows them to better group code action for example into
corresponding menus (e.g. all refactor code actions into a refactor menu).
Clients need to announce there support code action literals and code action
kinds via the corresponding client capability
textDocument.codeAction.codeActionLiteralSupport.

Request

    method: 'textDocument/codeAction'
    params: CodeActionParams defined as follows:

/**
 * Params for the CodeActionRequest
 */
interface CodeActionParams {
	/**
	 * The document in which the command was invoked.
	 */
	textDocument: TextDocumentIdentifier;

	/**
	 * The range for which the command was invoked.
	 */
	range: Range;

	/**
	 * Context carrying additional information.
	 */
	context: CodeActionContext;
}

/**
 * The kind of a code action.
 *
 * Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.
 *
 * The set of kinds is open and client needs to announce the kinds it supports to the server during
 * initialization.
 */
export type CodeActionKind = string;

/**
 * A set of predefined code action kinds
 */
export namespace CodeActionKind {
	/**
	 * Base kind for quickfix actions: 'quickfix'
	 */
	export const QuickFix: CodeActionKind = 'quickfix';

	/**
	 * Base kind for refactoring actions: 'refactor'
	 */
	export const Refactor: CodeActionKind = 'refactor';

	/**
	 * Base kind for refactoring extraction actions: 'refactor.extract'
	 *
	 * Example extract actions:
	 *
	 * - Extract method
	 * - Extract function
	 * - Extract variable
	 * - Extract interface from class
	 * - ...
	 */
	export const RefactorExtract: CodeActionKind = 'refactor.extract';

	/**
	 * Base kind for refactoring inline actions: 'refactor.inline'
	 *
	 * Example inline actions:
	 *
	 * - Inline function
	 * - Inline variable
	 * - Inline constant
	 * - ...
	 */
	export const RefactorInline: CodeActionKind = 'refactor.inline';

	/**
	 * Base kind for refactoring rewrite actions: 'refactor.rewrite'
	 *
	 * Example rewrite actions:
	 *
	 * - Convert JavaScript function to class
	 * - Add or remove parameter
	 * - Encapsulate field
	 * - Make method static
	 * - Move method to base class
	 * - ...
	 */
	export const RefactorRewrite: CodeActionKind = 'refactor.rewrite';

	/**
	 * Base kind for source actions: `source`
	 *
	 * Source code actions apply to the entire file.
	 */
	export const Source: CodeActionKind = 'source';

	/**
	 * Base kind for an organize imports source action: `source.organizeImports`
	 */
	export const SourceOrganizeImports: CodeActionKind = 'source.organizeImports';
}

/**
 * Contains additional diagnostic information about the context in which
 * a code action is run.
 */
interface CodeActionContext {
	/**
	 * An array of diagnostics.
	 */
	diagnostics: Diagnostic[];

	/**
	 * Requested kind of actions to return.
	 *
	 * Actions not of this kind are filtered out by the client before being shown. So servers
	 * can omit computing them.
	 */
	only?: CodeActionKind[];
}

Response

    result: (Command | CodeAction)[] | null where CodeAction is defined as follows:
        /**
    * A code action represents a change that can be performed in code, e.g. to fix a problem or
    * to refactor code.
    *
    * A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
    */
    export interface CodeAction {

        /**
        * A short, human-readable, title for this code action.
        */
        title: string;

        /**
        * The kind of the code action.
        *
        * Used to filter code actions.
        */
        kind?: CodeActionKind;

        /**
        * The diagnostics that this code action resolves.
        */
        diagnostics?: Diagnostic[];

        /**
        * The workspace edit this code action performs.
        */
        edit?: WorkspaceEdit;

        /**
        * A command this code action executes. If a code action
        * provides an edit and a command, first the edit is
        * executed and then the command.
        */
        command?: Command;
    }
    error: code and message set in case an exception happens during the code
           action request.

-}

data CodeActionKind = CodeActionQuickFix
                    | CodeActionRefactor
                    | CodeActionRefactorExtract
                    | CodeActionRefactorInline
                    | CodeActionRefactorRewrite
                    | CodeActionSource
                    | CodeActionSourceOrganizeImports
                    | CodeActionUnknown Text
  deriving (Read,Show,Eq)

instance ToJSON CodeActionKind where
  toJSON CodeActionQuickFix                   = String "quickfix"
  toJSON CodeActionRefactor                   = String "refactor"
  toJSON CodeActionRefactorExtract            = String "refactor.extract"
  toJSON CodeActionRefactorInline             = String "refactor.inline"
  toJSON CodeActionRefactorRewrite            = String "refactor.rewrite"
  toJSON CodeActionSource                     = String "source"
  toJSON CodeActionSourceOrganizeImports      = String "source.organizeImports"
  toJSON (CodeActionUnknown s)                = String s

instance FromJSON CodeActionKind where
  parseJSON (String "quickfix")               = pure CodeActionQuickFix
  parseJSON (String "refactor")               = pure CodeActionRefactor
  parseJSON (String "refactor.extract")       = pure CodeActionRefactorExtract
  parseJSON (String "refactor.inline")        = pure CodeActionRefactorInline
  parseJSON (String "refactor.rewrite")       = pure CodeActionRefactorRewrite
  parseJSON (String "source")                 = pure CodeActionSource
  parseJSON (String "source.organizeImports") = pure CodeActionSourceOrganizeImports
  parseJSON (String s)                        = pure (CodeActionUnknown s)
  parseJSON _                                 = mempty

data CodeActionContext =
  CodeActionContext
    { _diagnostics :: List Diagnostic
    , only         :: Maybe (List CodeActionKind)
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeActionContext


data CodeActionParams =
  CodeActionParams
    { _textDocument :: TextDocumentIdentifier
    , _range        :: Range
    , _context      :: CodeActionContext
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeActionParams

data CodeAction =
  -- | A code action represents a change that can be performed in code, e.g. to fix a problem or
  -- to refactor code.
  --
  -- A CodeAction must set either '_edit' and/or a '_command'. If both are supplied,
  -- the '_edit' is applied first, then the '_command' is executed.
  CodeAction
    { _title       :: Text -- ^ A short, human-readable, title for this code action.
    , _kind        :: Maybe CodeActionKind -- ^ The kind of the code action. Used to filter code actions.
    , _diagnostics :: Maybe (List Diagnostic) -- ^ The diagnostics that this code action resolves.
    , _edit        :: Maybe WorkspaceEdit -- ^ The workspace edit this code action performs.
    , _command     :: Maybe Command -- ^ A command this code action executes. If a code action
                                    -- provides an edit and a command, first the edit is
                                    -- executed and then the command.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeAction

data CommandOrCodeAction = CommandOrCodeActionCommand Command
                         | CommandOrCodeActionCodeAction CodeAction
  deriving (Read,Show,Eq)

instance FromJSON CommandOrCodeAction where
  parseJSON x = CommandOrCodeActionCommand <$> parseJSON x
              <|> CommandOrCodeActionCodeAction <$> parseJSON x

instance ToJSON CommandOrCodeAction where
  toJSON (CommandOrCodeActionCommand x) = toJSON x
  toJSON (CommandOrCodeActionCodeAction x) = toJSON x

type CodeActionRequest  = RequestMessage ClientMethod CodeActionParams (Maybe (List CommandOrCodeAction))
type CodeActionResponse = ResponseMessage (Maybe (List CommandOrCodeAction))
