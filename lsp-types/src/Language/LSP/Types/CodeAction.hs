{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.LSP.Types.CodeAction where

import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Default
import           Data.String
import           Data.Text                      ( Text )
import qualified Data.Text as T
import           Language.LSP.Types.Command
import           Language.LSP.Types.Diagnostic
import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.Progress
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils
import           Language.LSP.Types.WorkspaceEdit


data CodeActionKind
  = -- | Empty kind.
    CodeActionEmpty
  | -- | Base kind for quickfix actions: @quickfix@.
    CodeActionQuickFix
  | -- | Base kind for refactoring actions: @refactor@.
    CodeActionRefactor
  | -- | Base kind for refactoring extraction actions: @refactor.extract@.
    -- Example extract actions:
    --
    -- - Extract method
    -- - Extract function
    -- - Extract variable
    -- - Extract interface from class
    -- - ...
    CodeActionRefactorExtract
  | -- | Base kind for refactoring inline actions: @refactor.inline@.
    --
    -- Example inline actions:
    --
    -- - Inline function
    -- - Inline variable
    -- - Inline constant
    -- - ...
    CodeActionRefactorInline
  | -- | Base kind for refactoring rewrite actions: @refactor.rewrite@.
    --
    -- Example rewrite actions:
    --
    -- - Convert JavaScript function to class
    -- - Add or remove parameter
    -- - Encapsulate field
    -- - Make method static
    -- - Move method to base class
    -- - ...
    CodeActionRefactorRewrite
  | -- | Base kind for source actions: @source@.
    --
    -- Source code actions apply to the entire file.
    CodeActionSource
  | -- | Base kind for an organize imports source action: @source.organizeImports@.
    CodeActionSourceOrganizeImports
  | CodeActionUnknown Text
  deriving (Read, Show, Eq)

fromHierarchicalString :: Text -> CodeActionKind
fromHierarchicalString t = case t of
  ""                       -> CodeActionEmpty
  "quickfix"               -> CodeActionQuickFix
  "refactor"               -> CodeActionRefactor
  "refactor.extract"       -> CodeActionRefactorExtract
  "refactor.inline"        -> CodeActionRefactorInline
  "refactor.rewrite"       -> CodeActionRefactorRewrite
  "source"                 -> CodeActionSource
  "source.organizeImports" -> CodeActionSourceOrganizeImports
  s                        -> CodeActionUnknown s

toHierarchicalString :: CodeActionKind -> Text
toHierarchicalString k = case k of
  CodeActionEmpty                 -> ""
  CodeActionQuickFix              -> "quickfix"
  CodeActionRefactor              -> "refactor"
  CodeActionRefactorExtract       -> "refactor.extract"
  CodeActionRefactorInline        -> "refactor.inline"
  CodeActionRefactorRewrite       -> "refactor.rewrite"
  CodeActionSource                -> "source"
  CodeActionSourceOrganizeImports -> "source.organizeImports"
  (CodeActionUnknown s)           -> s

instance IsString CodeActionKind where
  fromString = fromHierarchicalString . T.pack

instance ToJSON CodeActionKind where
  toJSON = String . toHierarchicalString

instance FromJSON CodeActionKind where
  parseJSON (String s) = pure $ fromHierarchicalString s
  parseJSON _          = fail "CodeActionKind"

-- | Does the first 'CodeActionKind' subsume the other one, hierarchically. Reflexive.
codeActionKindSubsumes :: CodeActionKind -> CodeActionKind -> Bool
-- Simple but ugly implementation: prefix on the string representation
codeActionKindSubsumes parent child = toHierarchicalString parent `T.isPrefixOf` toHierarchicalString child

-- | The 'CodeActionKind's listed in the LSP spec specifically.
specCodeActionKinds :: [CodeActionKind]
specCodeActionKinds = [
  CodeActionQuickFix
  , CodeActionRefactor
  , CodeActionRefactorExtract
  , CodeActionRefactorInline
  , CodeActionRefactorRewrite
  , CodeActionSource
  , CodeActionSourceOrganizeImports
  ]

-- -------------------------------------

data CodeActionKindClientCapabilities =
  CodeActionKindClientCapabilities
   { -- | The code action kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
      _valueSet :: List CodeActionKind
   } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionKindClientCapabilities

instance Default CodeActionKindClientCapabilities where
  def = CodeActionKindClientCapabilities (List specCodeActionKinds)

data CodeActionLiteralSupport =
  CodeActionLiteralSupport
    { _codeActionKind :: CodeActionKindClientCapabilities -- ^ The code action kind is support with the following value set.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionLiteralSupport

data CodeActionResolveClientCapabilities =
  CodeActionResolveClientCapabilities
    { _properties :: List Text -- ^ The properties that a client can resolve lazily.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionResolveClientCapabilities

data CodeActionClientCapabilities = CodeActionClientCapabilities
  { -- | Whether code action supports dynamic registration.
    _dynamicRegistration :: Maybe Bool,
    -- | The client support code action literals as a valid response
    -- of the `textDocument/codeAction` request.
    -- Since 3.8.0
    _codeActionLiteralSupport :: Maybe CodeActionLiteralSupport,
    -- | Whether code action supports the `isPreferred` property. Since LSP 3.15.0
    _isPreferredSupport :: Maybe Bool,
    -- | Whether code action supports the `disabled` property.
    --
    -- @since 3.16.0
    _disabledSupport :: Maybe Bool,
    -- | Whether code action supports the `data` property which is
    -- preserved between a `textDocument/codeAction` and a
    -- `codeAction/resolve` request.
    --
    -- @since 3.16.0
    _dataSupport :: Maybe Bool,
    -- | Whether the client supports resolving additional code action
    -- properties via a separate `codeAction/resolve` request.
    --
    -- @since 3.16.0
    _resolveSupport :: Maybe CodeActionResolveClientCapabilities,
    -- | Whether the client honors the change annotations in
    -- text edits and resource operations returned via the
    -- `CodeAction#edit` property by for example presenting
    -- the workspace edit in the user interface and asking
    -- for confirmation.
    --
    -- @since 3.16.0
    _honorsChangeAnnotations :: Maybe Bool
  }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionClientCapabilities

-- -------------------------------------

makeExtendingDatatype "CodeActionOptions" [''WorkDoneProgressOptions]
  [("_codeActionKinds", [t| Maybe (List CodeActionKind) |]), ("_resolveProvider", [t| Maybe Bool |]) ]
deriveJSON lspOptions ''CodeActionOptions

makeExtendingDatatype "CodeActionRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''CodeActionOptions
  ] []
deriveJSON lspOptions ''CodeActionRegistrationOptions

-- -------------------------------------

-- | Contains additional diagnostic information about the context in which a
-- code action is run.
data CodeActionContext = CodeActionContext
  { -- | An array of diagnostics known on the client side overlapping the range provided to the
    -- @textDocument/codeAction@ request. They are provided so that the server knows which
    -- errors are currently presented to the user for the given range. There is no guarantee
    -- that these accurately reflect the error state of the resource. The primary parameter
    -- to compute code actions is the provided range.
    _diagnostics :: List Diagnostic
    -- | Requested kind of actions to return.
    --
    -- Actions not of this kind are filtered out by the client before being shown. So servers
    -- can omit computing them.
  , _only :: Maybe (List CodeActionKind)
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''CodeActionContext

makeExtendingDatatype "CodeActionParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [ ("_textDocument", [t|TextDocumentIdentifier|]),
    ("_range", [t|Range|]),
    ("_context", [t|CodeActionContext|])
  ]
deriveJSON lspOptions ''CodeActionParams

newtype Reason = Reason {_reason :: Text}
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''Reason

-- | A code action represents a change that can be performed in code, e.g. to fix a problem or
-- to refactor code.
--
-- A CodeAction must set either '_edit' and/or a '_command'. If both are supplied,
-- the '_edit' is applied first, then the '_command' is executed.
data CodeAction =
  CodeAction
  { -- | A short, human-readable, title for this code action.
    _title :: Text,
    -- | The kind of the code action. Used to filter code actions.
    _kind :: Maybe CodeActionKind,
    -- | The diagnostics that this code action resolves.
    _diagnostics :: Maybe (List Diagnostic),
    -- | Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
    -- by keybindings.
    --
    -- A quick fix should be marked preferred if it properly addresses the underlying error.
    -- A refactoring should be marked preferred if it is the most reasonable choice of actions to take.
    --
    -- Since LSP 3.15.0
    _isPreferred :: Maybe Bool,
    _disabled    :: Maybe Reason, -- ^ Marks that the code action cannot currently be applied.
    -- | The workspace edit this code action performs.
    _edit :: Maybe WorkspaceEdit,
    -- | A command this code action executes. If a code action
    -- provides an edit and a command, first the edit is
    -- executed and then the command.
    _command :: Maybe Command,
    -- | A data entry field that is preserved on a code action between
    -- a `textDocument/codeAction` and a `codeAction/resolve` request.
    --
    -- @since 3.16.0
    _xdata :: Maybe Value
  }
  deriving (Read, Show, Eq)
deriveJSON lspOptions ''CodeAction
