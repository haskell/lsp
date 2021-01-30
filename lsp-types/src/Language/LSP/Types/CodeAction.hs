{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.LSP.Types.CodeAction where

import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Default
import           Data.Text                      ( Text )
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

instance ToJSON CodeActionKind where
  toJSON CodeActionEmpty                      = String ""
  toJSON CodeActionQuickFix                   = String "quickfix"
  toJSON CodeActionRefactor                   = String "refactor"
  toJSON CodeActionRefactorExtract            = String "refactor.extract"
  toJSON CodeActionRefactorInline             = String "refactor.inline"
  toJSON CodeActionRefactorRewrite            = String "refactor.rewrite"
  toJSON CodeActionSource                     = String "source"
  toJSON CodeActionSourceOrganizeImports      = String "source.organizeImports"
  toJSON (CodeActionUnknown s)                = String s

instance FromJSON CodeActionKind where
  parseJSON (String "")                       = pure CodeActionEmpty
  parseJSON (String "quickfix")               = pure CodeActionQuickFix
  parseJSON (String "refactor")               = pure CodeActionRefactor
  parseJSON (String "refactor.extract")       = pure CodeActionRefactorExtract
  parseJSON (String "refactor.inline")        = pure CodeActionRefactorInline
  parseJSON (String "refactor.rewrite")       = pure CodeActionRefactorRewrite
  parseJSON (String "source")                 = pure CodeActionSource
  parseJSON (String "source.organizeImports") = pure CodeActionSourceOrganizeImports
  parseJSON (String s)                        = pure (CodeActionUnknown s)
  parseJSON _                                 = mempty
  
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
  def = CodeActionKindClientCapabilities (List allKinds)
    where allKinds = [ CodeActionQuickFix
                     , CodeActionRefactor
                     , CodeActionRefactorExtract
                     , CodeActionRefactorInline
                     , CodeActionRefactorRewrite
                     , CodeActionSource
                     , CodeActionSourceOrganizeImports
                     ]

data CodeActionLiteralSupport =
  CodeActionLiteralSupport
    { _codeActionKind :: CodeActionKindClientCapabilities -- ^ The code action kind is support with the following value set.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionLiteralSupport

data CodeActionClientCapabilities = CodeActionClientCapabilities
  { -- | Whether code action supports dynamic registration.
    _dynamicRegistration :: Maybe Bool,
    -- | The client support code action literals as a valid response
    -- of the `textDocument/codeAction` request.
    -- Since 3.8.0
    _codeActionLiteralSupport :: Maybe CodeActionLiteralSupport,
    -- | Whether code action supports the `isPreferred` property. Since LSP 3.15.0
    _isPreferredSupport :: Maybe Bool
  }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeActionClientCapabilities

-- -------------------------------------

makeExtendingDatatype "CodeActionOptions" [''WorkDoneProgressOptions]
  [("_codeActionKinds", [t| Maybe (List CodeActionKind) |])]
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
    _command :: Maybe Command
  }
  deriving (Read, Show, Eq)
deriveJSON lspOptions ''CodeAction
