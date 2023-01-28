{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.LSP.Types.Completion where

import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Language.LSP.Types.Command
import           Language.LSP.Types.Common
import           Language.LSP.Types.MarkupContent
import           Language.LSP.Types.Progress
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils
import           Language.LSP.Types.WorkspaceEdit
import           Language.LSP.Types.Location (Range)

data CompletionItemKind = CiText
                        | CiMethod
                        | CiFunction
                        | CiConstructor
                        | CiField
                        | CiVariable
                        | CiClass
                        | CiInterface
                        | CiModule
                        | CiProperty
                        | CiUnit
                        | CiValue
                        | CiEnum
                        | CiKeyword
                        | CiSnippet
                        | CiColor
                        | CiFile
                        | CiReference
                        | CiFolder
                        | CiEnumMember
                        | CiConstant
                        | CiStruct
                        | CiEvent
                        | CiOperator
                        | CiTypeParameter
         deriving (Read,Show,Eq,Ord)

instance A.ToJSON CompletionItemKind where
  toJSON CiText          = A.Number 1
  toJSON CiMethod        = A.Number 2
  toJSON CiFunction      = A.Number 3
  toJSON CiConstructor   = A.Number 4
  toJSON CiField         = A.Number 5
  toJSON CiVariable      = A.Number 6
  toJSON CiClass         = A.Number 7
  toJSON CiInterface     = A.Number 8
  toJSON CiModule        = A.Number 9
  toJSON CiProperty      = A.Number 10
  toJSON CiUnit          = A.Number 11
  toJSON CiValue         = A.Number 12
  toJSON CiEnum          = A.Number 13
  toJSON CiKeyword       = A.Number 14
  toJSON CiSnippet       = A.Number 15
  toJSON CiColor         = A.Number 16
  toJSON CiFile          = A.Number 17
  toJSON CiReference     = A.Number 18
  toJSON CiFolder        = A.Number 19
  toJSON CiEnumMember    = A.Number 20
  toJSON CiConstant      = A.Number 21
  toJSON CiStruct        = A.Number 22
  toJSON CiEvent         = A.Number 23
  toJSON CiOperator      = A.Number 24
  toJSON CiTypeParameter = A.Number 25

instance A.FromJSON CompletionItemKind where
  parseJSON (A.Number  1) = pure CiText
  parseJSON (A.Number  2) = pure CiMethod
  parseJSON (A.Number  3) = pure CiFunction
  parseJSON (A.Number  4) = pure CiConstructor
  parseJSON (A.Number  5) = pure CiField
  parseJSON (A.Number  6) = pure CiVariable
  parseJSON (A.Number  7) = pure CiClass
  parseJSON (A.Number  8) = pure CiInterface
  parseJSON (A.Number  9) = pure CiModule
  parseJSON (A.Number 10) = pure CiProperty
  parseJSON (A.Number 11) = pure CiUnit
  parseJSON (A.Number 12) = pure CiValue
  parseJSON (A.Number 13) = pure CiEnum
  parseJSON (A.Number 14) = pure CiKeyword
  parseJSON (A.Number 15) = pure CiSnippet
  parseJSON (A.Number 16) = pure CiColor
  parseJSON (A.Number 17) = pure CiFile
  parseJSON (A.Number 18) = pure CiReference
  parseJSON (A.Number 19) = pure CiFolder
  parseJSON (A.Number 20) = pure CiEnumMember
  parseJSON (A.Number 21) = pure CiConstant
  parseJSON (A.Number 22) = pure CiStruct
  parseJSON (A.Number 23) = pure CiEvent
  parseJSON (A.Number 24) = pure CiOperator
  parseJSON (A.Number 25) = pure CiTypeParameter
  parseJSON _             = fail "CompletionItemKind"

data CompletionItemTag
  -- | Render a completion as obsolete, usually using a strike-out.
  = CitDeprecated
  | CitUnknown Scientific
  deriving (Eq, Ord, Show, Read)

instance A.ToJSON CompletionItemTag where
  toJSON CitDeprecated  = A.Number 1
  toJSON (CitUnknown i) = A.Number i

instance A.FromJSON CompletionItemTag where
  parseJSON (A.Number 1) = pure CitDeprecated
  parseJSON _            = fail "CompletionItemTag"

data CompletionItemTagsClientCapabilities =
  CompletionItemTagsClientCapabilities
    { -- | The tag supported by the client.
      _valueSet :: List CompletionItemTag
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionItemTagsClientCapabilities

data CompletionItemResolveClientCapabilities =
  CompletionItemResolveClientCapabilities
    { -- | The properties that a client can resolve lazily.
      _properties :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionItemResolveClientCapabilities

{-|
How whitespace and indentation is handled during completion
item insertion.

@since 3.16.0
-}
data InsertTextMode =
  -- | The insertion or replace strings is taken as it is. If the
  -- value is multi line the lines below the cursor will be
  -- inserted using the indentation defined in the string value.
  -- The client will not apply any kind of adjustments to the
  -- string.
  AsIs
  -- | The editor adjusts leading whitespace of new lines so that
  -- they match the indentation up to the cursor of the line for
  -- which the item is accepted.
  --
  -- Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
  -- multi line completion item is indented using 2 tabs and all
  -- following lines inserted will be indented using 2 tabs as well.
  | AdjustIndentation
  deriving (Read,Show,Eq)

instance A.ToJSON InsertTextMode where
  toJSON AsIs              = A.Number 1
  toJSON AdjustIndentation = A.Number 2

instance A.FromJSON InsertTextMode where
  parseJSON (A.Number 1) = pure AsIs
  parseJSON (A.Number 2) = pure AdjustIndentation
  parseJSON _            = fail "InsertTextMode"

data CompletionItemInsertTextModeClientCapabilities =
  CompletionItemInsertTextModeClientCapabilities
    { _valueSet :: List InsertTextMode
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionItemInsertTextModeClientCapabilities

data CompletionItemClientCapabilities =
  CompletionItemClientCapabilities
    { -- | Client supports snippets as insert text.
      --
      -- A snippet can define tab stops and placeholders with `$1`, `$2` and
      -- `${3:foo}`. `$0` defines the final tab stop, it defaults to the end of
      -- the snippet. Placeholders with equal identifiers are linked, that is
      -- typing in one will update others too.
      _snippetSupport :: Maybe Bool

      -- | Client supports commit characters on a completion item.
    , _commitCharactersSupport :: Maybe Bool

      -- | Client supports the follow content formats for the documentation
      -- property. The order describes the preferred format of the client.
    , _documentationFormat :: Maybe (List MarkupKind)

      -- | Client supports the deprecated property on a completion item.
    , _deprecatedSupport :: Maybe Bool

      -- | Client supports the preselect property on a completion item.
    , _preselectSupport :: Maybe Bool

      -- | Client supports the tag property on a completion item. Clients
      -- supporting tags have to handle unknown tags gracefully. Clients
      -- especially need to preserve unknown tags when sending a
      -- completion item back to the server in a resolve call.
      --
      -- @since 3.15.0
    , _tagSupport :: Maybe CompletionItemTagsClientCapabilities
      -- | Client supports insert replace edit to control different behavior if
      -- completion item is inserted in the text or should replace text.
      --
      -- @since 3.16.0
    , _insertReplaceSupport :: Maybe Bool
      -- | Indicates which properties a client can resolve lazily on a
      -- completion item. Before version 3.16.0 only the predefined properties
      -- `documentation` and `details` could be resolved lazily.
      --
      -- @since 3.16.0
    , _resolveSupport :: Maybe CompletionItemResolveClientCapabilities
      -- | The client supports the `insertTextMode` property on
      -- a completion item to override the whitespace handling mode
      -- as defined by the client (see `insertTextMode`).
      --
      -- @since 3.16.0
    , _insertTextModeSupport :: Maybe CompletionItemInsertTextModeClientCapabilities
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionItemClientCapabilities

data CompletionItemKindClientCapabilities =
  CompletionItemKindClientCapabilities
    { -- | The completion item kind values the client supports. When this
      -- property exists the client also guarantees that it will
      --  handle values outside its set gracefully and falls back
      --  to a default value when unknown.
      _valueSet :: Maybe (List CompletionItemKind)
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionItemKindClientCapabilities

data CompletionClientCapabilities =
  CompletionClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^ Whether completion supports dynamic
                                         -- registration.
    , _completionItem :: Maybe CompletionItemClientCapabilities
    , _completionItemKind :: Maybe CompletionItemKindClientCapabilities
    , _contextSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CompletionClientCapabilities

-- -------------------------------------

data InsertTextFormat
  = PlainText -- ^The primary text to be inserted is treated as a plain string.
  | Snippet
      -- ^ The primary text to be inserted is treated as a snippet.
      --
      -- A snippet can define tab stops and placeholders with `$1`, `$2`
      -- and `${3:foo}`. `$0` defines the final tab stop, it defaults to
      -- the end of the snippet. Placeholders with equal identifiers are linked,
      -- that is typing in one will update others too.
      --
      -- See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
    deriving (Show, Read, Eq)

instance A.ToJSON InsertTextFormat where
  toJSON PlainText = A.Number 1
  toJSON Snippet   = A.Number 2

instance A.FromJSON InsertTextFormat where
  parseJSON (A.Number  1) = pure PlainText
  parseJSON (A.Number  2) = pure Snippet
  parseJSON _             = fail "InsertTextFormat"

data CompletionDoc = CompletionDocString Text
                   | CompletionDocMarkup MarkupContent
  deriving (Show, Read, Eq)

deriveJSON lspOptionsUntagged ''CompletionDoc

data InsertReplaceEdit =
  InsertReplaceEdit
    { _newText :: Text -- ^ The string to be inserted.
    , _insert  :: Range -- ^ The range if the insert is requested
    , _repalce :: Range -- ^ The range if the replace is requested.
    }
  deriving (Read,Show,Eq)
deriveJSON lspOptions ''InsertReplaceEdit

data CompletionEdit = CompletionEditText TextEdit | CompletionEditInsertReplace InsertReplaceEdit
  deriving (Read,Show,Eq)

deriveJSON lspOptionsUntagged ''CompletionEdit

data CompletionItem =
  CompletionItem
    { _label               :: Text -- ^ The label of this completion item. By default also
                       -- the text that is inserted when selecting this
                       -- completion.
    , _kind                :: Maybe CompletionItemKind
    , _tags                :: Maybe (List CompletionItemTag) -- ^ Tags for this completion item.
    , _detail              :: Maybe Text -- ^ A human-readable string with additional
                              -- information about this item, like type or
                              -- symbol information.
    , _documentation       :: Maybe CompletionDoc -- ^ A human-readable string that represents
                                                  -- a doc-comment.
    , _deprecated          :: Maybe Bool -- ^ Indicates if this item is deprecated.
    , _preselect           :: Maybe Bool
         -- ^ Select this item when showing.
         -- /Note/ that only one completion item can be selected and that the
         -- tool \/ client decides which item that is. The rule is that the /first/
         -- item of those that match best is selected.
    , _sortText            :: Maybe Text -- ^ A string that should be used when filtering
                                -- a set of completion items. When `falsy` the
                                -- label is used.
    , _filterText          :: Maybe Text -- ^ A string that should be used when
                                  -- filtering a set of completion items. When
                                  -- `falsy` the label is used.
    , _insertText          :: Maybe Text -- ^ A string that should be inserted a
                                  -- document when selecting this completion.
                                  -- When `falsy` the label is used.
    , _insertTextFormat    :: Maybe InsertTextFormat
         -- ^ The format of the insert text. The format applies to both the
         -- `insertText` property and the `newText` property of a provided
         -- `textEdit`.
    , _insertTextMode      :: Maybe InsertTextMode
         -- ^ How whitespace and indentation is handled during completion
         -- item insertion. If not provided the client's default value depends on
         -- the @textDocument.completion.insertTextMode@ client capability.
    , _textEdit            :: Maybe CompletionEdit
         -- ^ An edit which is applied to a document when selecting this
         -- completion. When an edit is provided the value of `insertText` is
         -- ignored.
         --
         -- /Note/: The range of the edit must be a single line range and it
         -- must contain the position at which completion has been requested.
    , _additionalTextEdits :: Maybe (List TextEdit)
         -- ^ An optional array of additional text edits that are applied when
         -- selecting this completion. Edits must not overlap with the main edit
         -- nor with themselves.
    , _commitCharacters    :: Maybe (List Text)
         -- ^ An optional set of characters that when pressed while this completion
         -- is active will accept it first and then type that character. /Note/
         -- that all commit characters should have `length=1` and that superfluous
         -- characters will be ignored.
    , _command             :: Maybe Command
        -- ^ An optional command that is executed /after/ inserting this
        -- completion. /Note/ that additional modifications to the current
        -- document should be described with the additionalTextEdits-property.
    , _xdata               :: Maybe A.Value -- ^ An data entry field that is preserved on a
                              -- completion item between a completion and a
                              -- completion resolve request.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CompletionItem

-- | Represents a collection of 'CompletionItem's to be presented in the editor.
data CompletionList =
  CompletionList
    { _isIncomplete :: Bool -- ^ This list it not complete. Further typing
                            -- should result in recomputing this list.
    , _items        :: List CompletionItem -- ^ The completion items.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CompletionList

-- | How a completion was triggered
data CompletionTriggerKind = -- | Completion was triggered by typing an identifier (24x7 code
                             -- complete), manual invocation (e.g Ctrl+Space) or via API.
                             CtInvoked
                             -- | Completion was triggered by a trigger character specified by
                             -- the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
                           | CtTriggerCharacter
                             -- | Completion was re-triggered as the current completion list is incomplete.
                           | CtTriggerForIncompleteCompletions
                             -- | An unknown 'CompletionTriggerKind' not yet supported in haskell-lsp.
                           | CtUnknown Scientific
  deriving (Read, Show, Eq)

instance A.ToJSON CompletionTriggerKind where
  toJSON CtInvoked                         = A.Number 1
  toJSON CtTriggerCharacter                = A.Number 2
  toJSON CtTriggerForIncompleteCompletions = A.Number 3
  toJSON (CtUnknown x)                     = A.Number x

instance A.FromJSON CompletionTriggerKind where
  parseJSON (A.Number 1) = pure CtInvoked
  parseJSON (A.Number 2) = pure CtTriggerCharacter
  parseJSON (A.Number 3) = pure CtTriggerForIncompleteCompletions
  parseJSON (A.Number x) = pure (CtUnknown x)
  parseJSON _            = fail "CompletionTriggerKind"

makeExtendingDatatype "CompletionOptions" [''WorkDoneProgressOptions]
  [ ("_triggerCharacters", [t| Maybe [Text] |])
  , ("_allCommitCharacters", [t| Maybe [Text] |])
  , ("_resolveProvider", [t| Maybe Bool|])
  ]
deriveJSON lspOptions ''CompletionOptions

makeExtendingDatatype "CompletionRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''CompletionOptions
  ]
  []
deriveJSON lspOptions ''CompletionRegistrationOptions

data CompletionContext =
  CompletionContext
    { _triggerKind      :: CompletionTriggerKind -- ^ How the completion was triggered.
    , _triggerCharacter :: Maybe Text
      -- ^ The trigger character (a single character) that has trigger code complete.
      -- Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
    }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''CompletionContext

makeExtendingDatatype "CompletionParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [ ("_context", [t| Maybe CompletionContext |]) ]
deriveJSON lspOptions ''CompletionParams

