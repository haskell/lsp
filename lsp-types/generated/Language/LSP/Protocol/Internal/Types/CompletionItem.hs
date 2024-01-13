{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItem where

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
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemKind
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemLabelDetails
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemTag
import qualified Language.LSP.Protocol.Internal.Types.InsertReplaceEdit
import qualified Language.LSP.Protocol.Internal.Types.InsertTextFormat
import qualified Language.LSP.Protocol.Internal.Types.InsertTextMode
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
A completion item represents a text snippet that is
proposed to complete text that is being typed.
-}
data CompletionItem = CompletionItem 
  { {-|
  The label of this completion item.

  The label property is also by default the text that
  is inserted when selecting this completion.

  If label details are provided the label itself should
  be an unqualified name of the completion item.
  -}
  _label :: Data.Text.Text
  , {-|
  Additional details for the label

  @since 3.17.0
  -}
  _labelDetails :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionItemLabelDetails.CompletionItemLabelDetails)
  , {-|
  The kind of this completion item. Based of the kind
  an icon is chosen by the editor.
  -}
  _kind :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionItemKind.CompletionItemKind)
  , {-|
  Tags for this completion item.

  @since 3.15.0
  -}
  _tags :: (Maybe [Language.LSP.Protocol.Internal.Types.CompletionItemTag.CompletionItemTag])
  , {-|
  A human-readable string with additional information
  about this item, like type or symbol information.
  -}
  _detail :: (Maybe Data.Text.Text)
  , {-|
  A human-readable string that represents a doc-comment.
  -}
  _documentation :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent))
  , {-|
  Indicates if this item is deprecated.
  @deprecated Use `tags` instead.
  -}
  _deprecated :: (Maybe Bool)
  , {-|
  Select this item when showing.

  *Note* that only one completion item can be selected and that the
  tool / client decides which item that is. The rule is that the *first*
  item of those that match best is selected.
  -}
  _preselect :: (Maybe Bool)
  , {-|
  A string that should be used when comparing this item
  with other items. When `falsy` the `CompletionItem.label`
  is used.
  -}
  _sortText :: (Maybe Data.Text.Text)
  , {-|
  A string that should be used when filtering a set of
  completion items. When `falsy` the `CompletionItem.label`
  is used.
  -}
  _filterText :: (Maybe Data.Text.Text)
  , {-|
  A string that should be inserted into a document when selecting
  this completion. When `falsy` the `CompletionItem.label`
  is used.

  The `insertText` is subject to interpretation by the client side.
  Some tools might not take the string literally. For example
  VS Code when code complete is requested in this example
  `con<cursor position>` and a completion item with an `insertText` of
  `console` is provided it will only insert `sole`. Therefore it is
  recommended to use `textEdit` instead since it avoids additional client
  side interpretation.
  -}
  _insertText :: (Maybe Data.Text.Text)
  , {-|
  The format of the insert text. The format applies to both the
  `insertText` property and the `newText` property of a provided
  `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.

  Please note that the insertTextFormat doesn't apply to
  `additionalTextEdits`.
  -}
  _insertTextFormat :: (Maybe Language.LSP.Protocol.Internal.Types.InsertTextFormat.InsertTextFormat)
  , {-|
  How whitespace and indentation is handled during completion
  item insertion. If not provided the clients default value depends on
  the `textDocument.completion.insertTextMode` client capability.

  @since 3.16.0
  -}
  _insertTextMode :: (Maybe Language.LSP.Protocol.Internal.Types.InsertTextMode.InsertTextMode)
  , {-|
  An `TextEdit` which is applied to a document when selecting
  this completion. When an edit is provided the value of
  `CompletionItem.insertText` is ignored.

  Most editors support two different operations when accepting a completion
  item. One is to insert a completion text and the other is to replace an
  existing text with a completion text. Since this can usually not be
  predetermined by a server it can report both ranges. Clients need to
  signal support for `InsertReplaceEdits` via the
  `textDocument.completion.insertReplaceSupport` client capability
  property.

  *Note 1:* The text edit's range as well as both ranges from an insert
  replace edit must be a [single line] and they must contain the position
  at which completion has been requested.
  *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
  must be a prefix of the edit's replace range, that means it must be
  contained and starting at the same position.

  @since 3.16.0 additional type `InsertReplaceEdit`
  -}
  _textEdit :: (Maybe (Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.InsertReplaceEdit.InsertReplaceEdit))
  , {-|
  The edit text used if the completion item is part of a CompletionList and
  CompletionList defines an item default for the text edit range.

  Clients will only honor this property if they opt into completion list
  item defaults using the capability `completionList.itemDefaults`.

  If not provided and a list's default range is provided the label
  property is used as a text.

  @since 3.17.0
  -}
  _textEditText :: (Maybe Data.Text.Text)
  , {-|
  An optional array of additional `TextEdit` that are applied when
  selecting this completion. Edits must not overlap (including the same insert position)
  with the main `CompletionItem.textEdit` nor with themselves.

  Additional text edits should be used to change text unrelated to the current cursor position
  (for example adding an import statement at the top of the file if the completion item will
  insert an unqualified type).
  -}
  _additionalTextEdits :: (Maybe [Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit])
  , {-|
  An optional set of characters that when pressed while this completion is active will accept it first and
  then type that character. *Note* that all commit characters should have `length=1` and that superfluous
  characters will be ignored.
  -}
  _commitCharacters :: (Maybe [Data.Text.Text])
  , {-|
  An optional `Command` that is executed *after* inserting this completion. *Note* that
  additional modifications to the current document should be described with the
  `CompletionItem.additionalTextEdits`-property.
  -}
  _command :: (Maybe Language.LSP.Protocol.Internal.Types.Command.Command)
  , {-|
  A data entry field that is preserved on a completion item between a
  `CompletionRequest`.
  -}
  _data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CompletionItem)

instance Aeson.ToJSON CompletionItem where
  toJSON (CompletionItem arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18) = Aeson.object $ concat $  [["label" Aeson..= arg0]
    ,"labelDetails" Language.LSP.Protocol.Types.Common..=? arg1
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg2
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg3
    ,"detail" Language.LSP.Protocol.Types.Common..=? arg4
    ,"documentation" Language.LSP.Protocol.Types.Common..=? arg5
    ,"deprecated" Language.LSP.Protocol.Types.Common..=? arg6
    ,"preselect" Language.LSP.Protocol.Types.Common..=? arg7
    ,"sortText" Language.LSP.Protocol.Types.Common..=? arg8
    ,"filterText" Language.LSP.Protocol.Types.Common..=? arg9
    ,"insertText" Language.LSP.Protocol.Types.Common..=? arg10
    ,"insertTextFormat" Language.LSP.Protocol.Types.Common..=? arg11
    ,"insertTextMode" Language.LSP.Protocol.Types.Common..=? arg12
    ,"textEdit" Language.LSP.Protocol.Types.Common..=? arg13
    ,"textEditText" Language.LSP.Protocol.Types.Common..=? arg14
    ,"additionalTextEdits" Language.LSP.Protocol.Types.Common..=? arg15
    ,"commitCharacters" Language.LSP.Protocol.Types.Common..=? arg16
    ,"command" Language.LSP.Protocol.Types.Common..=? arg17
    ,"data" Language.LSP.Protocol.Types.Common..=? arg18]

instance Aeson.FromJSON CompletionItem where
  parseJSON = Aeson.withObject "CompletionItem" $ \arg -> CompletionItem <$> arg Aeson..: "label" <*> arg Language.LSP.Protocol.Types.Common..:!? "labelDetails" <*> arg Language.LSP.Protocol.Types.Common..:!? "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "tags" <*> arg Language.LSP.Protocol.Types.Common..:!? "detail" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentation" <*> arg Language.LSP.Protocol.Types.Common..:!? "deprecated" <*> arg Language.LSP.Protocol.Types.Common..:!? "preselect" <*> arg Language.LSP.Protocol.Types.Common..:!? "sortText" <*> arg Language.LSP.Protocol.Types.Common..:!? "filterText" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertText" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextFormat" <*> arg Language.LSP.Protocol.Types.Common..:!? "insertTextMode" <*> arg Language.LSP.Protocol.Types.Common..:!? "textEdit" <*> arg Language.LSP.Protocol.Types.Common..:!? "textEditText" <*> arg Language.LSP.Protocol.Types.Common..:!? "additionalTextEdits" <*> arg Language.LSP.Protocol.Types.Common..:!? "commitCharacters" <*> arg Language.LSP.Protocol.Types.Common..:!? "command" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
