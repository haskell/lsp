-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlayHint where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.InlayHintKind
import qualified Language.LSP.Protocol.Internal.Types.InlayHintLabelPart
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
Inlay hint information.

@since 3.17.0
-}
data InlayHint = InlayHint 
  { {-|
  The position of this hint.
  -}
  _position :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  The label of this hint. A human readable string or an array of
  InlayHintLabelPart label parts.

  *Note* that neither the string nor the label part can be empty.
  -}
  _label :: (Data.Text.Text Language.LSP.Protocol.Types.Common.|? [Language.LSP.Protocol.Internal.Types.InlayHintLabelPart.InlayHintLabelPart])
  , {-|
  The kind of this hint. Can be omitted in which case the client
  should fall back to a reasonable default.
  -}
  _kind :: (Maybe Language.LSP.Protocol.Internal.Types.InlayHintKind.InlayHintKind)
  , {-|
  Optional text edits that are performed when accepting this inlay hint.

  *Note* that edits are expected to change the document so that the inlay
  hint (or its nearest variant) is now part of the document and the inlay
  hint itself is now obsolete.
  -}
  _textEdits :: (Maybe [Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit])
  , {-|
  The tooltip text when you hover over this item.
  -}
  _tooltip :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent))
  , {-|
  Render padding before the hint.

  Note: Padding should use the editor's background color, not the
  background color of the hint itself. That means padding can be used
  to visually align/separate an inlay hint.
  -}
  _paddingLeft :: (Maybe Bool)
  , {-|
  Render padding after the hint.

  Note: Padding should use the editor's background color, not the
  background color of the hint itself. That means padding can be used
  to visually align/separate an inlay hint.
  -}
  _paddingRight :: (Maybe Bool)
  , {-|
  A data entry field that is preserved on an inlay hint between
  a `textDocument/inlayHint` and a `inlayHint/resolve` request.
  -}
  _data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON InlayHint where
  toJSON (InlayHint arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) = Aeson.object $ concat $  [["position" Aeson..= arg0]
    ,["label" Aeson..= arg1]
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg2
    ,"textEdits" Language.LSP.Protocol.Types.Common..=? arg3
    ,"tooltip" Language.LSP.Protocol.Types.Common..=? arg4
    ,"paddingLeft" Language.LSP.Protocol.Types.Common..=? arg5
    ,"paddingRight" Language.LSP.Protocol.Types.Common..=? arg6
    ,"data" Language.LSP.Protocol.Types.Common..=? arg7]

instance Aeson.FromJSON InlayHint where
  parseJSON = Aeson.withObject "InlayHint" $ \arg -> InlayHint <$> arg Aeson..: "position" <*> arg Aeson..: "label" <*> arg Aeson..:! "kind" <*> arg Aeson..:! "textEdits" <*> arg Aeson..:! "tooltip" <*> arg Aeson..:! "paddingLeft" <*> arg Aeson..:! "paddingRight" <*> arg Aeson..:! "data"
