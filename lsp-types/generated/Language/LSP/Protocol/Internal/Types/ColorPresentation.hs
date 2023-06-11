-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ColorPresentation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data ColorPresentation = ColorPresentation 
  { {-|
  The label of this color presentation. It will be shown on the color
  picker header. By default this is also the text that is inserted when selecting
  this color presentation.
  -}
  _label :: Data.Text.Text
  , {-|
  An `TextEdit` which is applied to a document when selecting
  this presentation for the color.  When `falsy` the `ColorPresentation.label`
  is used.
  -}
  _textEdit :: (Maybe Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit)
  , {-|
  An optional array of additional `TextEdit` that are applied when
  selecting this color presentation. Edits must not overlap with the main `ColorPresentation.textEdit` nor with themselves.
  -}
  _additionalTextEdits :: (Maybe [Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON ColorPresentation where
  toJSON (ColorPresentation arg0 arg1 arg2) = Aeson.object $ concat $  [["label" Aeson..= arg0]
    ,"textEdit" Language.LSP.Protocol.Types.Common..=? arg1
    ,"additionalTextEdits" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ColorPresentation where
  parseJSON = Aeson.withObject "ColorPresentation" $ \arg -> ColorPresentation <$> arg Aeson..: "label" <*> arg Aeson..:! "textEdit" <*> arg Aeson..:! "additionalTextEdits"
