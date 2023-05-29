-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextEdit where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A text edit applicable to a text document.

-}
data TextEdit = TextEdit 
  { {-|
  The range of the text document to be manipulated. To insert
  text into a document create a range where start === end.

  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The string to be inserted. For delete operations use an
  empty string.

  -}
  _newText :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TextEdit where
  toJSON (TextEdit arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["newText" Aeson..= arg1]]

instance Aeson.FromJSON TextEdit where
  parseJSON = Aeson.withObject "TextEdit" $ \arg -> TextEdit <$> arg Aeson..: "range" <*> arg Aeson..: "newText"