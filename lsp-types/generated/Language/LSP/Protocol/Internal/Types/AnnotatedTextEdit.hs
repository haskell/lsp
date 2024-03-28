{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.AnnotatedTextEdit where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A special text edit with an additional change annotation.

@since 3.16.0.
-}
data AnnotatedTextEdit = AnnotatedTextEdit 
  { {-|
  The range of the text document to be manipulated. To insert
  text into a document create a range where start === end.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The string to be inserted. For delete operations use an
  empty string.
  -}
  newText :: Data.Text.Text
  , {-|
  The actual identifier of the change annotation
  -}
  annotationId :: Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON AnnotatedTextEdit)

instance Aeson.ToJSON AnnotatedTextEdit where
  toJSON (AnnotatedTextEdit arg0 arg1 arg2) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["newText" Aeson..= arg1]
    ,["annotationId" Aeson..= arg2]]

instance Aeson.FromJSON AnnotatedTextEdit where
  parseJSON = Aeson.withObject "AnnotatedTextEdit" $ \arg -> AnnotatedTextEdit <$> arg Aeson..: "range" <*> arg Aeson..: "newText" <*> arg Aeson..: "annotationId"
