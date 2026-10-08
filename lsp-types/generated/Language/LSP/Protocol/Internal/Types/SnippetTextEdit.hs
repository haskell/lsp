{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SnippetTextEdit where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.StringValue
import qualified Language.LSP.Protocol.Types.Common

{-|
An interactive text edit.

@since 3.18.0
-}
data SnippetTextEdit = SnippetTextEdit 
  { {-|
  The range of the text document to be manipulated.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The snippet to be inserted.
  -}
  _snippet :: Language.LSP.Protocol.Internal.Types.StringValue.StringValue
  , {-|
  The actual identifier of the snippet edit.
  -}
  _annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SnippetTextEdit)

instance Aeson.ToJSON SnippetTextEdit where
  toJSON (SnippetTextEdit arg0 arg1 arg2) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["snippet" Aeson..= arg1]
    ,"annotationId" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SnippetTextEdit where
  parseJSON = Aeson.withObject "SnippetTextEdit" $ \arg -> SnippetTextEdit <$> arg Aeson..: "range" <*> arg Aeson..: "snippet" <*> arg Language.LSP.Protocol.Types.Common..:!? "annotationId"
