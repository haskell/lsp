-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.FormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `DocumentRangeFormattingRequest`.
-}
data DocumentRangeFormattingParams = DocumentRangeFormattingParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The document to format.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The range to format
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The format options
  -}
  _options :: Language.LSP.Protocol.Internal.Types.FormattingOptions.FormattingOptions
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentRangeFormattingParams where
  toJSON (DocumentRangeFormattingParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["textDocument" Aeson..= arg1]
    ,["range" Aeson..= arg2]
    ,["options" Aeson..= arg3]]

instance Aeson.FromJSON DocumentRangeFormattingParams where
  parseJSON = Aeson.withObject "DocumentRangeFormattingParams" $ \arg -> DocumentRangeFormattingParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..: "range" <*> arg Aeson..: "options"
