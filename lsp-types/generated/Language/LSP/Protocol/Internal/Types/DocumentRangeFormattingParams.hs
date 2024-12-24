{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
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
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The document to format.
  -}
  textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The range to format
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The format options
  -}
  options :: Language.LSP.Protocol.Internal.Types.FormattingOptions.FormattingOptions
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentRangeFormattingParams)

instance Aeson.ToJSON DocumentRangeFormattingParams where
  toJSON (DocumentRangeFormattingParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["textDocument" Aeson..= arg1]
    ,["range" Aeson..= arg2]
    ,["options" Aeson..= arg3]]

instance Aeson.FromJSON DocumentRangeFormattingParams where
  parseJSON = Aeson.withObject "DocumentRangeFormattingParams" $ \arg -> DocumentRangeFormattingParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..: "range" <*> arg Aeson..: "options"
