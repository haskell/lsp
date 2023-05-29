-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.InlineValueContext
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
A parameter literal used in inline value requests.

@since 3.17.0
-}
data InlineValueParams = InlineValueParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The text document.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The document range for which inline values should be computed.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  Additional information about the context in which inline values were
  requested.
  -}
  _context :: Language.LSP.Protocol.Internal.Types.InlineValueContext.InlineValueContext
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON InlineValueParams where
  toJSON (InlineValueParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["textDocument" Aeson..= arg1]
    ,["range" Aeson..= arg2]
    ,["context" Aeson..= arg3]]

instance Aeson.FromJSON InlineValueParams where
  parseJSON = Aeson.withObject "InlineValueParams" $ \arg -> InlineValueParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..: "range" <*> arg Aeson..: "context"