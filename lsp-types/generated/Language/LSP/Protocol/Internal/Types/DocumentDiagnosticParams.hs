-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentDiagnosticParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
Parameters of the document diagnostic request.

@since 3.17.0

-}
data DocumentDiagnosticParams = DocumentDiagnosticParams 
  { {-|
  An optional token that a server can use to report work done progress.

  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.

  -}
  _partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The text document.

  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The additional identifier  provided during registration.

  -}
  _identifier :: (Maybe Data.Text.Text)
  , {-|
  The result id of a previous response if provided.

  -}
  _previousResultId :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentDiagnosticParams where
  toJSON (DocumentDiagnosticParams arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["textDocument" Aeson..= arg2]
    ,"identifier" Language.LSP.Protocol.Types.Common..=? arg3
    ,"previousResultId" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON DocumentDiagnosticParams where
  parseJSON = Aeson.withObject "DocumentDiagnosticParams" $ \arg -> DocumentDiagnosticParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..:! "partialResultToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..:! "identifier" <*> arg Aeson..:! "previousResultId"