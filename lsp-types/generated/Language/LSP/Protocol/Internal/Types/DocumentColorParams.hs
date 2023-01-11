-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentColorParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
Parameters for a `DocumentColorRequest`.

-}
data DocumentColorParams = DocumentColorParams 
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
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DocumentColorParams where
  toJSON (DocumentColorParams arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["textDocument" Aeson..= arg2]]

instance Aeson.FromJSON DocumentColorParams where
  parseJSON = Aeson.withObject "DocumentColorParams" $ \arg -> DocumentColorParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..:! "partialResultToken" <*> arg Aeson..: "textDocument"