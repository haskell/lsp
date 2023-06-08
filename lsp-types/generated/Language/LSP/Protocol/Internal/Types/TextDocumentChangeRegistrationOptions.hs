-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentChangeRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Describe options to be used when registered for text document change events.
-}
data TextDocumentChangeRegistrationOptions = TextDocumentChangeRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  How documents are synced to the server.
  -}
  _syncKind :: Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind.TextDocumentSyncKind
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TextDocumentChangeRegistrationOptions where
  toJSON (TextDocumentChangeRegistrationOptions arg0 arg1) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,["syncKind" Aeson..= arg1]]

instance Aeson.FromJSON TextDocumentChangeRegistrationOptions where
  parseJSON = Aeson.withObject "TextDocumentChangeRegistrationOptions" $ \arg -> TextDocumentChangeRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..: "syncKind"
