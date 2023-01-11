-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCapabilities where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WindowClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|
Defines the capabilities provided by the client.

-}
data ClientCapabilities = ClientCapabilities 
  { {-|
  Workspace specific client capabilities.

  -}
  _workspace :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities.WorkspaceClientCapabilities)
  , {-|
  Text document specific client capabilities.

  -}
  _textDocument :: (Maybe Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities.TextDocumentClientCapabilities)
  , {-|
  Capabilities specific to the notebook document support.

  @since 3.17.0

  -}
  _notebookDocument :: (Maybe Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities.NotebookDocumentClientCapabilities)
  , {-|
  Window specific client capabilities.

  -}
  _window :: (Maybe Language.LSP.Protocol.Internal.Types.WindowClientCapabilities.WindowClientCapabilities)
  , {-|
  General client capabilities.

  @since 3.16.0

  -}
  _general :: (Maybe Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities.GeneralClientCapabilities)
  , {-|
  Experimental client capabilities.

  -}
  _experimental :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ClientCapabilities where
  toJSON (ClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  ["workspace" Language.LSP.Protocol.Types.Common..=? arg0
    ,"textDocument" Language.LSP.Protocol.Types.Common..=? arg1
    ,"notebookDocument" Language.LSP.Protocol.Types.Common..=? arg2
    ,"window" Language.LSP.Protocol.Types.Common..=? arg3
    ,"general" Language.LSP.Protocol.Types.Common..=? arg4
    ,"experimental" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON ClientCapabilities where
  parseJSON = Aeson.withObject "ClientCapabilities" $ \arg -> ClientCapabilities <$> arg Aeson..:! "workspace" <*> arg Aeson..:! "textDocument" <*> arg Aeson..:! "notebookDocument" <*> arg Aeson..:! "window" <*> arg Aeson..:! "general" <*> arg Aeson..:! "experimental"