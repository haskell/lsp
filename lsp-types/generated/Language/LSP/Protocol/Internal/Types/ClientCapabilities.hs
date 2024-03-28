{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
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
  workspace :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities.WorkspaceClientCapabilities)
  , {-|
  Text document specific client capabilities.
  -}
  textDocument :: (Maybe Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities.TextDocumentClientCapabilities)
  , {-|
  Capabilities specific to the notebook document support.

  @since 3.17.0
  -}
  notebookDocument :: (Maybe Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities.NotebookDocumentClientCapabilities)
  , {-|
  Window specific client capabilities.
  -}
  window :: (Maybe Language.LSP.Protocol.Internal.Types.WindowClientCapabilities.WindowClientCapabilities)
  , {-|
  General client capabilities.

  @since 3.16.0
  -}
  general :: (Maybe Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities.GeneralClientCapabilities)
  , {-|
  Experimental client capabilities.
  -}
  experimental :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCapabilities)

instance Aeson.ToJSON ClientCapabilities where
  toJSON (ClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  ["workspace" Language.LSP.Protocol.Types.Common..=? arg0
    ,"textDocument" Language.LSP.Protocol.Types.Common..=? arg1
    ,"notebookDocument" Language.LSP.Protocol.Types.Common..=? arg2
    ,"window" Language.LSP.Protocol.Types.Common..=? arg3
    ,"general" Language.LSP.Protocol.Types.Common..=? arg4
    ,"experimental" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON ClientCapabilities where
  parseJSON = Aeson.withObject "ClientCapabilities" $ \arg -> ClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "workspace" <*> arg Language.LSP.Protocol.Types.Common..:!? "textDocument" <*> arg Language.LSP.Protocol.Types.Common..:!? "notebookDocument" <*> arg Language.LSP.Protocol.Types.Common..:!? "window" <*> arg Language.LSP.Protocol.Types.Common..:!? "general" <*> arg Language.LSP.Protocol.Types.Common..:!? "experimental"
