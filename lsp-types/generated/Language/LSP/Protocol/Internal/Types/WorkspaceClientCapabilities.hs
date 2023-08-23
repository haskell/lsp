-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlayHintWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensWorkspaceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|
Workspace specific client capabilities.
-}
data WorkspaceClientCapabilities = WorkspaceClientCapabilities 
  { {-|
  The client supports applying batch edits
  to the workspace by supporting the request
  'workspace/applyEdit'
  -}
  _applyEdit :: (Maybe Bool)
  , {-|
  Capabilities specific to `WorkspaceEdit`s.
  -}
  _workspaceEdit :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities.WorkspaceEditClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/didChangeConfiguration` notification.
  -}
  _didChangeConfiguration :: (Maybe Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities.DidChangeConfigurationClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
  -}
  _didChangeWatchedFiles :: (Maybe Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities.DidChangeWatchedFilesClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/symbol` request.
  -}
  _symbol :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities.WorkspaceSymbolClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/executeCommand` request.
  -}
  _executeCommand :: (Maybe Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities.ExecuteCommandClientCapabilities)
  , {-|
  The client has support for workspace folders.

  @since 3.6.0
  -}
  _workspaceFolders :: (Maybe Bool)
  , {-|
  The client supports `workspace/configuration` requests.

  @since 3.6.0
  -}
  _configuration :: (Maybe Bool)
  , {-|
  Capabilities specific to the semantic token requests scoped to the
  workspace.

  @since 3.16.0.
  -}
  _semanticTokens :: (Maybe Language.LSP.Protocol.Internal.Types.SemanticTokensWorkspaceClientCapabilities.SemanticTokensWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the code lens requests scoped to the
  workspace.

  @since 3.16.0.
  -}
  _codeLens :: (Maybe Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities.CodeLensWorkspaceClientCapabilities)
  , {-|
  The client has support for file notifications/requests for user operations on files.

  Since 3.16.0
  -}
  _fileOperations :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities.FileOperationClientCapabilities)
  , {-|
  Capabilities specific to the inline values requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  _inlineValue :: (Maybe Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities.InlineValueWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the inlay hint requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  _inlayHint :: (Maybe Language.LSP.Protocol.Internal.Types.InlayHintWorkspaceClientCapabilities.InlayHintWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the diagnostic requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  _diagnostics :: (Maybe Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities.DiagnosticWorkspaceClientCapabilities)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceClientCapabilities)

instance Aeson.ToJSON WorkspaceClientCapabilities where
  toJSON (WorkspaceClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) = Aeson.object $ concat $  ["applyEdit" Language.LSP.Protocol.Types.Common..=? arg0
    ,"workspaceEdit" Language.LSP.Protocol.Types.Common..=? arg1
    ,"didChangeConfiguration" Language.LSP.Protocol.Types.Common..=? arg2
    ,"didChangeWatchedFiles" Language.LSP.Protocol.Types.Common..=? arg3
    ,"symbol" Language.LSP.Protocol.Types.Common..=? arg4
    ,"executeCommand" Language.LSP.Protocol.Types.Common..=? arg5
    ,"workspaceFolders" Language.LSP.Protocol.Types.Common..=? arg6
    ,"configuration" Language.LSP.Protocol.Types.Common..=? arg7
    ,"semanticTokens" Language.LSP.Protocol.Types.Common..=? arg8
    ,"codeLens" Language.LSP.Protocol.Types.Common..=? arg9
    ,"fileOperations" Language.LSP.Protocol.Types.Common..=? arg10
    ,"inlineValue" Language.LSP.Protocol.Types.Common..=? arg11
    ,"inlayHint" Language.LSP.Protocol.Types.Common..=? arg12
    ,"diagnostics" Language.LSP.Protocol.Types.Common..=? arg13]

instance Aeson.FromJSON WorkspaceClientCapabilities where
  parseJSON = Aeson.withObject "WorkspaceClientCapabilities" $ \arg -> WorkspaceClientCapabilities <$> arg Aeson..:! "applyEdit" <*> arg Aeson..:! "workspaceEdit" <*> arg Aeson..:! "didChangeConfiguration" <*> arg Aeson..:! "didChangeWatchedFiles" <*> arg Aeson..:! "symbol" <*> arg Aeson..:! "executeCommand" <*> arg Aeson..:! "workspaceFolders" <*> arg Aeson..:! "configuration" <*> arg Aeson..:! "semanticTokens" <*> arg Aeson..:! "codeLens" <*> arg Aeson..:! "fileOperations" <*> arg Aeson..:! "inlineValue" <*> arg Aeson..:! "inlayHint" <*> arg Aeson..:! "diagnostics"
