{- ORMOLU_DISABLE -}
{- HLINT ignore -}
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
  applyEdit :: (Maybe Bool)
  , {-|
  Capabilities specific to `WorkspaceEdit`s.
  -}
  workspaceEdit :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities.WorkspaceEditClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/didChangeConfiguration` notification.
  -}
  didChangeConfiguration :: (Maybe Language.LSP.Protocol.Internal.Types.DidChangeConfigurationClientCapabilities.DidChangeConfigurationClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
  -}
  didChangeWatchedFiles :: (Maybe Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities.DidChangeWatchedFilesClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/symbol` request.
  -}
  symbol :: (Maybe Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities.WorkspaceSymbolClientCapabilities)
  , {-|
  Capabilities specific to the `workspace/executeCommand` request.
  -}
  executeCommand :: (Maybe Language.LSP.Protocol.Internal.Types.ExecuteCommandClientCapabilities.ExecuteCommandClientCapabilities)
  , {-|
  The client has support for workspace folders.

  @since 3.6.0
  -}
  workspaceFolders :: (Maybe Bool)
  , {-|
  The client supports `workspace/configuration` requests.

  @since 3.6.0
  -}
  configuration :: (Maybe Bool)
  , {-|
  Capabilities specific to the semantic token requests scoped to the
  workspace.

  @since 3.16.0.
  -}
  semanticTokens :: (Maybe Language.LSP.Protocol.Internal.Types.SemanticTokensWorkspaceClientCapabilities.SemanticTokensWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the code lens requests scoped to the
  workspace.

  @since 3.16.0.
  -}
  codeLens :: (Maybe Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities.CodeLensWorkspaceClientCapabilities)
  , {-|
  The client has support for file notifications/requests for user operations on files.

  Since 3.16.0
  -}
  fileOperations :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationClientCapabilities.FileOperationClientCapabilities)
  , {-|
  Capabilities specific to the inline values requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  inlineValue :: (Maybe Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities.InlineValueWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the inlay hint requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  inlayHint :: (Maybe Language.LSP.Protocol.Internal.Types.InlayHintWorkspaceClientCapabilities.InlayHintWorkspaceClientCapabilities)
  , {-|
  Capabilities specific to the diagnostic requests scoped to the
  workspace.

  @since 3.17.0.
  -}
  diagnostics :: (Maybe Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities.DiagnosticWorkspaceClientCapabilities)
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
  parseJSON = Aeson.withObject "WorkspaceClientCapabilities" $ \arg -> WorkspaceClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "applyEdit" <*> arg Language.LSP.Protocol.Types.Common..:!? "workspaceEdit" <*> arg Language.LSP.Protocol.Types.Common..:!? "didChangeConfiguration" <*> arg Language.LSP.Protocol.Types.Common..:!? "didChangeWatchedFiles" <*> arg Language.LSP.Protocol.Types.Common..:!? "symbol" <*> arg Language.LSP.Protocol.Types.Common..:!? "executeCommand" <*> arg Language.LSP.Protocol.Types.Common..:!? "workspaceFolders" <*> arg Language.LSP.Protocol.Types.Common..:!? "configuration" <*> arg Language.LSP.Protocol.Types.Common..:!? "semanticTokens" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeLens" <*> arg Language.LSP.Protocol.Types.Common..:!? "fileOperations" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlineValue" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlayHint" <*> arg Language.LSP.Protocol.Types.Common..:!? "diagnostics"
