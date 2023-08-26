{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.WorkspaceFolders
  ( WorkspaceFoldersHandle
  , new
  , workspaceFoldersHandlers
  , getWorkspaceFolders
  , fromInitializeParams
  ) where

import Control.Concurrent.STM
import Control.Lens
import Data.List (delete)
import JSONRPC.Typed.Method
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server.Core

new :: Maybe [LSP.WorkspaceFolder] -> IO WorkspaceFoldersHandle
new initialWfs = do
  workspaceFolders <- newTVarIO initialWfs
  pure $ WorkspaceFoldersHandle{..}

fromInitializeParams :: LSP.InitializeParams -> Maybe [LSP.WorkspaceFolder]
fromInitializeParams p =
  case p ^. LSP.workspaceFolders of
    Just (LSP.InL xs) -> Just xs
    Just (LSP.InR _) -> Just []
    Nothing -> Nothing

workspaceFoldersHandlers :: WorkspaceFoldersHandle -> Handlers Server LSP.Method
workspaceFoldersHandlers handle =
  notificationHandler LSP.SMethod_WorkspaceDidChangeWorkspaceFolders $
    mkNotificationHandler $
      updateWorkspaceFolders handle

updateWorkspaceFolders :: WorkspaceFoldersHandle -> MethodParams LSP.Method_WorkspaceDidChangeWorkspaceFolders -> IO ()
updateWorkspaceFolders handle params = do
  let toRemove = params ^. LSP.event . LSP.removed
      toAdd = params ^. LSP.event . LSP.added
      newWfs (Just oldWfs) = Just $ foldr delete oldWfs toRemove <> toAdd
      newWfs Nothing = Just toAdd
  atomically $ modifyTVar handle.workspaceFolders newWfs

-- | The current workspace folders, if the client supports workspace folders.
getWorkspaceFolders :: WorkspaceFoldersHandle -> IO (Maybe [LSP.WorkspaceFolder])
getWorkspaceFolders handle = readTVarIO handle.workspaceFolders

-- TODO: provide workspace/workspaceFolders call?
