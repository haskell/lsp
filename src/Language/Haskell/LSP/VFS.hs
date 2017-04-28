{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

{-

Manage the "textDocument/didChange" messages to keep a local copy of the files
in the client workspace, so that tools at the server can operate on them.
-}
module Language.Haskell.LSP.VFS
  (
    VFS
  , VirtualFile(..)
  , getVfs
  , openVFS
  , changeVFS
  , saveVFS
  , closeVFS
  ) where

import qualified Data.Aeson as J
import qualified Data.Map as Map
import           Language.Haskell.LSP.Constant
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J
import           Language.Haskell.LSP.Utility
import qualified Yi.Rope as Yi

data VirtualFile =
  VirtualFile {
      _version :: Int
    , _text    :: Yi.YiString
    } deriving (Show)

type VFS = Map.Map J.Uri VirtualFile

-- ---------------------------------------------------------------------

getVfs :: forall a.(J.FromJSON a) => VFS -> a -> IO (VirtualFile, VFS)
getVfs = undefined

-- ---------------------------------------------------------------------

openVFS :: VFS -> J.DidOpenTextDocumentNotification -> IO VFS
openVFS vfs (J.NotificationMessage _ _ Nothing) = return vfs
openVFS vfs (J.NotificationMessage _ _ (Just params)) = do
  let J.DidOpenTextDocumentNotificationParams
         (J.TextDocumentItem uri _ version text) = params
  return $ Map.insert uri (VirtualFile version (Yi.fromString text)) vfs

-- ---------------------------------------------------------------------

changeVFS :: VFS -> J.DidChangeTextDocumentNotification -> IO VFS
changeVFS vfs notification = do
  return vfs

-- ---------------------------------------------------------------------

saveVFS :: VFS -> J.DidSaveTextDocumentNotification -> IO VFS
saveVFS vfs notification = do
  return vfs

-- ---------------------------------------------------------------------

closeVFS :: VFS -> J.DidCloseTextDocumentNotification -> IO VFS
closeVFS vfs notification = do
  return vfs

-- ---------------------------------------------------------------------
