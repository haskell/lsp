{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , closeVFS
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List
import qualified Data.Map as Map
import           Language.Haskell.LSP.Constant
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J
import           Language.Haskell.LSP.Utility
import qualified Yi.Rope as Yi

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

data VirtualFile =
  VirtualFile {
      _version :: Int
    , _text    :: Yi.YiString
    } deriving (Show)

type VFS = Map.Map J.Uri VirtualFile

-- ---------------------------------------------------------------------

-- getVfs :: forall a.(J.FromJSON a) => VFS -> String -> B.ByteString -> IO VFS
getVfs :: VFS -> String -> B.ByteString -> IO VFS
getVfs vfs cmd jsonStr = do
  -- TODO: this approach is horrible, as we have already deserialised the
  -- message by the time we get here. Need to sort out the types so the call
  -- works cleanly.
  -- Even just pass in the existing JSON Value, rather than the ByteString.
  case cmd of
    "textDocument/didOpen" -> do
      case J.eitherDecode jsonStr of
        Right (m::J.DidOpenTextDocumentNotification) -> openVFS vfs m
        Left _ -> do
          logs $ "haskell-lsp:getVfs:wrong type processing" ++ show cmd
          return vfs

    "textDocument/didChange" -> do
      case J.eitherDecode jsonStr of
        Right (m::J.DidChangeTextDocumentNotification) -> changeVFS vfs m
        Left _ -> do
          logs $ "haskell-lsp:getVfs:wrong type processing" ++ show cmd
          return vfs

    "textDocument/didClose" -> do
      case J.eitherDecode jsonStr of
        Right (m::J.DidCloseTextDocumentNotification) -> closeVFS vfs m
        Left _ -> do
          logs $ "haskell-lsp:getVfs:wrong type processing" ++ show cmd
          return vfs

    _ -> do
      logs $ "haskell-lsp:getVfs:not processing" ++ show cmd
      return vfs

-- ---------------------------------------------------------------------

openVFS :: VFS -> J.DidOpenTextDocumentNotification -> IO VFS
openVFS vfs (J.NotificationMessage _ _ Nothing) = return vfs
openVFS vfs (J.NotificationMessage _ _ (Just params)) = do
  let J.DidOpenTextDocumentNotificationParams
         (J.TextDocumentItem uri _ version text) = params
  return $ Map.insert uri (VirtualFile version (Yi.fromString text)) vfs

-- ---------------------------------------------------------------------

changeVFS :: VFS -> J.DidChangeTextDocumentNotification -> IO VFS
changeVFS vfs (J.NotificationMessage _ _ Nothing) = return vfs
changeVFS vfs (J.NotificationMessage _ _ (Just params)) = do
  let
    J.DidChangeTextDocumentParams vid (J.List changes) = params
    J.VersionedTextDocumentIdentifier uri version = vid
  case Map.lookup uri vfs of
    Just (VirtualFile _ str) -> do
      let str' = applyChanges str changes
      return $ Map.insert uri (VirtualFile version str') vfs
    Nothing -> do
      logs $ "haskell-lsp:changeVfs:can't find uri:" ++ show uri
      return vfs

-- ---------------------------------------------------------------------

closeVFS :: VFS -> J.DidCloseTextDocumentNotification -> IO VFS
closeVFS vfs (J.NotificationMessage _ _ Nothing) = return vfs
closeVFS vfs (J.NotificationMessage _ _ (Just params)) = do
  let J.DidCloseTextDocumentParams (J.TextDocumentIdentifier uri) = params
  return $ Map.delete uri vfs

-- ---------------------------------------------------------------------
{-

data TextDocumentContentChangeEvent =
  TextDocumentContentChangeEvent
    { _range       :: Maybe Range
    , _rangeLength :: Maybe Int
    , _text        :: String
    } deriving (Read,Show,Eq)
-}

-- | Apply the list of changes, in descending order of range. Assuming no overlaps.
applyChanges :: Yi.YiString -> [J.TextDocumentContentChangeEvent] -> Yi.YiString
applyChanges str changes' = r
  where
    r = undefined
    myComp (J.TextDocumentContentChangeEvent (Just r1) _ _)
           (J.TextDocumentContentChangeEvent (Just r2) _ _)
      = compare r1 r2
    myComp _ _ = EQ
    changes = sortBy myComp changes'
