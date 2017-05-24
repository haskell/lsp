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

  -- * for tests
  , sortChanges
  , deleteChars
  , addChars
  , changeChars
  , yiSplitAt
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List
import qualified Data.Map as Map
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
      -- logs $ "haskell-lsp:getVfs:not processing" ++ show cmd
      return vfs

-- ---------------------------------------------------------------------

openVFS :: VFS -> J.DidOpenTextDocumentNotification -> IO VFS
openVFS vfs (J.NotificationMessage _ _ params) = do
  let J.DidOpenTextDocumentParams
         (J.TextDocumentItem uri _ version text) = params
  return $ Map.insert uri (VirtualFile version (Yi.fromString text)) vfs

-- ---------------------------------------------------------------------

changeVFS :: VFS -> J.DidChangeTextDocumentNotification -> IO VFS
changeVFS vfs (J.NotificationMessage _ _ params) = do
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
closeVFS vfs (J.NotificationMessage _ _ params) = do
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
    changes = sortChanges changes'
    r = foldl' applyChange str changes

-- ---------------------------------------------------------------------

applyChange :: Yi.YiString -> J.TextDocumentContentChangeEvent -> Yi.YiString
applyChange _ (J.TextDocumentContentChangeEvent Nothing Nothing str)
  = Yi.fromString str
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range fm _to)) (Just len) txt) =
  if txt == ""
    then -- delete len chars from fm
      deleteChars str fm len
    else -- add or change, based on length
      if len == 0
        then addChars str fm txt
             -- Note: changChars comes from applyEdit, emacs will split it into a
             -- delete and an add
        else changeChars str fm len txt
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range _fm _to)) Nothing _txt)
  -- TODO: This case may occur in the wild, need to convert to-fm into a length.
  -- Or encode a specific addChar function
  = str
applyChange str (J.TextDocumentContentChangeEvent Nothing (Just _) _txt)
  = str

-- ---------------------------------------------------------------------

deleteChars :: Yi.YiString -> J.Position -> Int -> Yi.YiString
deleteChars str (J.Position l c) len = str'
  where
    (before,after) = Yi.splitAtLine l str
    -- after contains the area we care about, starting with the selected line.
    -- Due to LSP zero-based coordinates
    beforeOnLine = Yi.take c after
    after' = Yi.drop (c + len) after
    str' = Yi.append before (Yi.append beforeOnLine after')

-- ---------------------------------------------------------------------

addChars :: Yi.YiString -> J.Position -> String -> Yi.YiString
addChars str (J.Position l c) new = str'
  where
    (before,after) = Yi.splitAtLine l str
    -- after contains the area we care about, starting with the selected line.
    -- Due to LSP zero-based coordinates
    beforeOnLine = Yi.take c after
    after' = Yi.drop c after
    str' = Yi.concat [before, beforeOnLine, (Yi.fromString new), after']

-- ---------------------------------------------------------------------

changeChars :: Yi.YiString -> J.Position -> Int -> String -> Yi.YiString
changeChars str (J.Position ls cs) len new = str'
  where
    (before,after) = yiSplitAt ls cs str
    after' = Yi.drop len after

    str' = Yi.concat [before, (Yi.fromString new), after']

-- changeChars :: Yi.YiString -> J.Position -> J.Position -> String -> Yi.YiString
-- changeChars str (J.Position ls cs) (J.Position le ce) new = str'
--   where
--     (before,_after) = yiSplitAt ls cs str
--     (_before,after) = yiSplitAt le ce str

--     str' = Yi.concat [before, (Yi.fromString new), after]
--     -- str' = Yi.concat [before]
--     -- str' = Yi.concat [_before]

-- ---------------------------------------------------------------------

yiSplitAt :: Int -> Int -> Yi.YiString -> (Yi.YiString, Yi.YiString)
yiSplitAt l c str = (before,after)
  where
    (b,a) = Yi.splitAtLine l str
    before = Yi.concat [b,Yi.take c a]
    after = Yi.drop c a


-- ---------------------------------------------------------------------

sortChanges :: [J.TextDocumentContentChangeEvent] -> [J.TextDocumentContentChangeEvent]
sortChanges changes = changes'
  where
    myComp (J.TextDocumentContentChangeEvent (Just r1) _ _)
           (J.TextDocumentContentChangeEvent (Just r2) _ _)
      = compare r2 r1 -- want descending order
    myComp _ _ = EQ
    changes' = sortBy myComp changes

-- ---------------------------------------------------------------------
