{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Manage the J.TextDocumentDidChange messages to keep a local copy of the files
in the client workspace, so that tools at the server can operate on them.
-}
module Language.Haskell.LSP.VFS
  (
    VFS
  , VirtualFile(..)
  , openVFS
  , changeFromClientVFS
  , changeFromServerVFS
  , closeVFS

  -- * for tests
  , applyChanges
  , applyChange
  , deleteChars , addChars
  , changeChars
  , yiSplitAt
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Text ( Text )
import           Data.List
import           Data.Ord
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Maybe
import qualified Language.Haskell.LSP.Types           as J
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

openVFS :: VFS -> J.DidOpenTextDocumentNotification -> IO VFS
openVFS vfs (J.NotificationMessage _ _ params) = do
  let J.DidOpenTextDocumentParams
         (J.TextDocumentItem uri _ version text) = params
  return $ Map.insert uri (VirtualFile version (Yi.fromText text)) vfs

-- ---------------------------------------------------------------------

changeFromClientVFS :: VFS -> J.DidChangeTextDocumentNotification -> IO VFS
changeFromClientVFS vfs (J.NotificationMessage _ _ params) = do
  let
    J.DidChangeTextDocumentParams vid (J.List changes) = params
    J.VersionedTextDocumentIdentifier uri version = vid
  case Map.lookup uri vfs of
    Just (VirtualFile _ str) -> do
      let str' = applyChanges str changes
      -- the client shouldn't be sending over a null version, only the server.
      return $ Map.insert uri (VirtualFile (fromMaybe 0 version) str') vfs
    Nothing -> do
      logs $ "haskell-lsp:changeVfs:can't find uri:" ++ show uri
      return vfs

-- ---------------------------------------------------------------------

changeFromServerVFS :: VFS -> J.ApplyWorkspaceEditRequest -> IO VFS
changeFromServerVFS initVfs (J.RequestMessage _ _ _ params) = do
  let J.ApplyWorkspaceEditParams edit = params
      J.WorkspaceEdit mChanges mDocChanges = edit
  case mDocChanges of
    Just (J.List textDocEdits) -> applyEdits textDocEdits
    Nothing -> case mChanges of
      Just cs -> applyEdits $ HashMap.foldlWithKey' changeToTextDocumentEdit [] cs
      Nothing -> do
        logs "haskell-lsp:changeVfs:no changes"
        return initVfs

  where

    changeToTextDocumentEdit acc uri edits =
      acc ++ [J.TextDocumentEdit (J.VersionedTextDocumentIdentifier uri (Just 0)) edits]

    applyEdits = foldM f initVfs . sortOn (^. J.textDocument . J.version)

    f vfs (J.TextDocumentEdit vid (J.List edits)) = do
      -- all edits are supposed to be applied at once
      -- so apply from bottom up so they don't affect others
      let sortedEdits = sortOn (Down . (^. J.range)) edits
          changeEvents = map editToChangeEvent sortedEdits
          ps = J.DidChangeTextDocumentParams vid (J.List changeEvents)
          notif = J.NotificationMessage "" J.TextDocumentDidChange ps
      changeFromClientVFS vfs notif
  
    editToChangeEvent (J.TextEdit range text) = J.TextDocumentContentChangeEvent (Just range) Nothing text

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

-- | Apply the list of changes.
-- Changes should be applied in the order that they are
-- received from the client.
applyChanges :: Yi.YiString -> [J.TextDocumentContentChangeEvent] -> Yi.YiString
applyChanges = foldl' applyChange

-- ---------------------------------------------------------------------

applyChange :: Yi.YiString -> J.TextDocumentContentChangeEvent -> Yi.YiString
applyChange _ (J.TextDocumentContentChangeEvent Nothing Nothing str)
  = Yi.fromText str
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range fm _to)) (Just len) txt) =
  if txt == ""
    then -- delete len chars from fm
      deleteChars str fm len
    else -- add or change, based on length
      if len == 0
        then addChars str fm txt
             -- Note: changeChars comes from applyEdit, emacs will split it into a
             -- delete and an add
        else changeChars str fm len txt
applyChange str (J.TextDocumentContentChangeEvent (Just r@(J.Range (J.Position sl sc) (J.Position el ec))) Nothing txt)
  = applyChange str (J.TextDocumentContentChangeEvent (Just r) (Just len) txt)
    where len = Yi.length region
          (beforeEnd, afterEnd) = Yi.splitAtLine el str
          lastLine = Yi.take ec afterEnd
          lastLine' | sl == el = Yi.drop sc lastLine
                    | otherwise = lastLine
          (_beforeStart, afterStartBeforeEnd) = Yi.splitAtLine sl beforeEnd
          region = Yi.drop sc afterStartBeforeEnd <> lastLine'
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

addChars :: Yi.YiString -> J.Position -> Text -> Yi.YiString
addChars str (J.Position l c) new = str'
  where
    (before,after) = Yi.splitAtLine l str
    -- after contains the area we care about, starting with the selected line.
    -- Due to LSP zero-based coordinates
    beforeOnLine = Yi.take c after
    after' = Yi.drop c after
    str' = Yi.concat [before, beforeOnLine, (Yi.fromText new), after']

-- ---------------------------------------------------------------------

changeChars :: Yi.YiString -> J.Position -> Int -> Text -> Yi.YiString
changeChars str (J.Position ls cs) len new = str'
  where
    (before,after) = yiSplitAt ls cs str
    after' = Yi.drop len after

    str' = Yi.concat [before, (Yi.fromText new), after']

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
