{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Handles the "Language.Haskell.LSP.Types.TextDocumentDidChange" \/
"Language.Haskell.LSP.Types.TextDocumentDidOpen" \/
"Language.Haskell.LSP.Types.TextDocumentDidClose" messages to keep an in-memory
`filesystem` of the current client workspace.  The server can access and edit
files in the client workspace by operating on the "VFS" in "LspFuncs".
-}
module Language.Haskell.LSP.VFS
  (
    VFS(..)
  , VirtualFile(..)
  , initVFS
  , openVFS
  , changeFromClientVFS
  , changeFromServerVFS
  , persistFileVFS
  , closeVFS
  , updateVFS

  -- * manipulating the file contents
  , rangeLinesFromVfs
  , PosPrefixInfo(..)
  , getCompletionPrefix

  -- * for tests
  , applyChanges
  , applyChange
  , changeChars
  ) where

import           Control.Lens hiding ( parts )
import           Control.Monad
import           Data.Char (isUpper, isAlphaNum)
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.List
import           Data.Ord
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Rope.UTF16 ( Rope )
import qualified Data.Rope.UTF16 as Rope
import qualified Language.Haskell.LSP.Types           as J
import qualified Language.Haskell.LSP.Types.Lens      as J
import           Language.Haskell.LSP.Utility
import           System.FilePath
import           Data.Hashable
import           System.Directory
import           System.IO.Temp

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

data VirtualFile =
  VirtualFile {
      _version :: Int
    , _text    :: Rope
    } deriving (Show)

type VFSMap = Map.Map J.NormalizedUri VirtualFile

data VFS = VFS { vfsMap :: Map.Map J.NormalizedUri VirtualFile
               , vfsTempDir :: FilePath -- ^ This is where all the temporary files will be written to
               } deriving Show

---

initVFS :: (VFS -> IO r) -> IO r
initVFS k = withSystemTempDirectory "haskell-lsp" $ \temp_dir -> k (VFS mempty temp_dir)

-- ---------------------------------------------------------------------

openVFS :: VFS -> J.DidOpenTextDocumentNotification -> (VFS, [String])
openVFS vfs (J.NotificationMessage _ _ params) =
  let J.DidOpenTextDocumentParams
         (J.TextDocumentItem uri _ version text) = params
  in (updateVFS (Map.insert (J.toNormalizedUri uri) (VirtualFile version (Rope.fromText text))) vfs
     , [])


-- ---------------------------------------------------------------------

changeFromClientVFS :: VFS -> J.DidChangeTextDocumentNotification -> (VFS,[String])
changeFromClientVFS vfs (J.NotificationMessage _ _ params) =
  let
    J.DidChangeTextDocumentParams vid (J.List changes) = params
    J.VersionedTextDocumentIdentifier (J.toNormalizedUri -> uri) version = vid
  in
    case Map.lookup uri (vfsMap vfs) of
      Just (VirtualFile _ str) ->
        let str' = applyChanges str changes
        -- the client shouldn't be sending over a null version, only the server.
        in (updateVFS (Map.insert uri (VirtualFile (fromMaybe 0 version) str')) vfs, [])
      Nothing ->
        -- logs $ "haskell-lsp:changeVfs:can't find uri:" ++ show uri
        -- return vfs
        (vfs, ["haskell-lsp:changeVfs:can't find uri:" ++ show uri])

updateVFS :: (VFSMap -> VFSMap) -> VFS -> VFS
updateVFS f vfs@VFS{vfsMap} = vfs { vfsMap = f vfsMap }

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

    -- applyEdits :: [J.TextDocumentEdit] -> VFS
    applyEdits :: [J.TextDocumentEdit] -> IO VFS
    applyEdits = foldM f initVfs . sortOn (^. J.textDocument . J.version)

    f :: VFS -> J.TextDocumentEdit -> IO VFS
    f vfs (J.TextDocumentEdit vid (J.List edits)) = do
      -- all edits are supposed to be applied at once
      -- so apply from bottom up so they don't affect others
      let sortedEdits = sortOn (Down . (^. J.range)) edits
          changeEvents = map editToChangeEvent sortedEdits
          ps = J.DidChangeTextDocumentParams vid (J.List changeEvents)
          notif = J.NotificationMessage "" J.TextDocumentDidChange ps
      let (vfs',ls) = changeFromClientVFS vfs notif
      mapM_ logs ls
      return vfs'

    editToChangeEvent (J.TextEdit range text) = J.TextDocumentContentChangeEvent (Just range) Nothing text

-- ---------------------------------------------------------------------
virtualFileName :: FilePath -> J.NormalizedUri -> VirtualFile -> FilePath
virtualFileName prefix uri (VirtualFile ver _) =
  prefix </> show (hash (J.fromNormalizedUri uri)) ++ "-" ++ show ver ++ ".hs"

persistFileVFS :: VFS -> J.NormalizedUri -> (FilePath, IO ())
persistFileVFS vfs uri =
  case Map.lookup uri (vfsMap vfs) of
    Nothing -> error ("File not found in VFS: " ++ show uri ++ show vfs)
    Just vf@(VirtualFile _v txt) ->
      let tfn = virtualFileName (vfsTempDir vfs) uri vf
          action = do
            exists <- doesFileExist tfn
            unless exists (writeFile tfn (Rope.toString txt))
      in (tfn, action)

-- ---------------------------------------------------------------------

closeVFS :: VFS -> J.DidCloseTextDocumentNotification -> (VFS, [String])
closeVFS vfs (J.NotificationMessage _ _ params) =
  let J.DidCloseTextDocumentParams (J.TextDocumentIdentifier uri) = params
  in (updateVFS (Map.delete (J.toNormalizedUri uri)) vfs,[])

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
applyChanges :: Rope -> [J.TextDocumentContentChangeEvent] -> Rope
applyChanges = foldl' applyChange

-- ---------------------------------------------------------------------

applyChange :: Rope -> J.TextDocumentContentChangeEvent -> Rope
applyChange _ (J.TextDocumentContentChangeEvent Nothing Nothing str)
  = Rope.fromText str
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range (J.Position sl sc) _to)) (Just len) txt)
  = changeChars str start len txt
  where
    start = Rope.rowColumnCodeUnits (Rope.RowColumn sl sc) str
applyChange str (J.TextDocumentContentChangeEvent (Just (J.Range (J.Position sl sc) (J.Position el ec))) Nothing txt)
  = changeChars str start len txt
  where
    start = Rope.rowColumnCodeUnits (Rope.RowColumn sl sc) str
    end = Rope.rowColumnCodeUnits (Rope.RowColumn el ec) str
    len = end - start
applyChange str (J.TextDocumentContentChangeEvent Nothing (Just _) _txt)
  = str

-- ---------------------------------------------------------------------

changeChars :: Rope -> Int -> Int -> Text -> Rope
changeChars str start len new = mconcat [before, Rope.fromText new, after']
  where
    (before, after) = Rope.splitAt start str
    after' = Rope.drop len after

-- ---------------------------------------------------------------------

-- TODO:AZ:move this to somewhere sane
-- | Describes the line at the current cursor position
data PosPrefixInfo = PosPrefixInfo
  { fullLine :: T.Text
    -- ^ The full contents of the line the cursor is at

  , prefixModule :: T.Text
    -- ^ If any, the module name that was typed right before the cursor position.
    --  For example, if the user has typed "Data.Maybe.from", then this property
    --  will be "Data.Maybe"

  , prefixText :: T.Text
    -- ^ The word right before the cursor position, after removing the module part.
    -- For example if the user has typed "Data.Maybe.from",
    -- then this property will be "from"
  , cursorPos :: J.Position
    -- ^ The cursor position
  } deriving (Show,Eq)

getCompletionPrefix :: (Monad m) => J.Position -> VirtualFile -> m (Maybe PosPrefixInfo)
getCompletionPrefix pos@(J.Position l c) (VirtualFile _ ropetext) =
      return $ Just $ fromMaybe (PosPrefixInfo "" "" "" pos) $ do -- Maybe monad
        let headMaybe [] = Nothing
            headMaybe (x:_) = Just x
            lastMaybe [] = Nothing
            lastMaybe xs = Just $ last xs

        curLine <- headMaybe $ T.lines $ Rope.toText
                             $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine l ropetext
        let beforePos = T.take c curLine
        curWord <- case T.last beforePos of
                     ' ' -> return "" -- don't count abc as the curword in 'abc '
                     _ -> lastMaybe (T.words beforePos)

        let parts = T.split (=='.')
                      $ T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'"::String)) curWord
        case reverse parts of
          [] -> Nothing
          (x:xs) -> do
            let modParts = dropWhile (not . isUpper . T.head)
                                $ reverse $ filter (not .T.null) xs
                modName = T.intercalate "." modParts
            return $ PosPrefixInfo curLine modName x pos

-- ---------------------------------------------------------------------

rangeLinesFromVfs :: VirtualFile -> J.Range -> T.Text
rangeLinesFromVfs (VirtualFile _ ropetext) (J.Range (J.Position lf _cf) (J.Position lt _ct)) = r
  where
    (_ ,s1) = Rope.splitAtLine lf ropetext
    (s2, _) = Rope.splitAtLine (lt - lf) s1
    r = Rope.toText s2
-- ---------------------------------------------------------------------
