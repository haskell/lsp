{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

{-|
Handles the "Language.LSP.Types.TextDocumentDidChange" \/
"Language.LSP.Types.TextDocumentDidOpen" \/
"Language.LSP.Types.TextDocumentDidClose" messages to keep an in-memory
`filesystem` of the current client workspace.  The server can access and edit
files in the client workspace by operating on the "VFS" in "LspFuncs".
-}
module Language.LSP.VFS
  (
    VFS(..)
  , VirtualFile(..)
  , virtualFileText
  , virtualFileVersion
  -- * Managing the VFS
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
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Rope.UTF16 ( Rope )
import qualified Data.Rope.UTF16 as Rope
import qualified Language.LSP.Types           as J
import qualified Language.LSP.Types.Lens      as J
import           System.FilePath
import           Data.Hashable
import           System.Directory
import           System.IO 
import           System.IO.Temp
import           System.Log.Logger

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

data VirtualFile =
  VirtualFile {
      _lsp_version :: !Int  -- ^ The LSP version of the document
    , _file_version :: !Int -- ^ This number is only incremented whilst the file
                           -- remains in the map.
    , _text    :: !Rope  -- ^ The full contents of the document
    } deriving (Show)


type VFSMap = Map.Map J.NormalizedUri VirtualFile

data VFS = VFS { vfsMap :: !(Map.Map J.NormalizedUri VirtualFile)
               , vfsTempDir :: !FilePath -- ^ This is where all the temporary files will be written to
               } deriving Show

---

virtualFileText :: VirtualFile -> Text
virtualFileText vf = Rope.toText (_text  vf)

virtualFileVersion :: VirtualFile -> Int
virtualFileVersion vf = _lsp_version vf

---

initVFS :: (VFS -> IO r) -> IO r
initVFS k = withSystemTempDirectory "haskell-lsp" $ \temp_dir -> k (VFS mempty temp_dir)

-- ---------------------------------------------------------------------

-- | Applies the changes from a 'J.DidOpenTextDocument' to the 'VFS'
openVFS :: VFS -> J.Message 'J.TextDocumentDidOpen -> (VFS, [String])
openVFS vfs (J.NotificationMessage _ _ params) =
  let J.DidOpenTextDocumentParams
         (J.TextDocumentItem uri _ version text) = params
  in (updateVFS (Map.insert (J.toNormalizedUri uri) (VirtualFile version 0 (Rope.fromText text))) vfs
     , [])


-- ---------------------------------------------------------------------

-- ^ Applies a 'DidChangeTextDocumentNotification' to the 'VFS'
changeFromClientVFS :: VFS -> J.Message 'J.TextDocumentDidChange -> (VFS,[String])
changeFromClientVFS vfs (J.NotificationMessage _ _ params) =
  let
    J.DidChangeTextDocumentParams vid (J.List changes) = params
    J.VersionedTextDocumentIdentifier (J.toNormalizedUri -> uri) version = vid
  in
    case Map.lookup uri (vfsMap vfs) of
      Just (VirtualFile _ file_ver str) ->
        let str' = applyChanges str changes
        -- the client shouldn't be sending over a null version, only the server.
        in (updateVFS (Map.insert uri (VirtualFile (fromMaybe 0 version) (file_ver + 1) str')) vfs, [])
      Nothing ->
        -- logs $ "haskell-lsp:changeVfs:can't find uri:" ++ show uri
        -- return vfs
        (vfs, ["haskell-lsp:changeVfs:can't find uri:" ++ show uri])

updateVFS :: (VFSMap -> VFSMap) -> VFS -> VFS
updateVFS f vfs@VFS{vfsMap} = vfs { vfsMap = f vfsMap }

-- ---------------------------------------------------------------------

applyCreateFile :: J.CreateFile -> VFS -> VFS
applyCreateFile (J.CreateFile uri options _ann) = 
  updateVFS $ Map.insertWith 
                (\ new old -> if shouldOverwrite then new else old)
                (J.toNormalizedUri uri)
                (VirtualFile 0 0 (Rope.fromText ""))
  where 
    shouldOverwrite :: Bool 
    shouldOverwrite = case options of 
        Nothing                                               -> False  -- default
        Just (J.CreateFileOptions Nothing       Nothing     ) -> False  -- default
        Just (J.CreateFileOptions Nothing       (Just True) ) -> False  -- `ignoreIfExists` is True 
        Just (J.CreateFileOptions Nothing       (Just False)) -> True   -- `ignoreIfExists` is False 
        Just (J.CreateFileOptions (Just True)   Nothing     ) -> True   -- `overwrite` is True
        Just (J.CreateFileOptions (Just True)   (Just True) ) -> True   -- `overwrite` wins over `ignoreIfExists`
        Just (J.CreateFileOptions (Just True)   (Just False)) -> True   -- `overwrite` is True
        Just (J.CreateFileOptions (Just False)  Nothing     ) -> False  -- `overwrite` is False
        Just (J.CreateFileOptions (Just False)  (Just True) ) -> False  -- `overwrite` is False
        Just (J.CreateFileOptions (Just False)  (Just False)) -> False  -- `overwrite` wins over `ignoreIfExists`

applyRenameFile :: J.RenameFile -> VFS -> VFS
applyRenameFile (J.RenameFile oldUri' newUri' options _ann) vfs = 
  let oldUri = J.toNormalizedUri oldUri'
      newUri = J.toNormalizedUri newUri'
  in  case Map.lookup oldUri (vfsMap vfs) of 
        -- nothing to rename 
        Nothing -> vfs 
        Just file -> case Map.lookup newUri (vfsMap vfs) of 
          -- the target does not exist, just move over 
          Nothing -> updateVFS (Map.insert newUri file . Map.delete oldUri) vfs
          Just _  -> if shouldOverwrite 
                      then updateVFS (Map.insert newUri file . Map.delete oldUri) vfs
                      else vfs 
  where 
    shouldOverwrite :: Bool 
    shouldOverwrite = case options of 
        Nothing                                               -> False  -- default
        Just (J.RenameFileOptions Nothing       Nothing     ) -> False  -- default
        Just (J.RenameFileOptions Nothing       (Just True) ) -> False  -- `ignoreIfExists` is True 
        Just (J.RenameFileOptions Nothing       (Just False)) -> True   -- `ignoreIfExists` is False 
        Just (J.RenameFileOptions (Just True)   Nothing     ) -> True   -- `overwrite` is True
        Just (J.RenameFileOptions (Just True)   (Just True) ) -> True   -- `overwrite` wins over `ignoreIfExists`
        Just (J.RenameFileOptions (Just True)   (Just False)) -> True   -- `overwrite` is True
        Just (J.RenameFileOptions (Just False)  Nothing     ) -> False  -- `overwrite` is False
        Just (J.RenameFileOptions (Just False)  (Just True) ) -> False  -- `overwrite` is False
        Just (J.RenameFileOptions (Just False)  (Just False)) -> False  -- `overwrite` wins over `ignoreIfExists`

-- NOTE: we are ignoring the `recursive` option here because we don't know which file is a directory
applyDeleteFile :: J.DeleteFile -> VFS -> VFS
applyDeleteFile (J.DeleteFile uri _options _ann) = 
  updateVFS $ Map.delete (J.toNormalizedUri uri)


applyTextDocumentEdit :: J.TextDocumentEdit -> VFS -> IO VFS
applyTextDocumentEdit (J.TextDocumentEdit vid (J.List edits)) vfs = do
  -- all edits are supposed to be applied at once
  -- so apply from bottom up so they don't affect others
  let sortedEdits = sortOn (Down . editRange) edits
      changeEvents = map editToChangeEvent sortedEdits
      ps = J.DidChangeTextDocumentParams vid (J.List changeEvents)
      notif = J.NotificationMessage "" J.STextDocumentDidChange ps
  let (vfs',ls) = changeFromClientVFS vfs notif
  mapM_ (debugM "haskell-lsp.applyTextDocumentEdit") ls
  return vfs'

  where
    editRange :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.Range
    editRange (J.InR e) = e ^. J.range
    editRange (J.InL e) = e ^. J.range

    editToChangeEvent :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.TextDocumentContentChangeEvent
    editToChangeEvent (J.InR e) = J.TextDocumentContentChangeEvent (Just $ e ^. J.range) Nothing (e ^. J.newText)
    editToChangeEvent (J.InL e) = J.TextDocumentContentChangeEvent (Just $ e ^. J.range) Nothing (e ^. J.newText)

applyDocumentChange :: J.DocumentChange -> VFS -> IO VFS 
applyDocumentChange (J.InL               change)   = applyTextDocumentEdit change
applyDocumentChange (J.InR (J.InL        change))  = return . applyCreateFile change
applyDocumentChange (J.InR (J.InR (J.InL change))) = return . applyRenameFile change
applyDocumentChange (J.InR (J.InR (J.InR change))) = return . applyDeleteFile change

-- ^ Applies the changes from a 'ApplyWorkspaceEditRequest' to the 'VFS'
changeFromServerVFS :: VFS -> J.Message 'J.WorkspaceApplyEdit -> IO VFS
changeFromServerVFS initVfs (J.RequestMessage _ _ _ params) = do
  let J.ApplyWorkspaceEditParams _label edit = params
      J.WorkspaceEdit mChanges mDocChanges _anns = edit
  case mDocChanges of
    Just (J.List docChanges) -> applyDocumentChanges docChanges
    Nothing -> case mChanges of
      Just cs -> applyDocumentChanges $ map J.InL $ HashMap.foldlWithKey' changeToTextDocumentEdit [] cs
      Nothing -> do
        debugM "haskell-lsp.changeVfs" "No changes"
        return initVfs

  where
    changeToTextDocumentEdit acc uri edits =
      acc ++ [J.TextDocumentEdit (J.VersionedTextDocumentIdentifier uri (Just 0)) (fmap J.InL edits)]

    applyDocumentChanges :: [J.DocumentChange] -> IO VFS 
    applyDocumentChanges = foldM (flip applyDocumentChange) initVfs . sortOn project
        
    -- for sorting [DocumentChange]
    project :: J.DocumentChange -> J.TextDocumentVersion -- type TextDocumentVersion = Maybe Int
    project (J.InL textDocumentEdit) = textDocumentEdit ^. J.textDocument . J.version
    project _ = Nothing

-- ---------------------------------------------------------------------
virtualFileName :: FilePath -> J.NormalizedUri -> VirtualFile -> FilePath
virtualFileName prefix uri (VirtualFile _ file_ver _) =
  let uri_raw = J.fromNormalizedUri uri
      basename = maybe "" takeFileName (J.uriToFilePath uri_raw)
      -- Given a length and a version number, pad the version number to
      -- the given n. Does nothing if the version number string is longer
      -- than the given length.
      padLeft :: Int -> Int -> String
      padLeft n num =
        let numString = show num
        in replicate (n - length numString) '0' ++ numString
  in prefix </> basename ++ "-" ++ padLeft 5 file_ver ++ "-" ++ show (hash uri_raw) ++ ".hs"

-- | Write a virtual file to a temporary file if it exists in the VFS.
persistFileVFS :: VFS -> J.NormalizedUri -> Maybe (FilePath, IO ())
persistFileVFS vfs uri =
  case Map.lookup uri (vfsMap vfs) of
    Nothing -> Nothing
    Just vf ->
      let tfn = virtualFileName (vfsTempDir vfs) uri vf
          action = do
            exists <- doesFileExist tfn
            unless exists $ do
               let contents = Rope.toString (_text vf)
                   writeRaw h = do
                    -- We honour original file line endings
                    hSetNewlineMode h noNewlineTranslation
                    hSetEncoding h utf8
                    hPutStr h contents
               debugM "haskell-lsp.persistFileVFS" $ "Writing virtual file: " 
                    ++ "uri = " ++ show uri ++ ", virtual file = " ++ show tfn
               withFile tfn WriteMode writeRaw
      in Just (tfn, action)

-- ---------------------------------------------------------------------

closeVFS :: VFS -> J.Message 'J.TextDocumentDidClose -> (VFS, [String])
closeVFS vfs (J.NotificationMessage _ _ params) =
  let J.DidCloseTextDocumentParams (J.TextDocumentIdentifier uri) = params
  in (updateVFS (Map.delete (J.toNormalizedUri uri)) vfs,["Closed: " ++ show uri])

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
  { fullLine :: !T.Text
    -- ^ The full contents of the line the cursor is at

  , prefixModule :: !T.Text
    -- ^ If any, the module name that was typed right before the cursor position.
    --  For example, if the user has typed "Data.Maybe.from", then this property
    --  will be "Data.Maybe"

  , prefixText :: !T.Text
    -- ^ The word right before the cursor position, after removing the module part.
    -- For example if the user has typed "Data.Maybe.from",
    -- then this property will be "from"
  , cursorPos :: !J.Position
    -- ^ The cursor position
  } deriving (Show,Eq)

getCompletionPrefix :: (Monad m) => J.Position -> VirtualFile -> m (Maybe PosPrefixInfo)
getCompletionPrefix pos@(J.Position l c) (VirtualFile _ _ ropetext) =
      return $ Just $ fromMaybe (PosPrefixInfo "" "" "" pos) $ do -- Maybe monad
        let headMaybe [] = Nothing
            headMaybe (x:_) = Just x
            lastMaybe [] = Nothing
            lastMaybe xs = Just $ last xs

        curLine <- headMaybe $ T.lines $ Rope.toText
                             $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine l ropetext
        let beforePos = T.take c curLine
        curWord <-
            if | T.null beforePos -> Just ""
               | T.last beforePos == ' ' -> Just "" -- don't count abc as the curword in 'abc '
               | otherwise -> lastMaybe (T.words beforePos)

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
rangeLinesFromVfs (VirtualFile _ _ ropetext) (J.Range (J.Position lf _cf) (J.Position lt _ct)) = r
  where
    (_ ,s1) = Rope.splitAtLine lf ropetext
    (s2, _) = Rope.splitAtLine (lt - lf) s1
    r = Rope.toText s2
-- ---------------------------------------------------------------------
