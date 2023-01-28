{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations #-}

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
  , vfsMap
  , vfsTempDir
  , VirtualFile(..)
  , lsp_version
  , file_version
  , file_text
  , virtualFileText
  , virtualFileVersion
  , VfsLog (..)
  -- * Managing the VFS
  , initVFS
  , openVFS
  , changeFromClientVFS
  , changeFromServerVFS
  , persistFileVFS
  , closeVFS

  -- * Positions and transformations
  , CodePointPosition (..)
  , line
  , character
  , codePointPositionToPosition
  , positionToCodePointPosition
  , CodePointRange (..)
  , start
  , end
  , codePointRangeToRange
  , rangeToCodePointRange

  -- * manipulating the file contents
  , rangeLinesFromVfs
  , PosPrefixInfo(..)
  , getCompletionPrefix

  -- * for tests
  , applyChanges
  , applyChange
  , changeChars
  ) where

import           Control.Lens hiding ( (<.>), parts )
import           Control.Monad
import           Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import           Control.Monad.State
import           Data.Char (isUpper, isAlphaNum)
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Int (Int32)
import           Data.List
import           Data.Ord
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text.Rope as URope
import           Data.Text.Utf16.Rope ( Rope )
import qualified Data.Text.Utf16.Rope as Rope
import           Data.Text.Prettyprint.Doc hiding (line)
import qualified Language.LSP.Types           as J
import qualified Language.LSP.Types.Lens      as J
import           System.FilePath
import           Data.Hashable
import           System.Directory
import           System.IO
import           System.IO.Temp
import Data.Foldable (traverse_)

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

data VirtualFile =
  VirtualFile {
      _lsp_version :: !Int32  -- ^ The LSP version of the document
    , _file_version :: !Int -- ^ This number is only incremented whilst the file
                           -- remains in the map.
    , _file_text    :: !Rope  -- ^ The full contents of the document
    } deriving (Show)

data VFS = VFS { _vfsMap :: !(Map.Map J.NormalizedUri VirtualFile)
               , _vfsTempDir :: !FilePath -- ^ This is where all the temporary files will be written to
               } deriving Show

data VfsLog =
  SplitInsideCodePoint Rope.Position Rope
  | URINotFound J.NormalizedUri
  | Opening J.NormalizedUri
  | Closing J.NormalizedUri
  | PersistingFile J.NormalizedUri FilePath
  | CantRecursiveDelete J.NormalizedUri
  | DeleteNonExistent J.NormalizedUri
  deriving (Show)

instance Pretty VfsLog where
  pretty (SplitInsideCodePoint pos r) =
    "VFS: asked to make change inside code point. Position" <+> viaShow pos <+> "in" <+> viaShow r
  pretty (URINotFound uri) = "VFS: don't know about URI" <+> viaShow uri
  pretty (Opening uri) = "VFS: opening" <+> viaShow uri
  pretty (Closing uri) = "VFS: closing" <+> viaShow uri
  pretty (PersistingFile uri fp) = "VFS: Writing virtual file for" <+> viaShow uri <+> "to" <+> viaShow fp
  pretty (CantRecursiveDelete uri) =
    "VFS: can't recursively delete" <+> viaShow uri <+> "because we don't track directory status"
  pretty (DeleteNonExistent uri) = "VFS: asked to delete non-existent file" <+> viaShow uri

makeFieldsNoPrefix ''VirtualFile
makeFieldsNoPrefix ''VFS

---

virtualFileText :: VirtualFile -> Text
virtualFileText vf = Rope.toText (_file_text vf)

virtualFileVersion :: VirtualFile -> Int32
virtualFileVersion vf = _lsp_version vf

---

initVFS :: (VFS -> IO r) -> IO r
initVFS k = withSystemTempDirectory "haskell-lsp" $ \temp_dir -> k (VFS mempty temp_dir)

-- ---------------------------------------------------------------------

-- | Applies the changes from a 'J.DidOpenTextDocument' to the 'VFS'
openVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.Message 'J.TextDocumentDidOpen -> m ()
openVFS logger msg = do
  let J.TextDocumentItem (J.toNormalizedUri -> uri) _ version text = msg ^. J.params . J.textDocument
      vfile = VirtualFile version 0 (Rope.fromText text)
  logger <& Opening uri `WithSeverity` Debug
  vfsMap . at uri .= Just vfile

-- ---------------------------------------------------------------------

-- | Applies a 'DidChangeTextDocumentNotification' to the 'VFS'
changeFromClientVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.Message 'J.TextDocumentDidChange -> m ()
changeFromClientVFS logger msg = do
  let
    J.DidChangeTextDocumentParams vid (J.List changes) = msg ^. J.params
    -- the client shouldn't be sending over a null version, only the server, but we just use 0 if that happens
    J.VersionedTextDocumentIdentifier (J.toNormalizedUri -> uri) (fromMaybe 0 -> version) = vid
  vfs <- get
  case vfs ^. vfsMap . at uri of
    Just (VirtualFile _ file_ver contents) -> do
      contents' <- applyChanges logger contents changes
      vfsMap . at uri .= Just (VirtualFile version (file_ver + 1) contents')
    Nothing -> logger <& URINotFound uri `WithSeverity` Warning

-- ---------------------------------------------------------------------

applyCreateFile :: (MonadState VFS m) => J.CreateFile -> m ()
applyCreateFile (J.CreateFile (J.toNormalizedUri -> uri) options _ann) =
  vfsMap %= Map.insertWith
                (\ new old -> if shouldOverwrite then new else old)
                uri
                (VirtualFile 0 0 mempty)
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

applyRenameFile :: (MonadState VFS m) => J.RenameFile -> m ()
applyRenameFile (J.RenameFile (J.toNormalizedUri -> oldUri) (J.toNormalizedUri -> newUri) options _ann) = do
  vfs <- get
  case vfs ^. vfsMap . at oldUri of
      -- nothing to rename
      Nothing -> pure ()
      Just file -> case vfs ^. vfsMap . at newUri of
        -- the target does not exist, just move over
        Nothing -> do
          vfsMap . at oldUri .= Nothing
          vfsMap . at newUri .= Just file
        Just _  -> when shouldOverwrite $ do
          vfsMap . at oldUri .= Nothing
          vfsMap . at newUri .= Just file
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

applyDeleteFile :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.DeleteFile -> m ()
applyDeleteFile logger (J.DeleteFile (J.toNormalizedUri -> uri) options _ann) = do
  -- NOTE: we are ignoring the `recursive` option here because we don't know which file is a directory
  when (options ^? _Just . J.recursive . _Just == Just True) $
    logger <& CantRecursiveDelete uri `WithSeverity` Warning
  -- Remove and get the old value so we can check if it was missing
  old <- vfsMap . at uri <.= Nothing
  case old of
    -- It's not entirely clear what the semantics of 'ignoreIfNotExists' are, but if it
    -- doesn't exist and we're not ignoring it, let's at least log it.
    Nothing | options ^? _Just . J.ignoreIfNotExists . _Just /= Just True ->
              logger <& CantRecursiveDelete uri `WithSeverity` Warning
    _ -> pure ()

applyTextDocumentEdit :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.TextDocumentEdit -> m ()
applyTextDocumentEdit logger (J.TextDocumentEdit vid (J.List edits)) = do
  -- all edits are supposed to be applied at once
  -- so apply from bottom up so they don't affect others
  let sortedEdits = sortOn (Down . editRange) edits
      changeEvents = map editToChangeEvent sortedEdits
      ps = J.DidChangeTextDocumentParams vid (J.List changeEvents)
      notif = J.NotificationMessage "" J.STextDocumentDidChange ps
  changeFromClientVFS logger notif

  where
    editRange :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.Range
    editRange (J.InR e) = e ^. J.range
    editRange (J.InL e) = e ^. J.range

    editToChangeEvent :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.TextDocumentContentChangeEvent
    editToChangeEvent (J.InR e) = J.TextDocumentContentChangeEvent (Just $ e ^. J.range) Nothing (e ^. J.newText)
    editToChangeEvent (J.InL e) = J.TextDocumentContentChangeEvent (Just $ e ^. J.range) Nothing (e ^. J.newText)

applyDocumentChange :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.DocumentChange -> m ()
applyDocumentChange logger (J.InL               change)   = applyTextDocumentEdit logger change
applyDocumentChange _      (J.InR (J.InL        change))  = applyCreateFile change
applyDocumentChange _      (J.InR (J.InR (J.InL change))) = applyRenameFile change
applyDocumentChange logger (J.InR (J.InR (J.InR change))) = applyDeleteFile logger change

-- | Applies the changes from a 'ApplyWorkspaceEditRequest' to the 'VFS'
changeFromServerVFS :: forall m . MonadState VFS m => LogAction m (WithSeverity VfsLog) -> J.Message 'J.WorkspaceApplyEdit -> m ()
changeFromServerVFS logger msg = do
  let J.ApplyWorkspaceEditParams _label edit = msg ^. J.params
      J.WorkspaceEdit mChanges mDocChanges _anns = edit
  case mDocChanges of
    Just (J.List docChanges) -> applyDocumentChanges docChanges
    Nothing -> case mChanges of
      Just cs -> applyDocumentChanges $ map J.InL $ HashMap.foldlWithKey' changeToTextDocumentEdit [] cs
      Nothing -> pure ()

  where
    changeToTextDocumentEdit acc uri edits =
      acc ++ [J.TextDocumentEdit (J.VersionedTextDocumentIdentifier uri (Just 0)) (fmap J.InL edits)]

    applyDocumentChanges :: [J.DocumentChange] -> m ()
    applyDocumentChanges = traverse_ (applyDocumentChange logger) . sortOn project

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
  in prefix </> basename ++ "-" ++ padLeft 5 file_ver ++ "-" ++ show (hash uri_raw) <.> takeExtensions basename

-- | Write a virtual file to a temporary file if it exists in the VFS.
persistFileVFS :: (MonadIO m) => LogAction m (WithSeverity VfsLog) -> VFS -> J.NormalizedUri -> Maybe (FilePath, m ())
persistFileVFS logger vfs uri =
  case vfs ^. vfsMap . at uri of
    Nothing -> Nothing
    Just vf ->
      let tfn = virtualFileName (vfs ^. vfsTempDir) uri vf
          action = do
            exists <- liftIO $ doesFileExist tfn
            unless exists $ do
               let contents = Rope.toText (_file_text vf)
                   writeRaw h = do
                    -- We honour original file line endings
                    hSetNewlineMode h noNewlineTranslation
                    hSetEncoding h utf8
                    T.hPutStr h contents
               logger <& PersistingFile uri tfn `WithSeverity` Debug
               liftIO $ withFile tfn WriteMode writeRaw
      in Just (tfn, action)

-- ---------------------------------------------------------------------

closeVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.Message 'J.TextDocumentDidClose -> m ()
closeVFS logger msg = do
  let J.DidCloseTextDocumentParams (J.TextDocumentIdentifier (J.toNormalizedUri -> uri)) = msg ^. J.params
  logger <& Closing uri `WithSeverity` Debug
  vfsMap . at uri .= Nothing

-- ---------------------------------------------------------------------

-- | Apply the list of changes.
-- Changes should be applied in the order that they are
-- received from the client.
applyChanges :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> [J.TextDocumentContentChangeEvent] -> m Rope
applyChanges logger = foldM (applyChange logger)

-- ---------------------------------------------------------------------

applyChange :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> J.TextDocumentContentChangeEvent -> m Rope
applyChange _ _ (J.TextDocumentContentChangeEvent Nothing _ str)
  = pure $ Rope.fromText str
applyChange logger str (J.TextDocumentContentChangeEvent (Just (J.Range (J.Position sl sc) (J.Position fl fc))) _ txt)
  = changeChars logger str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) txt

-- ---------------------------------------------------------------------

-- | Given a 'Rope', start and end positions, and some new text, replace
-- the given range with the new text. If the given positions lie within
-- a code point then this does nothing (returns the original 'Rope') and logs.
changeChars :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> Rope.Position -> Rope.Position -> Text -> m Rope
changeChars logger str start finish new = do
 case Rope.splitAtPosition finish str of
   Nothing -> logger <& SplitInsideCodePoint finish str `WithSeverity` Warning >> pure str
   Just (before, after) ->  case Rope.splitAtPosition start before of
     Nothing -> logger <& SplitInsideCodePoint start before `WithSeverity` Warning >> pure str
     Just (before', _) -> pure $ mconcat [before', Rope.fromText new, after]

-- ---------------------------------------------------------------------

-- | A position, like a 'J.Position', but where the offsets in the line are measured in
-- Unicode code points instead of UTF-16 code units.
data CodePointPosition =
  CodePointPosition
    { -- | Line position in a document (zero-based).
      _line      :: J.UInt
      -- | Character offset on a line in a document in \code points\ (zero-based).
    , _character :: J.UInt
    } deriving (Show, Read, Eq, Ord)

-- | A range, like a 'J.Range', but where the offsets in the line are measured in
-- Unicode code points instead of UTF-16 code units.
data CodePointRange =
  CodePointRange
    { _start :: CodePointPosition -- ^ The range's start position.
    , _end   :: CodePointPosition -- ^ The range's end position.
    } deriving (Show, Read, Eq, Ord)

makeFieldsNoPrefix ''CodePointPosition
makeFieldsNoPrefix ''CodePointRange

{- Note [Converting between code points and code units]
This is inherently a somewhat expensive operation, but we take some care to minimize the cost.
In particular, we use the good asymptotics of 'Rope' to our advantage:
- We extract the single line that we are interested in in time logarithmic in the number of lines.
- We then split the line at the given position, and check how long the prefix is, which takes
linear time in the length of the (single) line.

We also may need to convert the line back and forth between ropes with different indexing. Again
this is linear time in the length of the line.

So the overall process is logarithmic in the number of lines, and linear in the length of the specific
line. Which is okay-ish, so long as we don't have very long lines.
-}

-- | Extracts a specific line from a 'Rope.Rope'.
-- Logarithmic in the number of lines.
extractLine :: Rope.Rope -> Word -> Maybe Rope.Rope
extractLine rope l = do
  -- Check for the line being out of bounds
  let lastLine = Rope.posLine $ Rope.lengthAsPosition rope
  guard $ l <= lastLine

  let (_, suffix) = Rope.splitAtLine l rope
      (prefix, _) = Rope.splitAtLine 1 suffix
  pure prefix

-- | Translate a code-point offset into a code-unit offset.
-- Linear in the length of the rope.
codePointOffsetToCodeUnitOffset :: URope.Rope -> Word -> Maybe Word
codePointOffsetToCodeUnitOffset rope offset = do
  -- Check for the position being out of bounds
  guard $ offset <= URope.length rope
  -- Split at the given position in \code points\
  let (prefix, _) = URope.splitAt offset rope
      -- Convert the prefix to a rope using \code units\
      utf16Prefix = Rope.fromText $ URope.toText prefix
      -- Get the length of the prefix in \code units\
  pure $ Rope.length utf16Prefix

-- | Translate a UTF-16 code-unit offset into a code-point offset.
-- Linear in the length of the rope.
codeUnitOffsetToCodePointOffset :: Rope.Rope -> Word -> Maybe Word
codeUnitOffsetToCodePointOffset rope offset = do
  -- Check for the position being out of bounds
  guard $ offset <= Rope.length rope
  -- Split at the given position in \code units\
  (prefix, _) <- Rope.splitAt offset rope
  -- Convert the prefix to a rope using \code points\
  let utfPrefix = URope.fromText $ Rope.toText prefix
      -- Get the length of the prefix in \code points\
  pure $ URope.length utfPrefix

-- | Given a virtual file, translate a 'CodePointPosition' in that file into a 'J.Position' in that file.
--
-- Will return 'Nothing' if the requested position is out of bounds of the document.
--
-- Logarithmic in the number of lines in the document, and linear in the length of the line containing
-- the position.
codePointPositionToPosition :: VirtualFile -> CodePointPosition -> Maybe J.Position
codePointPositionToPosition vFile (CodePointPosition l cpc) = do
  -- See Note [Converting between code points and code units]
  let text = _file_text vFile
  utf16Line <- extractLine text (fromIntegral l)
  -- Convert the line a rope using \code points\
  let utfLine = URope.fromText $ Rope.toText utf16Line

  cuc <- codePointOffsetToCodeUnitOffset utfLine (fromIntegral cpc)
  pure $ J.Position l (fromIntegral cuc)

-- | Given a virtual file, translate a 'CodePointRange' in that file into a 'J.Range' in that file.
--
-- Will return 'Nothing' if any of the positions are out of bounds of the document.
--
-- Logarithmic in the number of lines in the document, and linear in the length of the lines containing
-- the positions.
codePointRangeToRange :: VirtualFile -> CodePointRange -> Maybe J.Range
codePointRangeToRange vFile (CodePointRange b e) =
  J.Range <$> codePointPositionToPosition vFile b <*> codePointPositionToPosition vFile e

-- | Given a virtual file, translate a 'J.Position' in that file into a 'CodePointPosition' in that file.
--
-- Will return 'Nothing' if the requested position lies inside a code point, or if it is out of bounds of the document.
--
-- Logarithmic in the number of lines in the document, and linear in the length of the line containing
-- the position.
positionToCodePointPosition :: VirtualFile -> J.Position -> Maybe CodePointPosition
positionToCodePointPosition vFile (J.Position l cuc) = do
  -- See Note [Converting between code points and code units]
  let text = _file_text vFile
  utf16Line <- extractLine text (fromIntegral l)

  cpc <- codeUnitOffsetToCodePointOffset utf16Line (fromIntegral cuc)
  pure $ CodePointPosition l (fromIntegral cpc)

-- | Given a virtual file, translate a 'J.Range' in that file into a 'CodePointRange' in that file.
--
-- Will return 'Nothing' if any of the positions are out of bounds of the document.
--
-- Logarithmic in the number of lines in the document, and linear in the length of the lines containing
-- the positions.
rangeToCodePointRange :: VirtualFile -> J.Range -> Maybe CodePointRange
rangeToCodePointRange vFile (J.Range b e) =
  CodePointRange <$> positionToCodePointPosition vFile b <*> positionToCodePointPosition vFile e

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
        let lastMaybe [] = Nothing
            lastMaybe xs = Just $ last xs

        let curRope = fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
        beforePos <- Rope.toText . fst <$> Rope.splitAt (fromIntegral c) curRope
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
            -- curRope is already a single line, but it may include an enclosing '\n'
            let curLine = T.dropWhileEnd (== '\n') $ Rope.toText curRope
            return $ PosPrefixInfo curLine modName x pos

-- ---------------------------------------------------------------------

rangeLinesFromVfs :: VirtualFile -> J.Range -> T.Text
rangeLinesFromVfs (VirtualFile _ _ ropetext) (J.Range (J.Position lf _cf) (J.Position lt _ct)) = r
  where
    (_ ,s1) = Rope.splitAtLine (fromIntegral lf) ropetext
    (s2, _) = Rope.splitAtLine (fromIntegral (lt - lf)) s1
    r = Rope.toText s2
-- ---------------------------------------------------------------------
