{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Handles the "Language.LSP.Types.TextDocumentDidChange" \/
"Language.LSP.Types.TextDocumentDidOpen" \/
"Language.LSP.Types.TextDocumentDidClose" messages to keep an in-memory
`filesystem` of the current client workspace.  The server can access and edit
files in the client workspace by operating on the "VFS" in "LspFuncs".
-}
module Language.LSP.VFS (
  VFS (..),
  vfsMap,
  VirtualFile (..),
  lsp_version,
  file_version,
  file_text,
  virtualFileText,
  virtualFileVersion,
  VfsLog (..),

  -- * Managing the VFS
  emptyVFS,
  openVFS,
  changeFromClientVFS,
  changeFromServerVFS,
  persistFileVFS,
  closeVFS,

  -- * Positions and transformations
  CodePointPosition (..),
  line,
  character,
  codePointPositionToPosition,
  positionToCodePointPosition,
  CodePointRange (..),
  start,
  end,
  codePointRangeToRange,
  rangeToCodePointRange,

  -- * manipulating the file contents
  rangeLinesFromVfs,
  PosPrefixInfo (..),
  getCompletionPrefix,

  -- * for tests
  applyChanges,
  applyChange,
  changeChars,
) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Control.Lens hiding (parts, (<.>))
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum, isUpper)
import Data.Foldable (traverse_)
import Data.Hashable
import Data.Int (Int32)
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Row
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lines as Char (Position (..))
import Data.Text.Prettyprint.Doc hiding (line)
import Data.Text.Utf16.Lines as Utf16 (Position (..))
import Data.Text.Utf16.Rope.Mixed (Rope)
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message qualified as J
import Language.LSP.Protocol.Types qualified as J
import System.Directory
import System.FilePath
import System.IO

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

data VirtualFile = VirtualFile
  { _lsp_version :: !Int32
  -- ^ The LSP version of the document
  , _file_version :: !Int
  -- ^ This number is only incremented whilst the file
  -- remains in the map.
  , _file_text :: !Rope
  -- ^ The full contents of the document
  }
  deriving (Show)

data VFS = VFS
  { _vfsMap :: !(Map.Map J.NormalizedUri VirtualFile)
  }
  deriving (Show)

data VfsLog
  = SplitInsideCodePoint Utf16.Position Rope
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
  pretty (URINotFound uri) = "VFS: don't know about URI" <+> pretty uri
  pretty (Opening uri) = "VFS: opening" <+> pretty uri
  pretty (Closing uri) = "VFS: closing" <+> pretty uri
  pretty (PersistingFile uri fp) = "VFS: Writing virtual file for" <+> pretty uri <+> "to" <+> viaShow fp
  pretty (CantRecursiveDelete uri) =
    "VFS: can't recursively delete" <+> pretty uri <+> "because we don't track directory status"
  pretty (DeleteNonExistent uri) = "VFS: asked to delete non-existent file" <+> pretty uri

makeFieldsNoPrefix ''VirtualFile
makeFieldsNoPrefix ''VFS

---

virtualFileText :: VirtualFile -> Text
virtualFileText vf = Rope.toText (_file_text vf)

virtualFileVersion :: VirtualFile -> Int32
virtualFileVersion vf = _lsp_version vf

---

emptyVFS :: VFS
emptyVFS = VFS mempty

-- ---------------------------------------------------------------------

-- | Applies the changes from a 'J.DidOpenTextDocument' to the 'VFS'
openVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.TMessage 'J.Method_TextDocumentDidOpen -> m ()
openVFS logger msg = do
  let J.TextDocumentItem (J.toNormalizedUri -> uri) _ version text = msg ^. J.params . J.textDocument
      vfile = VirtualFile version 0 (Rope.fromText text)
  logger <& Opening uri `WithSeverity` Debug
  vfsMap . at uri .= Just vfile

-- ---------------------------------------------------------------------

-- | Applies a 'DidChangeTextDocumentNotification' to the 'VFS'
changeFromClientVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.TMessage 'J.Method_TextDocumentDidChange -> m ()
changeFromClientVFS logger msg = do
  let
    J.DidChangeTextDocumentParams vid changes = msg ^. J.params
    -- the client shouldn't be sending over a null version, only the server, but we just use 0 if that happens
    J.VersionedTextDocumentIdentifier (J.toNormalizedUri -> uri) version = vid
  vfs <- get
  case vfs ^. vfsMap . at uri of
    Just (VirtualFile _ file_ver contents) -> do
      contents' <- applyChanges logger contents changes
      vfsMap . at uri .= Just (VirtualFile version (file_ver + 1) contents')
    Nothing -> logger <& URINotFound uri `WithSeverity` Warning

-- ---------------------------------------------------------------------

applyCreateFile :: (MonadState VFS m) => J.CreateFile -> m ()
applyCreateFile (J.CreateFile _ann _kind (J.toNormalizedUri -> uri) options) =
  vfsMap
    %= Map.insertWith
      (\new old -> if shouldOverwrite then new else old)
      uri
      (VirtualFile 0 0 mempty)
 where
  shouldOverwrite :: Bool
  shouldOverwrite = case options of
    Nothing -> False -- default
    Just (J.CreateFileOptions Nothing Nothing) -> False -- default
    Just (J.CreateFileOptions Nothing (Just True)) -> False -- `ignoreIfExists` is True
    Just (J.CreateFileOptions Nothing (Just False)) -> True -- `ignoreIfExists` is False
    Just (J.CreateFileOptions (Just True) Nothing) -> True -- `overwrite` is True
    Just (J.CreateFileOptions (Just True) (Just True)) -> True -- `overwrite` wins over `ignoreIfExists`
    Just (J.CreateFileOptions (Just True) (Just False)) -> True -- `overwrite` is True
    Just (J.CreateFileOptions (Just False) Nothing) -> False -- `overwrite` is False
    Just (J.CreateFileOptions (Just False) (Just True)) -> False -- `overwrite` is False
    Just (J.CreateFileOptions (Just False) (Just False)) -> False -- `overwrite` wins over `ignoreIfExists`

applyRenameFile :: (MonadState VFS m) => J.RenameFile -> m ()
applyRenameFile (J.RenameFile _ann _kind (J.toNormalizedUri -> oldUri) (J.toNormalizedUri -> newUri) options) = do
  vfs <- get
  case vfs ^. vfsMap . at oldUri of
    -- nothing to rename
    Nothing -> pure ()
    Just file -> case vfs ^. vfsMap . at newUri of
      -- the target does not exist, just move over
      Nothing -> do
        vfsMap . at oldUri .= Nothing
        vfsMap . at newUri .= Just file
      Just _ -> when shouldOverwrite $ do
        vfsMap . at oldUri .= Nothing
        vfsMap . at newUri .= Just file
 where
  shouldOverwrite :: Bool
  shouldOverwrite = case options of
    Nothing -> False -- default
    Just (J.RenameFileOptions Nothing Nothing) -> False -- default
    Just (J.RenameFileOptions Nothing (Just True)) -> False -- `ignoreIfExists` is True
    Just (J.RenameFileOptions Nothing (Just False)) -> True -- `ignoreIfExists` is False
    Just (J.RenameFileOptions (Just True) Nothing) -> True -- `overwrite` is True
    Just (J.RenameFileOptions (Just True) (Just True)) -> True -- `overwrite` wins over `ignoreIfExists`
    Just (J.RenameFileOptions (Just True) (Just False)) -> True -- `overwrite` is True
    Just (J.RenameFileOptions (Just False) Nothing) -> False -- `overwrite` is False
    Just (J.RenameFileOptions (Just False) (Just True)) -> False -- `overwrite` is False
    Just (J.RenameFileOptions (Just False) (Just False)) -> False -- `overwrite` wins over `ignoreIfExists`

applyDeleteFile :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.DeleteFile -> m ()
applyDeleteFile logger (J.DeleteFile _ann _kind (J.toNormalizedUri -> uri) options) = do
  -- NOTE: we are ignoring the `recursive` option here because we don't know which file is a directory
  when (options ^? _Just . J.recursive . _Just == Just True) $
    logger <& CantRecursiveDelete uri `WithSeverity` Warning
  -- Remove and get the old value so we can check if it was missing
  old <- vfsMap . at uri <.= Nothing
  case old of
    -- It's not entirely clear what the semantics of 'ignoreIfNotExists' are, but if it
    -- doesn't exist and we're not ignoring it, let's at least log it.
    Nothing
      | options ^? _Just . J.ignoreIfNotExists . _Just /= Just True ->
          logger <& CantRecursiveDelete uri `WithSeverity` Warning
    _ -> pure ()

applyTextDocumentEdit :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.TextDocumentEdit -> m ()
applyTextDocumentEdit logger (J.TextDocumentEdit vid edits) = do
  -- all edits are supposed to be applied at once
  -- so apply from bottom up so they don't affect others
  let sortedEdits = sortOn (Down . editRange) edits
      changeEvents = map editToChangeEvent sortedEdits
      -- TODO: is this right?
      vid' = J.VersionedTextDocumentIdentifier (vid ^. J.uri) (case vid ^. J.version of J.InL v -> v; J.InR _ -> 0)
      ps = J.DidChangeTextDocumentParams vid' changeEvents
      notif = J.TNotificationMessage "" J.SMethod_TextDocumentDidChange ps
  changeFromClientVFS logger notif
 where
  editRange :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.Range
  editRange (J.InR e) = e ^. J.range
  editRange (J.InL e) = e ^. J.range

  editToChangeEvent :: J.TextEdit J.|? J.AnnotatedTextEdit -> J.TextDocumentContentChangeEvent
  editToChangeEvent (J.InR e) = J.TextDocumentContentChangeEvent $ J.InL $ #range .== e ^. J.range .+ #rangeLength .== Nothing .+ #text .== e ^. J.newText
  editToChangeEvent (J.InL e) = J.TextDocumentContentChangeEvent $ J.InL $ #range .== e ^. J.range .+ #rangeLength .== Nothing .+ #text .== e ^. J.newText

applyDocumentChange :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.DocumentChange -> m ()
applyDocumentChange logger (J.InL change) = applyTextDocumentEdit logger change
applyDocumentChange _ (J.InR (J.InL change)) = applyCreateFile change
applyDocumentChange _ (J.InR (J.InR (J.InL change))) = applyRenameFile change
applyDocumentChange logger (J.InR (J.InR (J.InR change))) = applyDeleteFile logger change

-- | Applies the changes from a 'ApplyWorkspaceEditRequest' to the 'VFS'
changeFromServerVFS :: forall m. MonadState VFS m => LogAction m (WithSeverity VfsLog) -> J.TMessage 'J.Method_WorkspaceApplyEdit -> m ()
changeFromServerVFS logger msg = do
  let J.ApplyWorkspaceEditParams _label edit = msg ^. J.params
      J.WorkspaceEdit mChanges mDocChanges _anns = edit
  case mDocChanges of
    Just docChanges -> applyDocumentChanges docChanges
    Nothing -> case mChanges of
      Just cs -> applyDocumentChanges $ map J.InL $ Map.foldlWithKey' changeToTextDocumentEdit [] cs
      Nothing -> pure ()
 where
  changeToTextDocumentEdit acc uri edits =
    acc ++ [J.TextDocumentEdit (J.OptionalVersionedTextDocumentIdentifier uri (J.InL 0)) (fmap J.InL edits)]

  applyDocumentChanges :: [J.DocumentChange] -> m ()
  applyDocumentChanges = traverse_ (applyDocumentChange logger) . sortOn project

  -- for sorting [DocumentChange]
  project :: J.DocumentChange -> Maybe J.Int32
  project (J.InL textDocumentEdit) = case textDocumentEdit ^. J.textDocument . J.version of
    J.InL v -> Just v
    _ -> Nothing
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

-- | Write a virtual file to a file in the given directory if it exists in the VFS.
persistFileVFS :: (MonadIO m) => LogAction m (WithSeverity VfsLog) -> FilePath -> VFS -> J.NormalizedUri -> Maybe (FilePath, m ())
persistFileVFS logger dir vfs uri =
  case vfs ^. vfsMap . at uri of
    Nothing -> Nothing
    Just vf ->
      let tfn = virtualFileName dir uri vf
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

closeVFS :: (MonadState VFS m) => LogAction m (WithSeverity VfsLog) -> J.TMessage 'J.Method_TextDocumentDidClose -> m ()
closeVFS logger msg = do
  let J.DidCloseTextDocumentParams (J.TextDocumentIdentifier (J.toNormalizedUri -> uri)) = msg ^. J.params
  logger <& Closing uri `WithSeverity` Debug
  vfsMap . at uri .= Nothing

-- ---------------------------------------------------------------------

{- | Apply the list of changes.
 Changes should be applied in the order that they are
 received from the client.
-}
applyChanges :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> [J.TextDocumentContentChangeEvent] -> m Rope
applyChanges logger = foldM (applyChange logger)

-- ---------------------------------------------------------------------

applyChange :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> J.TextDocumentContentChangeEvent -> m Rope
applyChange logger str (J.TextDocumentContentChangeEvent (J.InL e))
  | J.Range (J.Position sl sc) (J.Position fl fc) <- e .! #range
  , txt <- e .! #text =
      changeChars logger str (Utf16.Position (fromIntegral sl) (fromIntegral sc)) (Utf16.Position (fromIntegral fl) (fromIntegral fc)) txt
applyChange _ _ (J.TextDocumentContentChangeEvent (J.InR e)) =
  pure $ Rope.fromText $ e .! #text

-- ---------------------------------------------------------------------

{- | Given a 'Rope', start and end positions, and some new text, replace
 the given range with the new text. If the given positions lie within
 a code point then this does nothing (returns the original 'Rope') and logs.
-}
changeChars :: (Monad m) => LogAction m (WithSeverity VfsLog) -> Rope -> Utf16.Position -> Utf16.Position -> Text -> m Rope
changeChars logger str start finish new = do
  case Rope.utf16SplitAtPosition finish str of
    Nothing -> logger <& SplitInsideCodePoint finish str `WithSeverity` Warning >> pure str
    Just (before, after) -> case Rope.utf16SplitAtPosition start before of
      Nothing -> logger <& SplitInsideCodePoint start before `WithSeverity` Warning >> pure str
      Just (before', _) -> pure $ mconcat [before', Rope.fromText new, after]

-- ---------------------------------------------------------------------

{- | A position, like a 'J.Position', but where the offsets in the line are measured in
 Unicode code points instead of UTF-16 code units.
-}
data CodePointPosition = CodePointPosition
  { _line :: J.UInt
  -- ^ Line position in a document (zero-based).
  , _character :: J.UInt
  -- ^ Character offset on a line in a document in *code points* (zero-based).
  }
  deriving (Show, Read, Eq, Ord)

{- | A range, like a 'J.Range', but where the offsets in the line are measured in
 Unicode code points instead of UTF-16 code units.
-}
data CodePointRange = CodePointRange
  { _start :: CodePointPosition
  -- ^ The range's start position.
  , _end :: CodePointPosition
  -- ^ The range's end position.
  }
  deriving (Show, Read, Eq, Ord)

makeFieldsNoPrefix ''CodePointPosition
makeFieldsNoPrefix ''CodePointRange

{- Note [Converting between code points and code units]
This is inherently a somewhat expensive operation, but we take some care to minimize the cost.
In particular, we use the good asymptotics of 'Rope' to our advantage:
- We extract the single line that we are interested in in time logarithmic in the number of lines.
- We then split the line at the given position, and check how long the prefix is, which takes
linear time in the length of the (single) line.

So the overall process is logarithmic in the number of lines, and linear in the length of the specific
line. Which is okay-ish, so long as we don't have very long lines.

We are not able to use the `Rope.splitAtPosition`
Because when column index out of range or when the column indexing at the newline char.
The prefix result would wrap over the line and having the same result (nextLineNum, 0).
We would not be able to distinguish them. When the first case should return `Nothing`,
second case should return a `Just (CurrentLineNum, columnNumberConverted)`.
-}

{- | Extracts a specific line from a 'Rope.Rope'.
 Logarithmic in the number of lines.
-}
extractLine :: Rope.Rope -> Word -> Maybe Rope.Rope
extractLine rope l = do
  -- Check for the line being out of bounds
  let lastLine = Utf16.posLine $ Rope.utf16LengthAsPosition rope
  guard $ l <= lastLine
  let (_, suffix) = Rope.splitAtLine l rope
      (prefix, _) = Rope.splitAtLine 1 suffix
  pure prefix

{- | Given a virtual file, translate a 'CodePointPosition' in that file into a 'J.Position' in that file.

 Will return 'Nothing' if the requested position is out of bounds of the document.

 Logarithmic in the number of lines in the document, and linear in the length of the line containing
 the position.
-}
codePointPositionToPosition :: VirtualFile -> CodePointPosition -> Maybe J.Position
codePointPositionToPosition vFile (CodePointPosition l c) = do
  -- See Note [Converting between code points and code units]
  let text = _file_text vFile
  lineRope <- extractLine text $ fromIntegral l
  guard $ c <= fromIntegral (Rope.charLength lineRope)
  return $ J.Position l (fromIntegral $ Rope.utf16Length $ fst $ Rope.charSplitAt (fromIntegral c) lineRope)

{- | Given a virtual file, translate a 'CodePointRange' in that file into a 'J.Range' in that file.

 Will return 'Nothing' if any of the positions are out of bounds of the document.

 Logarithmic in the number of lines in the document, and linear in the length of the lines containing
 the positions.
-}
codePointRangeToRange :: VirtualFile -> CodePointRange -> Maybe J.Range
codePointRangeToRange vFile (CodePointRange b e) =
  J.Range <$> codePointPositionToPosition vFile b <*> codePointPositionToPosition vFile e

{- | Given a virtual file, translate a 'J.Position' in that file into a 'CodePointPosition' in that file.

 Will return 'Nothing' if the requested position lies inside a code point, or if it is out of bounds of the document.

 Logarithmic in the number of lines in the document, and linear in the length of the line containing
 the position.
-}
positionToCodePointPosition :: VirtualFile -> J.Position -> Maybe CodePointPosition
positionToCodePointPosition vFile (J.Position l c) = do
  -- See Note [Converting between code points and code units]
  let text = _file_text vFile
  lineRope <- extractLine text $ fromIntegral l
  guard $ c <= fromIntegral (Rope.utf16Length lineRope)
  CodePointPosition l . fromIntegral . Rope.charLength . fst <$> Rope.utf16SplitAt (fromIntegral c) lineRope

{- | Given a virtual file, translate a 'J.Range' in that file into a 'CodePointRange' in that file.

 Will return 'Nothing' if any of the positions are out of bounds of the document.

 Logarithmic in the number of lines in the document, and linear in the length of the lines containing
 the positions.
-}
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
  }
  deriving (Show, Eq)

getCompletionPrefix :: (Monad m) => J.Position -> VirtualFile -> m (Maybe PosPrefixInfo)
getCompletionPrefix pos@(J.Position l c) (VirtualFile _ _ ropetext) =
  return $ Just $ fromMaybe (PosPrefixInfo "" "" "" pos) $ do
    -- Maybe monad
    let lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

    let curRope = fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
    beforePos <- Rope.toText . fst <$> Rope.utf16SplitAt (fromIntegral c) curRope
    curWord <-
      if
        | T.null beforePos -> Just ""
        | T.last beforePos == ' ' -> Just "" -- don't count abc as the curword in 'abc '
        | otherwise -> lastMaybe (T.words beforePos)

    let parts =
          T.split (== '.') $
            T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'" :: String)) curWord
    case reverse parts of
      [] -> Nothing
      (x : xs) -> do
        let modParts =
              dropWhile (not . isUpper . T.head) $
                reverse $
                  filter (not . T.null) xs
            modName = T.intercalate "." modParts
        -- curRope is already a single line, but it may include an enclosing '\n'
        let curLine = T.dropWhileEnd (== '\n') $ Rope.toText curRope
        return $ PosPrefixInfo curLine modName x pos

-- ---------------------------------------------------------------------

rangeLinesFromVfs :: VirtualFile -> J.Range -> T.Text
rangeLinesFromVfs (VirtualFile _ _ ropetext) (J.Range (J.Position lf _cf) (J.Position lt _ct)) = r
 where
  (_, s1) = Rope.splitAtLine (fromIntegral lf) ropetext
  (s2, _) = Rope.splitAtLine (fromIntegral (lt - lf)) s1
  r = Rope.toText s2

-- ---------------------------------------------------------------------
