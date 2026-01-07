{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Test.Session.UpdateState (
  updateState
  , modifyStatePure
  , modifyStatePure_
  , modifyStateM

  , documentChangeUri
  ) where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Applicative
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Process (gracefullyWaitForProcess)
import Language.LSP.Test.Session.Core
import Language.LSP.Test.Types
import Language.LSP.VFS
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.Timeout (timeout)


updateState :: (MonadLoggerIO m, MonadUnliftIO m, MonadReader SessionContext m)
            => FromServerMessage -> m ()
updateState (FromServerMess SMethod_Progress req) = case req ^. L.params . L.value of
  v | Just _ <- v ^? _workDoneProgressBegin ->
    modifyStatePure_ $ \s -> s { curProgressSessions = Set.insert (req ^. L.params . L.token) $ curProgressSessions s }
  v | Just _ <- v ^? _workDoneProgressEnd ->
    modifyStatePure_ $ \s -> s { curProgressSessions = Set.delete (req ^. L.params . L.token) $ curProgressSessions s }
  _ -> pure ()

-- Keep track of dynamic capability registration
updateState (FromServerMess SMethod_ClientRegisterCapability req) = do
  let
    regs :: [SomeRegistration]
    regs = req ^.. L.params . L.registrations . traversed . to toSomeRegistration . _Just
  let newRegs = (\sr@(SomeRegistration r) -> (r ^. L.id, sr)) <$> regs
  modifyStatePure_ $ \s ->
    s { curDynCaps = Map.union (Map.fromList newRegs) (curDynCaps s) }

updateState (FromServerMess SMethod_ClientUnregisterCapability req) = do
  let unRegs = (^. L.id) <$> req ^. L.params . L.unregisterations
  modifyStatePure_ $ \s ->
    let newCurDynCaps = foldr' Map.delete (curDynCaps s) unRegs
    in s { curDynCaps = newCurDynCaps }

updateState (FromServerMess SMethod_TextDocumentPublishDiagnostics n) = do
  let diags = n ^. L.params . L.diagnostics
      doc = n ^. L.params . L.uri
  modifyStatePure_ $ \s ->
    let newDiags = Map.insert (toNormalizedUri doc) diags (curDiagnostics s)
      in s { curDiagnostics = newDiags }

updateState (FromServerMess SMethod_WorkspaceApplyEdit r) = do

  -- First, prefer the versioned documentChanges field
  allChangeParams <- case r ^. L.params . L.edit . L.documentChanges of
    Just cs -> do
      mapM_ (checkIfNeedsOpened . documentChangeUri) cs
      -- replace the user provided version numbers with the VFS ones + 1
      -- (technically we should check that the user versions match the VFS ones)
      cs' <- traverseOf (traverse . _L . L.textDocument . _versionedTextDocumentIdentifier) bumpNewestVersion cs
      return $ mapMaybe getParamsFromDocumentChange cs'
    -- Then fall back to the changes field
    Nothing -> case r ^. L.params . L.edit . L.changes of
      Just cs -> do
        mapM_ checkIfNeedsOpened (Map.keys cs)
        concat <$> mapM (uncurry getChangeParams) (Map.toList cs)
      Nothing ->
        error "WorkspaceEdit contains neither documentChanges nor changes!"

  modifyStatePure_ $ \s ->
    let newVFS = flip execState (vfs s) $ changeFromServerVFS logger r
    in s { vfs = newVFS }

  let groupedParams = groupBy (\a b -> a ^. L.textDocument == b ^. L.textDocument) allChangeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams (sendMessage . TNotificationMessage "2.0" SMethod_TextDocumentDidChange)

  -- Update VFS to new document versions
  let sortedVersions = map (sortBy (compare `on` (^. L.textDocument . L.version))) groupedParams
      latestVersions = map ((^. L.textDocument) . last) sortedVersions

  forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
    modifyStatePure_ $ \s ->
      let oldVFS = vfs s
          update (VirtualFile _ file_ver t _kind) = VirtualFile v (file_ver +1) t _kind
          newVFS = oldVFS & vfsMap . ix (toNormalizedUri uri) . _Open %~ update
      in s { vfs = newVFS }

  where
        logger = LogAction $ \(WithSeverity msg sev) -> case sev of { Error -> error $ show msg; _ -> pure () }
        checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> (asks sessionState >>= readMVar)

          -- if its not open, open it
          unless (has (vfsMap . ix (toNormalizedUri uri)) oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = TNotificationMessage "2.0" SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams item)
            sendMessage msg

            modifyStatePure_ $ \s ->
              let newVFS = flip execState (vfs s) $ openVFS logger msg
              in s { vfs = newVFS }

        getParamsFromTextDocumentEdit :: TextDocumentEdit -> Maybe DidChangeTextDocumentParams
        getParamsFromTextDocumentEdit (TextDocumentEdit docId edits) =
          DidChangeTextDocumentParams <$> docId ^? _versionedTextDocumentIdentifier <*> pure (map editToChangeEvent edits)

        -- TODO: move somewhere reusable
        editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
        editToChangeEvent (InR e) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial { _range = e ^. L.range , _rangeLength = Nothing , _text = e ^. L.newText }
        editToChangeEvent (InL e) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial { _range = e ^. L.range , _rangeLength = Nothing , _text = e ^. L.newText }

        getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
        getParamsFromDocumentChange (InL textDocumentEdit) = getParamsFromTextDocumentEdit textDocumentEdit
        getParamsFromDocumentChange _ = Nothing

        bumpNewestVersion (VersionedTextDocumentIdentifier uri _) =
          head <$> textDocumentVersions uri

        -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
        -- where n is the current version
        textDocumentVersions uri = do
          vfs' <- vfs <$> (asks sessionState >>= readMVar)
          let curVer = fromMaybe 0 $ vfs' ^? vfsMap . ix (toNormalizedUri uri) . _Open . lsp_version
          pure $ map (VersionedTextDocumentIdentifier uri) [curVer + 1..]

        textDocumentEdits uri edits = do
          vers <- textDocumentVersions uri
          pure $ zipWith (\v e -> TextDocumentEdit (review _versionedTextDocumentIdentifier v) [InL e]) vers edits

        getChangeParams uri edits = do
          edits <- textDocumentEdits uri (reverse edits)
          pure $ mapMaybe getParamsFromTextDocumentEdit edits

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. L.contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. L.textDocument) events
updateState _ = return ()

modifyStatePure :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> (SessionState, a)) -> m a
modifyStatePure f = do
  ss <- asks sessionState
  modifyMVar ss (pure . f)

modifyStatePure_ :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> SessionState) -> m ()
modifyStatePure_ f = do
  ss <- asks sessionState
  modifyMVar_ ss (pure . f)

modifyStateM :: (MonadUnliftIO m, MonadReader SessionContext m) => (SessionState -> m SessionState) -> m ()
modifyStateM f = do
  ss <- asks sessionState
  modifyMVar_ ss f

-- extract Uri out from DocumentChange
-- didn't put this in `lsp-types` because TH was getting in the way
documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. L.textDocument . L.uri
documentChangeUri (InR (InL x)) = x ^. L.uri
documentChangeUri (InR (InR (InL x))) = x ^. L.oldUri
documentChangeUri (InR (InR (InR x))) = x ^. L.uri
