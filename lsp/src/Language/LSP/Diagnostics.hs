{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Diagnostics (
  DiagnosticsHandle,
  new,
  DiagnosticStore,
  DiagnosticsBySource,
  StoreItem (..),
  partitionBySource,
  flushBySource,
  updateDiagnostics,
  getDiagnosticParamsFor,
  publishDiagnostics,
  flushDiagnosticsBySource,
) where

import Control.Concurrent.STM
import Control.Monad
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.SortedList qualified as SL
import Data.Text (Text)
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC
import JSONRPC.Typed.Server
import Language.LSP.MethodInstance ()
import Language.LSP.Protocol.Message qualified as J
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as J

{-
We need a three level store

  Uri : Maybe Int32 : Maybe DiagnosticSource : [Diagnostics]

For a given Uri, as soon as we see a new (Maybe Int32) we flush
all prior entries for the Uri.

-}

type DiagnosticStore = HM.HashMap J.NormalizedUri StoreItem

data DiagnosticsHandle = DiagnosticsHandle
  { diagnosticStore :: TVar DiagnosticStore
  , serverHandle :: ServerHandle Server J.Method
  }

new :: ServerHandle Server J.Method -> IO DiagnosticsHandle
new serverHandle = do
  diagnosticStore <- newTVarIO mempty
  pure DiagnosticsHandle{..}

data StoreItem
  = StoreItem (Maybe J.Int32) DiagnosticsBySource
  deriving stock (Show, Eq)

type DiagnosticsBySource = Map.Map (Maybe Text) (SL.SortedList J.Diagnostic)

-- ---------------------------------------------------------------------

partitionBySource :: [J.Diagnostic] -> DiagnosticsBySource
partitionBySource diags = Map.fromListWith mappend $ map (\d -> (J._source d, SL.singleton d)) diags

-- ---------------------------------------------------------------------

flushBySource :: DiagnosticStore -> Maybe Text -> DiagnosticStore
flushBySource store Nothing = store
flushBySource store (Just source) = HM.map remove store
 where
  remove (StoreItem mv diags) = StoreItem mv (Map.delete (Just source) diags)

-- ---------------------------------------------------------------------

updateDiagnostics ::
  DiagnosticStore ->
  J.NormalizedUri ->
  Maybe J.Int32 ->
  DiagnosticsBySource ->
  DiagnosticStore
updateDiagnostics store uri mv newDiagsBySource = r
 where
  newStore :: DiagnosticStore
  newStore = HM.insert uri (StoreItem mv newDiagsBySource) store

  updateDbs dbs = HM.insert uri newItem store
   where
    newItem = StoreItem mv newDbs
    -- note: Map.union is left-biased, so for identical keys the first
    -- argument is used
    newDbs = Map.union newDiagsBySource dbs

  r = case HM.lookup uri store of
    Nothing -> newStore
    Just (StoreItem mvs dbs) ->
      if mvs /= mv
        then newStore
        else updateDbs dbs

-- ---------------------------------------------------------------------

getDiagnosticParamsFor :: Int -> DiagnosticStore -> J.NormalizedUri -> Maybe J.PublishDiagnosticsParams
getDiagnosticParamsFor maxDiagnostics ds uri =
  case HM.lookup uri ds of
    Nothing -> Nothing
    Just (StoreItem mv diags) ->
      Just $ J.PublishDiagnosticsParams (J.fromNormalizedUri uri) (fmap fromIntegral mv) (take maxDiagnostics $ SL.fromSortedList $ mconcat $ Map.elems diags)

-- ---------------------------------------------------------------------

{- | Aggregate all diagnostics pertaining to a particular version of a document,
 by source, and sends a @textDocument/publishDiagnostics@ notification with
 the total (limited by the first parameter) whenever it is updated.
-}
publishDiagnostics ::
  DiagnosticsHandle ->
  Int ->
  NormalizedUri ->
  Maybe Int32 ->
  DiagnosticsBySource ->
  IO ()
publishDiagnostics handle maxDiagnosticCount uri version diags = join $ atomically $ stateTVar handle.diagnosticStore $ \oldDiags ->
  let !newDiags = updateDiagnostics oldDiags uri version diags
      mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
      act = case mdp of
        Nothing -> return ()
        Just params -> sendNotification handle.serverHandle.rpcHandle J.SMethod_TextDocumentPublishDiagnostics params
   in (act, newDiags)

-- ---------------------------------------------------------------------

{- | Remove all diagnostics from a particular source, and send the updates to
 the client.
-}
flushDiagnosticsBySource ::
  DiagnosticsHandle ->
  -- | Max number of diagnostics to send
  Int ->
  Maybe Text ->
  IO ()
flushDiagnosticsBySource handle maxDiagnosticCount msource = join $ atomically $ stateTVar handle.diagnosticStore $ \oldDiags ->
  let !newDiags = flushBySource oldDiags msource
      -- Send the updated diagnostics to the client
      act = forM_ (HM.keys newDiags) $ \uri -> do
        let mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
        case mdp of
          Nothing -> return ()
          Just params -> sendNotification handle.serverHandle.rpcHandle J.SMethod_TextDocumentPublishDiagnostics params
   in (act, newDiags)
