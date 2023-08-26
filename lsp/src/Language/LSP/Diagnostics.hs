{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Manage the "textDocument/publishDiagnostics" notifications to keep a local copy of the
diagnostics for a particular file and version, partitioned by source.
-}
module Language.LSP.Diagnostics (
  DiagnosticStore,
  DiagnosticsBySource,
  StoreItem (..),
  partitionBySource,
  flushBySource,
  updateDiagnostics,
  getDiagnosticParamsFor,

  -- * for tests
) where

import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.SortedList qualified as SL
import Data.Text (Text)
import Language.LSP.Protocol.Types qualified as J

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

{-
We need a three level store

  Uri : Maybe Int32 : Maybe DiagnosticSource : [Diagnostics]

For a given Uri, as soon as we see a new (Maybe Int32) we flush
all prior entries for the Uri.

-}

type DiagnosticStore = HM.HashMap J.NormalizedUri StoreItem

data StoreItem
  = StoreItem (Maybe J.Int32) DiagnosticsBySource
  deriving (Show, Eq)

type DiagnosticsBySource = Map.Map (Maybe Text) (SL.SortedList J.Diagnostic)

-- ---------------------------------------------------------------------

partitionBySource :: [J.Diagnostic] -> DiagnosticsBySource
partitionBySource diags = Map.fromListWith mappend $ map (\d -> (J._source d, (SL.singleton d))) diags

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

  updateDbs dbs = HM.insert uri new store
   where
    new = StoreItem mv newDbs
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
