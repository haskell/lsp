{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Manage the "textDocument/publishDiagnostics" notifications to keep a local copy of the
diagnostics for a particular file and version, partitioned by source.
-}
module Language.Haskell.LSP.Diagnostics
  (
    DiagnosticStore
  , DiagnosticsBySource
  , StoreItem(..)
  , updateDiagnostics

  -- * for tests
  ) where

import qualified Data.Map as Map
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

{-
We need a three level store

  Uri : Maybe TextDocumentVersion : Maybe DiagnosticSource : [Diagnostics]

For a given Uri, as soon as we see a new (Maybe TextDocumentVersion) we flush
all prior entries for the Uri.

-}

type DiagnosticStore = Map.Map J.Uri StoreItem

data StoreItem
  = StoreItem (Maybe J.TextDocumentVersion) DiagnosticsBySource
  deriving (Show,Eq)

type DiagnosticsBySource = Map.Map (Maybe J.DiagnosticSource) [J.Diagnostic]

-- ---------------------------------------------------------------------

updateDiagnostics :: DiagnosticStore
                  -> J.Uri -> Maybe J.TextDocumentVersion -> [J.Diagnostic]
                  -> DiagnosticStore
updateDiagnostics store uri mv diags = r
  where
    newStore :: DiagnosticStore
    newStore = Map.insert uri (StoreItem mv newDiagsBySource) store

    newDiagsBySource :: DiagnosticsBySource
    newDiagsBySource = Map.fromListWith (++) $ map (\d -> (J._source d, [d])) diags

    updateDbs dbs = Map.insert uri new store
      where
        new = StoreItem mv newDbs
        -- note: Map.union is left-biased, so for identical keys the first
        -- argument is used
        newDbs = Map.union newDiagsBySource dbs

    r = case Map.lookup uri store of
      Nothing -> newStore
      Just (StoreItem mvs dbs) ->
        if mvs /= mv
          then newStore
          else updateDbs dbs

-- ---------------------------------------------------------------------
