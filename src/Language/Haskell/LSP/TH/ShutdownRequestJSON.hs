{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.TH.ShutdownRequestJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Client-initiated request
--

data ShutdownRequest =
  ShutdownRequest {
    idShutdownRequest       :: Int                         -- Sequence number
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownRequest") } ''ShutdownRequest)

defaultShutdownRequest :: ShutdownRequest
defaultShutdownRequest = ShutdownRequest 0
