{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.TH.InitializeRequestJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.TH.InitializeRequestArgumentsJSON

-- |
--   Client-initiated request
--
data InitializeRequest =
  InitializeRequest {
    seqInitializeRequest       :: Int                         -- Sequence number
  , typeInitializeRequest      :: String                      -- One of "request", "response", or "event"
  , commandInitializeRequest   :: String                      -- The command to execute
  , argumentsInitializeRequest :: InitializeRequestArguments  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequest") } ''InitializeRequest)

defaultInitializeRequest :: InitializeRequest
defaultInitializeRequest = InitializeRequest 0 "" "" defaultInitializeRequestArguments
