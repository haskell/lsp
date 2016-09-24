{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.TH.RequestJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Client-initiated request
--
data Request =
  Request {
    -- commandRequest   :: String    -- The command to execute
    methodRequest   :: String    -- The command to execute
  -- , argumentsRequest :: [String]  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)
