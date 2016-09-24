{-# LANGUAGE TemplateHaskell #-}


module Language.Haskell.LSP.TH.ShutdownResponseJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.TH.ShutdownRequestJSON

-- |
--   Server-initiated response to client request
--
data ShutdownResponse =
  ShutdownResponse {
    jsonrpcShutdownResponse    :: String
  , idShutdownResponse         :: Int     -- Sequence number
  , resultShutdownResponse     :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ShutdownResponse") } ''ShutdownResponse)


-- |
--
parseErrorShutdownResponse :: Int -> String -> ShutdownResponse
parseErrorShutdownResponse seq msg =
  ShutdownResponse  "2.0" seq msg

-- |
--
errorShutdownResponse :: Int -> ShutdownRequest -> String -> ShutdownResponse
errorShutdownResponse seq (ShutdownRequest reqSeq) msg =
  ShutdownResponse "2.0" seq msg

