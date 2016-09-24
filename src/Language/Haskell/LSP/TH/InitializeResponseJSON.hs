{-# LANGUAGE TemplateHaskell #-}


module Language.Haskell.LSP.TH.InitializeResponseJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.TH.InitializeResponseCapabilitiesJSON
import Language.Haskell.LSP.TH.InitializeRequestJSON

-- |
--   Server-initiated response to client request
--
data InitializeResponse =
  InitializeResponse {
    jsonrpcInitializeResponse    :: String
  , idInitializeResponse         :: Int     -- Sequence number
  , resultInitializeResponse        :: InitializeResponseCapabilites  -- The capabilities of this debug adapter
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponse") } ''InitializeResponse)


-- |
--
parseErrorInitializeResponse :: Int -> String -> InitializeResponse
parseErrorInitializeResponse seq msg =
  InitializeResponse  "2.0" seq defaultInitializeResponseCapabilites

-- |
--
errorInitializeResponse :: InitializeRequest -> String -> InitializeResponse
errorInitializeResponse (InitializeRequest reqSeq _ _) msg =
  InitializeResponse "2.0" reqSeq defaultInitializeResponseCapabilites

