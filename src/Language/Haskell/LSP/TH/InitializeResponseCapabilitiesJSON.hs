{-# LANGUAGE TemplateHaskell #-}


module Language.Haskell.LSP.TH.InitializeResponseCapabilitiesJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilitiesInner =
  InitializeResponseCapabilitiesInner {
    definitionProviderInitializeResponseCapabilitiesInner :: Bool
  , renameProviderInitializeResponseCapabilitiesInner     :: Bool
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilitiesInner") } ''InitializeResponseCapabilitiesInner)

-- |
--
defaultInitializeResponseCapabilitiesInner :: InitializeResponseCapabilitiesInner
defaultInitializeResponseCapabilitiesInner = InitializeResponseCapabilitiesInner True True

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    capabilitiesInitializeResponseCapabilities :: InitializeResponseCapabilitiesInner
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilities") } ''InitializeResponseCapabilities)

-- |
--
defaultInitializeResponseCapabilities :: InitializeResponseCapabilities
defaultInitializeResponseCapabilities = InitializeResponseCapabilities defaultInitializeResponseCapabilitiesInner

