{-# LANGUAGE TemplateHaskell #-}


module Language.Haskell.LSP.TH.InitializeRequestArgumentsJSON where

import Data.Aeson.TH
import qualified Data.Aeson as A
import Data.Monoid

import Language.Haskell.LSP.Utility

-- |
--   Initialize request; value of command field is "initialize".
--
data InitializeRequestArguments =
  InitializeRequestArguments {
    processIdInitializeRequestArguments    :: Int
  , capabilitiesInitializeRequestArguments :: A.Object -- None currently defined, but empty object sent
  , traceInitializeRequestArguments        :: String
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequestArguments") } ''InitializeRequestArguments)

defaultInitializeRequestArguments :: InitializeRequestArguments
defaultInitializeRequestArguments = InitializeRequestArguments 0 mempty mempty
