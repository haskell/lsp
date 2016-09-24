{-# LANGUAGE TemplateHaskell #-}


module Language.Haskell.LSP.TH.InitializeRequestArgumentsJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Initialize request; value of command field is "initialize".
--
data InitializeRequestArguments =
  InitializeRequestArguments {
    adapterIDInitializeRequestArguments       :: String  -- The ID of the debugger adapter. Used to select or verify debugger adapter.
  , linesStartAt1InitializeRequestArguments   :: Bool    -- If true all line numbers are 1-based (default).
  , columnsStartAt1InitializeRequestArguments :: Bool    -- If true all column numbers are 1-based (default).
  , pathFormatInitializeRequestArguments      :: String  -- Determines in what format paths are specified. Possible values are 'path' or 'uri'. The default is 'path', which is the native format.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequestArguments") } ''InitializeRequestArguments)

defaultInitializeRequestArguments :: InitializeRequestArguments
defaultInitializeRequestArguments = InitializeRequestArguments "" False False ""
