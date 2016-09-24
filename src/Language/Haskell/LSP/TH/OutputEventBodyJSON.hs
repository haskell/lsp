{-# LANGUAGE TemplateHaskell     #-}


module Language.Haskell.LSP.TH.OutputEventBodyJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEventBody =
  OutputEventBody {
    categoryOutputEventBody :: String        -- The category of output (such as: 'console', 'stdout', 'stderr', 'telemetry'). If not specified, 'console' is assumed. 
  , outputOutputEventBody   :: String        -- The output to report.
  , dataOutputEventBody     :: Maybe String  -- Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "OutputEventBody") } ''OutputEventBody)

defaultOutputEventBody :: OutputEventBody
defaultOutputEventBody = OutputEventBody "console" "" Nothing


