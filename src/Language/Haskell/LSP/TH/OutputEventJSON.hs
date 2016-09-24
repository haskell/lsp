{-# LANGUAGE TemplateHaskell     #-}

module Language.Haskell.LSP.TH.OutputEventJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.TH.OutputEventBodyJSON

-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEvent =
  OutputEvent {
    seqOutputEvent   :: Int     -- Sequence number
  , typeOutputEvent  :: String  -- One of "request", "response", or "event"
  , eventOutputEvent :: String  -- Type of event
  , bodyOutputEvent  :: OutputEventBody 
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "OutputEvent") } ''OutputEvent)

defaultOutputEvent :: Int -> OutputEvent
defaultOutputEvent resSeq = OutputEvent resSeq "event" "output" defaultOutputEventBody

