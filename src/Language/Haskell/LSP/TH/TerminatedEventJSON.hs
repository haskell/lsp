{-# LANGUAGE TemplateHaskell     #-}


module Language.Haskell.LSP.TH.TerminatedEventJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.TH.TerminatedEventBodyJSON

-- |
--   Event message for "terminated" event types.
--   The event indicates that debugging of the debuggee has terminated.
--
data TerminatedEvent =
  TerminatedEvent {
    seqTerminatedEvent   :: Int     -- Sequence number
  , typeTerminatedEvent  :: String  -- One of "request", "response", or "event"
  , eventTerminatedEvent :: String  -- Type of event
  , bodyTerminatedEvent  :: TerminatedEventBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "TerminatedEvent") } ''TerminatedEvent)

defaultTerminatedEvent :: Int -> TerminatedEvent
defaultTerminatedEvent seq = TerminatedEvent seq "event" "terminated" defaultTerminatedEventBody

