{-# LANGUAGE TemplateHaskell     #-}

module Language.Haskell.LSP.TH.ExitNotificationJSON where

import Data.Aeson.TH

import Language.Haskell.LSP.Utility

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data ExitNotification =
  ExitNotification {
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExitNotification") } ''ExitNotification)

defaultExitNotification :: ExitNotification
defaultExitNotification = ExitNotification

