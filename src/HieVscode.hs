{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The structure of this server is based on the one in
-- https://hackage.haskell.org/package/phoityne-vscode

module Main (main) where

import qualified Control.Exception as E
import           Data.Aeson
import           Data.Default
import           System.Exit
import qualified System.Log.Logger as L
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
--
main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: IO Int
run = flip E.catches handlers $ do

  flip E.finally finalProc $ do
    CTRL.run (return Nothing) hieHandlers hieOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------
-- TODO: temporary here, move to hie

hieOptions :: GUI.Options
hieOptions = def

hieHandlers :: GUI.Handlers
hieHandlers = def {GUI.renameHandler = Just renameRequestHandler }

renameRequestHandler :: GUI.Handler J.RenameRequest
renameRequestHandler sf (J.RequestMessage _ origId _ _) = do
  let loc = def :: J.Location
      res  = GUI.makeResponseMessage (J.responseId origId) loc
  sf (encode res)

-- ---------------------------------------------------------------------
