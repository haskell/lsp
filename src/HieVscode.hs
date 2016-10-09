{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The structure of this server is based on the one in
-- https://hackage.haskell.org/package/phoityne-vscode

module Main (main) where

import qualified Control.Exception as E
import qualified Data.ConfigFile as C
import           Data.Default
import           Data.Either.Utils
import qualified System.Console.CmdArgs as CMD
import           System.Exit
import qualified System.Log.Logger as L

import qualified Language.Haskell.LSP.Argument as A
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: IO Int
run = flip E.catches handlers $ do

  -- コマンドライン引数設定
  args <- getArgs --

  -- INI設定ファイルのRead
  iniSet <- loadIniFile args

  flip E.finally finalProc $ do
    CTRL.run () hieHandlers hieOptions

  where
    handlers = [ E.Handler helpExcept
               , E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    helpExcept (_ :: A.HelpExitException) = return 0
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------
-- TODO: temporary here, move to hie

hieOptions :: GUI.Options
hieOptions = def

hieHandlers :: GUI.Handlers a
hieHandlers = def {GUI.renameHandler = Just renameRequestHandler }

renameRequestHandler :: a -> J.RenameRequest -> IO J.RenameResponse
renameRequestHandler _ (J.RequestMessage _ origId _ _) = do
  let loc = def :: J.Location
      res  = GUI.makeResponseMessage origId loc
  return res

-- ---------------------------------------------------------------------

-- |
--  引数データの取得
--
getArgs :: IO A.ArgData
getArgs = E.catches (CMD.cmdArgs A.config) handlers
  where
    handlers = [E.Handler someExcept]
    someExcept (e :: E.SomeException) = if
      | "ExitSuccess" == show e -> E.throw A.HelpExitException
      | otherwise -> E.throwIO e


-- ---------------------------------------------------------------------
-- |
--  INI設定ファイルデータの取得
--
loadIniFile :: A.ArgData          -- コマンドライン引数
            -> IO C.ConfigParser  -- INI設定
loadIniFile _ = do
  let cp = forceEither $ C.readstring C.emptyCP defaultIniSetting
  return cp{ C.accessfunc = C.interpolatingAccess 5 }


-- ---------------------------------------------------------------------
-- |
-- デフォルトINI設定
--
defaultIniSetting :: String
defaultIniSetting = unlines [
    "[DEFAULT]"
  , "work_dir = ./"
  , ""
  , "[LOG]"
  , "file  = %(work_dir)sphoityne.log"
  , "level = WARNING"
  , ""
  , "[PHOITYNE]"
  ]

-- ---------------------------------------------------------------------
