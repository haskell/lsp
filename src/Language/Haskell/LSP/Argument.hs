{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -fno-cse         #-}

module Language.Haskell.LSP.Argument (
  HelpExitException(..)
, ArgData(..)
, config
) where

-- システム
import System.Console.CmdArgs
import qualified Control.Exception as E

import Paths_haskell_lsp (version)
import Data.Version (showVersion)

-- |
--  ヘルプ表示時の例外
--
data HelpExitException = HelpExitException
                       deriving (Show, Typeable)

instance E.Exception HelpExitException

-- |
--  コマンドライン引数データ設定
--    モードを使用する場合のサンプル
--
data ArgData = ModeA deriving (Data, Typeable, Show, Read, Eq)

-- |
--  アノテーション設定
--
config :: ArgData
config = modes [confA]
         &= summary sumMsg
         &= program "phoityne-vscode"

  where
    confA = ModeA {

          } &= name "ModeA"
            &= details mdAMsg
            &= auto

    sumMsg = unlines [
             "phoityne-vscode-" ++ showVersion version
           ]

    mdAMsg = [
             ""
           , "Phoityne is a ghci debug viewer for Visual Studio Code. "
           , ""
           ]


-- |
--  enum値引数
--
data Method = Get | Post | Put | Delete
              deriving (Data, Typeable, Show, Read, Eq)
