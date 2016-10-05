{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Language.Haskell.LSP.Control where

import           Control.Concurrent
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ConfigFile as C
import           Data.Monoid
import qualified Language.Haskell.LSP.Argument as A
import qualified Language.Haskell.LSP.Core as GUI
import           Language.Haskell.LSP.Utility
import           System.IO
import           Text.Parsec

-- ---------------------------------------------------------------------
-- |
--  ロジックメイン
--
run :: GUI.Handlers
    -> GUI.Options
    -> IO Int         -- exit code
run h o = do

  logm $ B.pack "\n\n\n\n\nStarting up server ..."
  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8

  mvarDat <- newMVar $ (GUI.defaultLanguageContextData h o) {GUI.resSendResponse = sendResponse}

  wait mvarDat

  return 1

-- ---------------------------------------------------------------------
-- |
--
--
wait :: MVar GUI.LanguageContextData -> IO ()
wait mvarDat = go BSL.empty
  where
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet stdin 1
      let newBuf = BSL.append buf c
      case readContentLength (lbs2str newBuf) of
        Left _ -> go newBuf
        Right len -> do
          cnt <- BSL.hGet stdin len
          logm $ (B.pack "---> ") <> cnt
          GUI.handleRequest mvarDat newBuf cnt
          wait mvarDat

      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

-- ---------------------------------------------------------------------
-- |
--
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout
  logm $ B.pack "<--2--" <> str

-- |
--
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"



