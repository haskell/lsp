{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Control
  (
    run
  , sendNotificationMessage
  , sendRequestMessage
  , sendResponseMessage
  ) where

import           Control.Concurrent
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid
import qualified Language.Haskell.LSP.Core as GUI
import qualified Language.Haskell.LSP.TH.ClientCapabilities as J
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J
import           Language.Haskell.LSP.Utility
import           System.IO
import           Text.Parsec

-- ---------------------------------------------------------------------

run :: (J.ClientCapabilities -> IO (Maybe J.ResponseError))
                                       -- ^ function to be called once
                                       -- initialize has been received from the
                                       -- client. Further message processing
                                       -- will start only after this returns.
    -> GUI.Handlers
    -> GUI.Options
    -> IO Int         -- exit code
run dp h o = do

  logm $ B.pack "\n\n\n\n\nStarting up server ..."
  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8


  mvarDat <- newMVar ((GUI.defaultLanguageContextData h o :: GUI.LanguageContextData)
                         { GUI.resSendResponse = GUI.sendResponse
                         } )

  ioLoop dp mvarDat

  return 1

-- ---------------------------------------------------------------------

ioLoop :: (J.ClientCapabilities -> IO (Maybe J.ResponseError)) -> MVar GUI.LanguageContextData -> IO ()
ioLoop dispatcherProc mvarDat = go BSL.empty
  where
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet stdin 1
      if c == BSL.empty
        then do
          logm $ B.pack "\nGot EOF, exiting 1 ...\n"
          return ()
        else do
          -- logs $ "ioLoop: got" ++ show c
          let newBuf = BSL.append buf c
          case readContentLength (lbs2str newBuf) of
            Left _ -> go newBuf
            Right len -> do
              cnt <- BSL.hGet stdin len
              if cnt == BSL.empty
                then do
                  logm $ B.pack "\nGot EOF, exiting 1 ...\n"
                  return ()
                else do
                  logm $ (B.pack "---> ") <> cnt
                  GUI.handleRequest dispatcherProc mvarDat newBuf cnt
                  ioLoop dispatcherProc mvarDat
      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          _ <- string "Content-Length: "
          len <- manyTill digit (string GUI._TWO_CRLF)
          return . read $ len

-- ---------------------------------------------------------------------

sendNotificationMessage :: (J.ToJSON a) => J.NotificationMessage a -> IO ()
sendNotificationMessage res = GUI.sendResponse (J.encode res)

-- ---------------------------------------------------------------------

sendRequestMessage :: (J.ToJSON a) => J.RequestMessage a -> IO ()
sendRequestMessage res = GUI.sendResponse (J.encode res)

-- ---------------------------------------------------------------------

sendResponseMessage :: (J.ToJSON a) => J.ResponseMessage a -> IO ()
sendResponseMessage res = GUI.sendResponse (J.encode res)



