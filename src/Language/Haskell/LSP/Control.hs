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
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Language.Haskell.LSP.Utility
import           System.IO
import           System.Log.Logger
import           Text.Parsec

-- ---------------------------------------------------------------------

run :: IO () -- ^ function to be called once initialize has been received from the client
    -> GUI.Handlers 
    -> GUI.Options
    -> IO Int         -- exit code
run dp h o = do

  GUI.setupLogger "/tmp/hie-vscode.log" DEBUG

  logm $ B.pack "\n\n\n\n\nStarting up server ..."
  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8


  mvarDat <- newMVar ((GUI.defaultLanguageContextData h o :: GUI.LanguageContextData)
                         { GUI.resSendResponse = sendResponse
                         } )

  ioLoop dp mvarDat

  return 1

-- ---------------------------------------------------------------------

ioLoop :: IO () -> MVar GUI.LanguageContextData -> IO ()
ioLoop dispatcherProc mvarDat = go BSL.empty
  where
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet stdin 1
      -- logs $ "ioLoop: got" ++ show c
      let newBuf = BSL.append buf c
      case readContentLength (lbs2str newBuf) of
        Left _ -> go newBuf
        Right len -> do
          cnt <- BSL.hGet stdin len
          logm $ (B.pack "---> ") <> cnt
          GUI.handleRequest dispatcherProc mvarDat newBuf cnt
          ioLoop dispatcherProc mvarDat

      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          _ <- string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

-- ---------------------------------------------------------------------

sendNotificationMessage :: (J.ToJSON a) => J.NotificationMessage a -> IO ()
sendNotificationMessage res = sendResponse (J.encode res)

-- ---------------------------------------------------------------------

sendRequestMessage :: (J.ToJSON a) => J.RequestMessage a -> IO ()
sendRequestMessage res = sendResponse (J.encode res)

-- ---------------------------------------------------------------------

sendResponseMessage :: (J.ToJSON a) => J.ResponseMessage a -> IO ()
sendResponseMessage res = sendResponse (J.encode res)

-- ---------------------------------------------------------------------

-- | Send a message with the required JSON 2.0 Content-Length header
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout
  logm $ B.pack "<--2--" <> str

_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"



