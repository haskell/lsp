{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Control
  (
    run
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.Utility
import           System.IO
import           Text.Parsec

-- ---------------------------------------------------------------------

run :: Core.InitializeCallback -- ^ function to be called once initialize has
                               -- been received from the client. Further message
                               -- processing will start only after this returns.
    -> Core.Handlers
    -> Core.Options
    -> IO Int         -- exit code
run dp h o = do

  logm $ B.pack "\n\n\n\n\nStarting up server ..."
  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8

  cout <- atomically newTChan :: IO (TChan BSL.ByteString)
  _rhpid <- forkIO $ sendServer cout

  let sendFunc str = atomically $ writeTChan cout str

  mvarDat <- newMVar ((Core.defaultLanguageContextData h o :: Core.LanguageContextData)
                         { Core.resSendResponse = sendFunc
                         } )

  ioLoop dp mvarDat

  return 1

-- ---------------------------------------------------------------------

ioLoop :: Core.InitializeCallback -> MVar Core.LanguageContextData -> IO ()
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
                  Core.handleRequest dispatcherProc mvarDat newBuf cnt
                  ioLoop dispatcherProc mvarDat
      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          _ <- string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

-- ---------------------------------------------------------------------

-- | Simple server to make sure all stdout is serialised
sendServer :: TChan BSL.ByteString -> IO ()
sendServer cstdout = do
  forever $ do
    str <- atomically $ readTChan cstdout
    let out = BSL.concat
                 [ str2lbs $ "Content-Length: " ++ show (BSL.length str)
                 , str2lbs _TWO_CRLF
                 , str ]

    BSL.hPut stdout out
    hFlush stdout
    logm $ B.pack "<--2--" <> str

-- |
--
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"


