{-# LANGUAGE CPP                 #-}
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
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.Utility
import           System.IO
import           System.Directory
import           Text.Parsec

-- ---------------------------------------------------------------------

run :: (Show c) => Core.InitializeCallback c
                                 -- ^ function to be called once initialize has
                                 -- been received from the client. Further message
                                 -- processing will start only after this returns.
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -- ^ File to record interaction with client to
    -> IO Int         -- exit code
run dp h o recFp = do

  logm $ B.pack "\n\n\n\n\nhaskell-lsp:Starting up server ..."
  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8

  cout <- atomically newTChan :: IO (TChan BSL.ByteString)
  _rhpid <- forkIO $ sendServer cout


  let sendFunc :: Core.SendFunc
      sendFunc str = atomically $ writeTChan cout (J.encode str)
  let lf = error "LifeCycle error, ClientCapabilities not set yet via initialize maessage"

  tvarId <- atomically $ newTVar 0

  tvarDat <- atomically $ newTVar $ Core.defaultLanguageContextData h o lf tvarId sendFunc

  maybe (return ()) removeFile recFp

  ioLoop dp tvarDat recFp

  return 1

-- ---------------------------------------------------------------------

ioLoop :: (Show c) => Core.InitializeCallback c -> TVar (Core.LanguageContextData c) -> Maybe FilePath -> IO ()
ioLoop dispatcherProc tvarDat recFp = go BSL.empty
  where
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet stdin 1

      record c

      if c == BSL.empty
        then do
          logm $ B.pack "\nhaskell-lsp:Got EOF, exiting 1 ...\n"
          return ()
        else do
          -- logs $ "ioLoop: got" ++ show c
          let newBuf = BSL.append buf c
          case readContentLength (lbs2str newBuf) of
            Left _ -> go newBuf
            Right len -> do
              cnt <- BSL.hGet stdin len

              record cnt
              
              if cnt == BSL.empty
                then do
                  logm $ B.pack "\nhaskell-lsp:Got EOF, exiting 1 ...\n"
                  return ()
                else do
                  logm $ (B.pack "---> ") <> cnt
                  Core.handleRequest dispatcherProc tvarDat newBuf cnt
                  ioLoop dispatcherProc tvarDat recFp
      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          _ <- string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

        record c = maybe (return ()) (flip BSL.appendFile c) recFp

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


