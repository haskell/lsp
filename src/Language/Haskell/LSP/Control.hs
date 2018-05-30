{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Control
  (
    run
  , runWithHandles
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Language.Haskell.LSP.Capture
import qualified Language.Haskell.LSP.Core as Core
import           Language.Haskell.LSP.Utility
import           System.IO
import           System.IO.Error
import           System.Directory
import           Text.Parsec

-- ---------------------------------------------------------------------

-- | Convenience function for 'runWithHandles stdin stdout'.
run :: (Show c) => Core.InitializeCallback c
                -- ^ function to be called once initialize has
                -- been received from the client. Further message
                -- processing will start only after this returns.
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -- ^ File to capture the session to.
    -> IO Int
run = runWithHandles stdin stdout

-- | Starts listening and sending requests and responses
-- at the specified handles.
runWithHandles :: (Show c) =>
       Handle
    -- ^ Handle to read client input from.
    -> Handle
    -- ^ Handle to write output to.
    -> Core.InitializeCallback c
    -> Core.Handlers
    -> Core.Options
    -> Maybe FilePath
    -> IO Int         -- exit code
runWithHandles hin hout dp h o captureFp = do

  logm $ B.pack "\n\n\n\n\nhaskell-lsp:Starting up server ..."
  hSetBuffering hin NoBuffering
  hSetEncoding  hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding  hout utf8

  -- Delete existing recordings if they exist
  maybe (return ()) removeFileIfExists captureFp


  cout <- atomically newTChan :: IO (TChan Core.OutMessage)
  _rhpid <- forkIO $ sendServer cout hout captureFp


  let sendFunc :: Core.SendFunc
      sendFunc msg = atomically $ writeTChan cout msg
  let lf = error "LifeCycle error, ClientCapabilities not set yet via initialize maessage"

  tvarId <- atomically $ newTVar 0

  tvarDat <- atomically $ newTVar $ Core.defaultLanguageContextData h o lf tvarId sendFunc captureFp

  ioLoop hin dp tvarDat

  return 1

  where
    removeFileIfExists f = removeFile f `catch` handleDoesNotExist
    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e


-- ---------------------------------------------------------------------

ioLoop :: (Show c) => Handle
                   -> Core.InitializeCallback c
                   -> TVar (Core.LanguageContextData c)
                   -> IO ()
ioLoop hin dispatcherProc tvarDat = go BSL.empty
  where
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet hin 1

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
              cnt <- BSL.hGet hin len

              if cnt == BSL.empty
                then do
                  logm $ B.pack "\nhaskell-lsp:Got EOF, exiting 1 ...\n"
                  return ()
                else do
                  logm $ B.pack "---> " <> cnt
                  Core.handleRequest dispatcherProc tvarDat newBuf cnt
                  ioLoop hin dispatcherProc tvarDat
      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"

        parser = do
          _ <- string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

-- ---------------------------------------------------------------------

-- | Simple server to make sure all output is serialised
sendServer :: TChan Core.OutMessage -> Handle -> Maybe FilePath -> IO ()
sendServer msgChan clientH captureFp =
  forever $ do
    msg <- atomically $ readTChan msgChan

    let str = J.encode msg

    let out = BSL.concat
                 [ str2lbs $ "Content-Length: " ++ show (BSL.length str)
                 , str2lbs _TWO_CRLF
                 , str ]

    BSL.hPut clientH out
    hFlush clientH
    logm $ B.pack "<--2--" <> str
    
    mapM_ (captureFromServer msg) captureFp

-- |
--
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"
