{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Network.JsonRpc.Server
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.IO
import qualified Data.Text as T
import System.IO.Unsafe
import Control.Exception    (bracket)

main = do
  hSetBuffering stdout LineBuffering
  contents <- B.getContents
  logm contents
  count <- newMVar 0
  forM_ (B.lines contents) $ \request -> do
         response <- runReaderT (call methods request) count
         B.putStrLn $ fromMaybe "" response

type Server = ReaderT (MVar Integer) IO

methods :: [Method Server]
methods = [add, printSequence, increment]

add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = liftIO $ return (x + y)

printSequence = toMethod "print_sequence" f params
    where params = Required "string" :+:
                   Optional "count" 1 :+:
                   Optional "separator" ',' :+: ()
          f :: String -> Int -> Char -> RpcResult Server ()
          f str count sep = do
              when (count < 0) $ throwError negativeCount
              liftIO $ print $ intercalate [sep] $ replicate count str
          negativeCount = rpcError (-32000) "negative count"

increment = toMethod "increment_and_get_count" f ()
    where f :: RpcResult Server Integer
          f = ask >>= \count -> liftIO $ modifyMVar count inc
              where inc x = return (x + 1, x + 1)

-- ---------------------------------------------------------------------

logm str = do
  appendFileAndFlush "/tmp/hie-vscode.log" str

-- | Append a 'ByteString' to a file.
appendFileAndFlush :: FilePath -> B.ByteString -> IO ()
appendFileAndFlush f txt = bracket (openBinaryFile f AppendMode) hClose
    (\hdl -> B.hPut hdl txt >> hFlush hdl)

 
