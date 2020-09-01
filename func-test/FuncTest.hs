{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}
module Main where

import Data.Default
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Control
import qualified Language.Haskell.LSP.Test as Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Control.Monad.IO.Class
import System.IO
import Control.Concurrent
import Control.Monad
import System.Process
import Control.Applicative.Combinators
import Control.Monad.Trans.Control
import Control.Lens

main :: IO ()
main = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe
  
  killVar <- newEmptyMVar
  
  forkIO $ void $ runWithHandles hinRead houtWrite initCallbacks (handlers killVar) def
  
  Test.runSessionWithHandles hinWrite houtRead Test.defaultConfig Test.fullCaps "." $ do
    skipManyTill Test.anyMessage $ do
      x <- Test.message SProgress
      let isBegin (Begin _) = True
          isBegin _ = False
      guard $ isBegin $ x ^. params . value
    liftIO $ putMVar killVar ()
    skipManyTill Test.anyMessage $ do
      x <- Test.message SProgress
      let isEnd (End _) = True
          isEnd _ = False
      guard $ isEnd $ x ^. params . value
    liftIO $ putStrLn "Hello, Haskell!"

initCallbacks :: InitializeCallbacks ()
initCallbacks = InitializeCallbacks
  { onConfigurationChange = const $ pure $ Right ()
  , onInitialization = const $ pure Nothing
  }

handlers :: MVar () -> Handlers ()
handlers killVar SInitialized = Just $ \noti -> do
  tid <- liftBaseDiscard forkIO $
    withProgress "Doing something" NotCancellable $ \updater ->
      liftIO $ threadDelay (1 * 1000000)
  liftIO $ void $ forkIO $ do
    takeMVar killVar
    killThread tid

handlers _ _ = Nothing
