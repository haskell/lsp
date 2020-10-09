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
import Control.Lens hiding (List)
import Test.Hspec
import Data.Maybe
import Control.Concurrent.Async
import Control.Exception
import System.Exit

main :: IO ()
main = hspec $ do
  describe "progress reporting" $
    it "sends end notification if thread is killed" $ do
      (hinRead, hinWrite) <- createPipe
      (houtRead, houtWrite) <- createPipe
      
      killVar <- newEmptyMVar

      let initCallbacks = InitializeCallbacks
            { onConfigurationChange = const $ pure $ Right ()
            , doInitialize = const $ pure Nothing
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
      
      forkIO $ void $ runWithHandles hinRead houtWrite initCallbacks (handlers killVar) def
      
      Test.runSessionWithHandles hinWrite houtRead Test.defaultConfig Test.fullCaps "." $ do
        -- First make sure that we get a $/progress begin notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SProgress
          let isBegin (Begin _) = True
              isBegin _ = False
          guard $ isBegin $ x ^. params . value
          
        -- Then kill the thread
        liftIO $ putMVar killVar ()
        
        -- Then make sure we still get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SProgress
          let isEnd (End _) = True
              isEnd _ = False
          guard $ isEnd $ x ^. params . value

  describe "workspace folders" $
    it "keeps track of open workspace folders" $ do
      (hinRead, hinWrite) <- createPipe
      (houtRead, houtWrite) <- createPipe
      
      countVar <- newMVar 0

      let wf0 = WorkspaceFolder "one" "Starter workspace"
          wf1 = WorkspaceFolder "/foo/bar" "My workspace"
          wf2 = WorkspaceFolder "/foo/baz" "My other workspace"
          
          initCallbacks = InitializeCallbacks
            { onConfigurationChange = const $ pure $ Right ()
            , doInitialize = const $ pure Nothing
            }

          handlers :: Handlers ()
          handlers SInitialized = Just $ \noti -> do
            wfs <- fromJust <$> getWorkspaceFolders
            liftIO $ wfs `shouldContain` [wf0]
          handlers SWorkspaceDidChangeWorkspaceFolders = Just $ \noti -> do
            i <- liftIO $ modifyMVar countVar (\i -> pure (i + 1, i))
            wfs <- fromJust <$> getWorkspaceFolders
            liftIO $ case i of
              0 -> do
                wfs `shouldContain` [wf1]
                wfs `shouldContain` [wf0]
              1 -> do
                wfs `shouldNotContain` [wf1]
                wfs `shouldContain` [wf0]
                wfs `shouldContain` [wf2]
              _ -> error "Shouldn't be here"
          handlers _ = Nothing
                
      
      server <- async $ void $ runWithHandles hinRead houtWrite initCallbacks handlers def
      
      let config = Test.defaultConfig
            { Test.initialWorkspaceFolders = Just [wf0]
            }
            
          changeFolders add rmv =
            let addedFolders = List add
                removedFolders = List rmv
                ev = WorkspaceFoldersChangeEvent addedFolders removedFolders
                ps = DidChangeWorkspaceFoldersParams ev
            in Test.sendNotification SWorkspaceDidChangeWorkspaceFolders ps

      Test.runSessionWithHandles hinWrite houtRead config Test.fullCaps "." $ do
        changeFolders [wf1] []
        changeFolders [wf2] [wf1]

      Left e <- waitCatch server
      fromException e `shouldBe` Just ExitSuccess
      