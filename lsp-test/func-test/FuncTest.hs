{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Colog.Core
import Colog.Core qualified as L
import Control.Applicative.Combinators
import Control.Concurrent.Extra (newBarrier, signalBarrier, waitBarrier)
import Control.Exception
import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (field')
import Data.Maybe
import Data.Proxy
import Data.Set qualified as Set
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message hiding (error)
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Test qualified as Test
import System.Exit
import System.Process
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent

runSessionWithServer ::
  LogAction IO (WithSeverity LspServerLog) ->
  ServerDefinition config ->
  Test.SessionConfig ->
  ClientCapabilities ->
  FilePath ->
  Test.Session a ->
  IO a
runSessionWithServer logger defn testConfig caps root session = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  server <- async $ void $ runServerWithHandles logger (L.hoistLogAction liftIO logger) hinRead houtWrite defn

  res <- Test.runSessionWithHandles hinWrite houtRead testConfig caps root session

  timeout 3000000 $ do
    Left (fromException -> Just ExitSuccess) <- waitCatch server
    pure ()

  pure res

spec :: Spec
spec = do
  let logger = L.cmap show L.logStringStderr
  describe "server-initiated progress reporting" $ do
    it "sends updates" $ do
      startBarrier <- newBarrier
      b1 <- newBarrier
      b2 <- newBarrier
      b3 <- newBarrier

      let definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: Handlers (LspM ())
          handlers =
            requestHandler (SMethod_CustomMethod (Proxy @"something")) $ \_req resp -> void $ forkIO $ do
              withProgress "Doing something" Nothing NotCancellable $ \updater -> do
                liftIO $ waitBarrier startBarrier
                updater $ ProgressAmount (Just 25) (Just "step1")
                liftIO $ waitBarrier b1
                updater $ ProgressAmount (Just 50) (Just "step2")
                liftIO $ waitBarrier b2
                updater $ ProgressAmount (Just 75) (Just "step3")
                liftIO $ waitBarrier b3

      runSessionWithServer logger definition Test.defaultConfig Test.fullLatestClientCaps "." $ do
        Test.sendRequest (SMethod_CustomMethod (Proxy @"something")) J.Null

        -- Wait until we have seen a begin messsage. This means that the token setup
        -- has happened and the server has been able to send us a begin message
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressBegin) x

        -- allow the hander to send us updates
        liftIO $ signalBarrier startBarrier ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step1")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 25)
        liftIO $ signalBarrier b1 ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step2")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 50)
        liftIO $ signalBarrier b2 ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step3")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 75)
        liftIO $ signalBarrier b3 ()

        -- Then make sure we get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressEnd) x

    it "handles cancellation" $ do
      wasCancelled <- newMVar False

      let definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: Handlers (LspM ())
          handlers =
            requestHandler (SMethod_CustomMethod (Proxy @"something")) $ \_req resp -> void $ forkIO $ do
              -- Doesn't matter what cancellability we set here!
              withProgress "Doing something" Nothing NotCancellable $ \updater -> do
                -- Wait around to be cancelled, set the MVar only if we are
                liftIO $ threadDelay (5 * 1000000) `Control.Exception.catch` (\(e :: ProgressCancelledException) -> modifyMVar_ wasCancelled (\_ -> pure True))

      runSessionWithServer logger definition Test.defaultConfig Test.fullLatestClientCaps "." $ do
        Test.sendRequest (SMethod_CustomMethod (Proxy @"something")) J.Null

        -- Wait until we have created the progress so the updates will be sent individually
        token <- skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_WindowWorkDoneProgressCreate
          pure $ x ^. field' @"params" . #token

        -- First make sure that we get a $/progress begin notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressBegin) x

        Test.sendNotification SMethod_WindowWorkDoneProgressCancel (WorkDoneProgressCancelParams token)

        -- Then make sure we still get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressEnd) x

      c <- readMVar wasCancelled
      c `shouldBe` True

    it "sends end notification if thread is killed" $ do
      killVar <- newEmptyMVar

      let definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers killVar
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: MVar () -> Handlers (LspM ())
          handlers killVar =
            notificationHandler SMethod_Initialized $ \noti -> void $
              forkIO $
                withProgress "Doing something" Nothing NotCancellable $ \updater -> liftIO $ do
                  takeMVar killVar
                  Control.Exception.throwIO AsyncCancelled

      runSessionWithServer logger definition Test.defaultConfig Test.fullLatestClientCaps "." $ do
        -- First make sure that we get a $/progress begin notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressBegin) x

        -- Then kill the thread
        liftIO $ putMVar killVar ()

        -- Then make sure we still get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressEnd) x

  describe "client-initiated progress reporting" $ do
    it "sends updates" $ do
      startBarrier <- newBarrier
      b1 <- newBarrier
      b2 <- newBarrier
      b3 <- newBarrier

      let definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions{optSupportClientInitiatedProgress = True}
              }

          handlers :: Handlers (LspM ())
          handlers =
            requestHandler SMethod_TextDocumentCodeLens $ \req resp -> void $ forkIO $ do
              withProgress "Doing something" (req ^. field' @"params" . #workDoneToken) NotCancellable $ \updater -> do
                liftIO $ waitBarrier startBarrier
                updater $ ProgressAmount (Just 25) (Just "step1")
                liftIO $ waitBarrier b1
                updater $ ProgressAmount (Just 50) (Just "step2")
                liftIO $ waitBarrier b2
                updater $ ProgressAmount (Just 75) (Just "step3")
                liftIO $ waitBarrier b3

      runSessionWithServer logger definition Test.defaultConfig Test.fullLatestClientCaps "." $ do
        Test.sendRequest SMethod_TextDocumentCodeLens (CodeLensParams (Just $ ProgressToken $ InR "hello") Nothing (TextDocumentIdentifier $ Uri "."))

        -- First make sure that we get a $/progress begin notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressBegin) x

        liftIO $ signalBarrier startBarrier ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step1")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 25)
        liftIO $ signalBarrier b1 ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step2")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 50)
        liftIO $ signalBarrier b2 ()

        do
          u <- Test.message SMethod_Progress
          liftIO $ do
            u ^? field' @"params" . #value . workDoneProgressReport . #message `shouldBe` Just (Just "step3")
            u ^? field' @"params" . #value . workDoneProgressReport . #percentage `shouldBe` Just (Just 75)
        liftIO $ signalBarrier b3 ()

        -- Then make sure we get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (field' @"params" . #value . workDoneProgressEnd) x

  describe "workspace folders" $
    it "keeps track of open workspace folders" $ do
      countVar <- newMVar 0

      let wf0 = WorkspaceFolder (filePathToUri "one") "Starter workspace"
          wf1 = WorkspaceFolder (filePathToUri "/foo/bar") "My workspace"
          wf2 = WorkspaceFolder (filePathToUri "/foo/baz") "My other workspace"

          definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: Handlers (LspM ())
          handlers =
            mconcat
              [ notificationHandler SMethod_Initialized $ \noti -> do
                  wfs <- fromJust <$> getWorkspaceFolders
                  liftIO $ wfs `shouldContain` [wf0]
              , notificationHandler SMethod_WorkspaceDidChangeWorkspaceFolders $ \noti -> do
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
              ]

      let config = Test.defaultConfig{Test.initialWorkspaceFolders = Just [wf0]}

          changeFolders add rmv =
            let ev = WorkspaceFoldersChangeEvent add rmv
                ps = DidChangeWorkspaceFoldersParams ev
             in Test.sendNotification SMethod_WorkspaceDidChangeWorkspaceFolders ps

      runSessionWithServer logger definition config Test.fullLatestClientCaps "." $ do
        changeFolders [wf1] []
        changeFolders [wf2] [wf1]

main :: IO ()
main = hspec spec
