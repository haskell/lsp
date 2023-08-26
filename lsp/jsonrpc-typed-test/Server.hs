module Server where

import Colog.Core as L
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.ByteString
import Data.Either
import Data.Foldable
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Singletons
import JSONRPC.Message qualified as Untyped
import JSONRPC.RPC qualified as RPC
import JSONRPC.Server (runServerIn)
import JSONRPC.Server qualified as Untyped
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC qualified as TRPC
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Types (type (|?) (..))
import Prettyprinter
import System.Console.Concurrent
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import TestLib
import Prelude hiding (id)

adder :: ServerFunction Server TestMethod
adder _env =
  pure $
    ServerDefinition
      { initialHandlers =
          requestHandler SM1 (mkRequestHandler $ \p -> pure $ Right (p + 1))
      }

adderPlusNotify :: ServerFunction Server TestMethod
adderPlusNotify h =
  pure $
    ServerDefinition
      { initialHandlers =
          requestHandler SM1 $ mkRequestHandler $ \p -> do
            TRPC.sendNotification h.rpcHandle SM4 1
            pure $ Right (p + 1)
      }

adderPlusNewHandler :: ServerFunction Server TestMethod
adderPlusNewHandler h =
  pure $
    ServerDefinition
      { initialHandlers =
          requestHandler SM1 $ mkRequestHandler $ \p -> atomically $ do
            _ <- setRequestHandler h SM5 $ mkRequestHandler $ \p -> do
              pure $ Right (p - 1)
            -- Use the untyped API to register a handler just using the method string
            _ <- Untyped.setRequestHandler (untypedServerHandle h) "M6" $ Untyped.RequestHandler $ \msg -> do
              case msg ^. #params of
                Just p -> case J.fromJSON p of
                  J.Success (p :: Int) -> pure $ Untyped.ResponseMessage (InL $ msg ^. #id) (Just $ J.toJSON (p * 2)) Nothing
                  J.Error e -> error e
                Nothing -> error "no params"
            pure $ Right (p + 1)
      }

askNotify :: ServerFunction Server TestMethod
askNotify _env = do
  var <- newMVar 0
  pure $
    ServerDefinition
      { initialHandlers =
          ( requestHandler SM1 $ mkRequestHandler $ \_ -> do
              n <- liftIO $ readMVar var
              pure $ Right n
          )
            <> ( notificationHandler SM2 $ mkNotificationHandler $ \p -> do
                  void $ liftIO $ swapMVar var p
               )
      }

runServerClientPair :: forall k. Method k => ServerFunction Server k -> TRPC.RpcT Client k IO () -> IO ()
runServerClientPair server client = do
  (_var, logM) <- logMVar
  let
    prettyMsg :: Pretty msg => Role -> WithSeverity msg -> Doc ann
    prettyMsg r l = brackets (viaShow r) <+> brackets (viaShow (L.getSeverity l)) <+> pretty (L.getMsg l)
    loggerFor :: Pretty msg => SRole r -> LogAction IO (WithSeverity msg)
    loggerFor r = L.cmap (\msg -> prettyMsg (fromSing r) msg) logM

  serverOut <- atomically newTChan :: IO (TChan ByteString)
  serverIn <- atomically newTChan :: IO (TChan ByteString)
  let clientIn = serverOut
      clientOut = serverIn

  srpc <-
    RPC.initRpcWith
      (loggerFor SServer)
      (atomically $ readTChan serverIn)
      (atomically . writeTChan serverOut)
  let serverRun = runServerIn (loggerFor SServer) srpc (toUntypedServerFunction server)
  (env, rpcThreads) <-
    RPC.initRpcWith
      (loggerFor SClient)
      (atomically $ readTChan clientIn)
      (atomically . writeTChan clientOut)
  let clientRun = TRPC.runRpcT (TRPC.RpcHandle env) client `Async.race_` rpcThreads

  serverRun `Async.race_` clientRun

-- `finally`
-- printMVarLogs var

test_requestResponse :: TestTree
test_requestResponse = testCase "requestResponse" $
  withConcurrentOutput $
    runServerClientPair adder $
      withSingI SClient $ do
        h <- TRPC.getRpcHandle
        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
          liftIO $ resp @?= Right 2

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 3
          liftIO $ resp @?= Right 4

test_notifications :: TestTree
test_notifications = testCase "notifications" $
  withConcurrentOutput $
    runServerClientPair askNotify $
      withSingI SClient $ do
        h <- TRPC.getRpcHandle
        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
          liftIO $ resp @?= Right 0

        liftIO $ TRPC.sendNotification h SM2 1

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
          liftIO $ resp @?= Right 1

        liftIO $ TRPC.sendNotification h SM2 3

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
          liftIO $ resp @?= Right 3

test_interveningNotification :: TestTree
test_interveningNotification = testCase "interveningNotification" $
  withConcurrentOutput $
    runServerClientPair adderPlusNotify $ do
      h <- TRPC.getRpcHandle
      id1 <- liftIO $ TRPC.sendRequest h SM1 1
      notif <- liftIO $ TRPC.expectNotification h SM4
      liftIO $ notif ^. #params @?= 1
      resp <- liftIO $ TRPC.expectResponseFor h SM1 id1
      liftIO $ resp ^. #result @?= Right 2

prop_spam :: Property
prop_spam = withSingI SClient $ noShrinking $ property $ \(msgs :: [SomeMessage Client TestMethod]) -> ioProperty $ do
  runServerClientPair adder $ do
    h <- TRPC.getRpcHandle
    for_ @_ @_ @_ @() msgs $ \case
      (SomeMessage (Req s msg)) -> do
        -- Don't just send the message, it will have a
        -- generated ID and we won't recognize the response
        -- when it comes back!
        rid <- liftIO $ TRPC.sendRequest h s (msg ^. #params)
        void $ liftIO $ TRPC.expectResponseFor h s rid
      (SomeMessage (Not s msg)) ->
        liftIO $ TRPC.sendNotification h s (msg ^. #params)
      (SomeMessage (Rsp s msg)) ->
        liftIO $ TRPC.sendResponse h s (msg ^. #id) (msg ^. #result)

    -- Server should still be alive and respond properly
    do
      resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
      liftIO $ resp @?= Right 2

test_newHandler :: TestTree
test_newHandler = testCase "newHandler" $
  withConcurrentOutput $
    runServerClientPair adderPlusNewHandler $
      withSingI SClient $ do
        h <- TRPC.getRpcHandle
        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM5 3
          liftIO $ assertBool "Request unexpectedly succeeded" $ isLeft resp

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM1 1
          liftIO $ resp @?= Right 2

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM5 3
          liftIO $ resp @?= Right 2

        do
          resp <- liftIO $ TRPC.requestExpectResponse h SM6 3
          liftIO $ resp @?= Right 6
