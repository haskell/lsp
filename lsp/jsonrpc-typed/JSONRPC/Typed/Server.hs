module JSONRPC.Typed.Server where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson qualified as J
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Monoid (Ap (..))
import Data.Singletons
import Data.String (IsString (..))
import GHC.Records
import JSONRPC.Id
import JSONRPC.Message qualified as Untyped
import JSONRPC.RPC qualified as URPC
import JSONRPC.Server.Core qualified as Untyped
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC qualified as RPC
import Language.LSP.Protocol.Types (type (|?) (InL))
import Prelude hiding (id)
import GHC.Generics

newtype ServerHandle r k = ServerHandle {untypedServerHandle :: Untyped.ServerHandle}

instance HasField "rpcHandle" (ServerHandle r k) (RPC.RpcHandle r k) where
  getField (ServerHandle sh) = RPC.RpcHandle (Untyped.rpcHandle sh)

class (Untyped.MonadServer m) => MonadServer r k m | m -> r k

newtype ServerT r k m a = ServerT (Untyped.ServerT m a)
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix, URPC.MonadRpc, Untyped.MonadServer)
  deriving anyclass (MonadServer r k, RPC.MonadRpc r k)
  deriving (Semigroup, Monoid) via (Ap (ServerT r k m) a)

-- for deriving the instance of MonadUnliftIO
type role ServerT nominal nominal nominal nominal

runServerT :: ServerHandle r k -> ServerT r k m a -> m a
runServerT (ServerHandle env) (ServerT ma) = Untyped.runServerT env ma
{-# INLINE runServerT #-}

getServerHandle :: MonadServer r k m => m (ServerHandle r k)
getServerHandle = ServerHandle <$> Untyped.getServerHandle

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

data RequestHandler r k (m :: k) where
  RequestHandler :: (RequestOk (OtherRole r) m) => { handleRequest :: RequestMessage m -> IO (ResponseMessage m) } -> RequestHandler r k m

mkRequestHandler ::
  forall r k (m :: k).
  (Method k, RequestOk (OtherRole r) m) =>
  (MethodParams m -> IO (Either (ResponseError m) (MethodResult m))) ->
  RequestHandler r k m
mkRequestHandler f = RequestHandler $ \msg -> ResponseMessage (msg ^. #id) <$> f (msg ^. #params)

toUntypedReqHandler :: forall r k (m :: k). (Method k) => Sing m -> RequestHandler r k m -> Untyped.RequestHandler
toUntypedReqHandler m h = Untyped.RequestHandler $ \msg ->
  case fromUntypedReq msg >>= assertReqType m of
    Left e -> pure $ Untyped.ResponseMessage (InL $ msg ^. #id) Nothing (Just (Untyped.ResponseError (-32602) (fromString e) Nothing))
    Right tmsg -> toUntypedRsp m <$> handleRequest h tmsg

data NotificationHandler r k (m :: k) where
  NotificationHandler :: (NotificationOk (OtherRole r) m) => { handleNotification :: NotificationMessage m -> IO () } -> NotificationHandler r k m

instance Semigroup (NotificationHandler r k m) where
  (NotificationHandler h1) <> (NotificationHandler h2) = NotificationHandler $ \msg -> h1 msg <> h2 msg

instance (NotificationOk (OtherRole r) m) => Monoid (NotificationHandler r k m) where
  mempty = NotificationHandler $ const mempty

mkNotificationHandler ::
  forall r k (m :: k).
  (Method k, NotificationOk (OtherRole r) m) =>
  (MethodParams m -> IO ()) ->
  NotificationHandler r k m
mkNotificationHandler f = NotificationHandler $ \msg -> f (msg ^. #params)

toUntypedNotHandler :: forall r k (m :: k). (Method k) => Sing m -> NotificationHandler r k m -> Untyped.NotificationHandler
toUntypedNotHandler m h = Untyped.NotificationHandler $ \msg ->
  case fromUntypedNot msg >>= assertNotType m of
    -- TODO: this error gets swallowed, which is bad, since it includes the error from parsing the params
    Left _e -> pure ()
    Right tmsg -> handleNotification h tmsg

data ResponseHandler (r :: Role) k (m :: k) where
  ResponseHandler :: (RequestOk r m) => { handleResponse :: ResponseMessage m -> IO () } -> ResponseHandler r k m

mkResponseHandler ::
  forall r k (m :: k).
  (Method k, RequestOk r m) =>
  (Either (ResponseError m) (MethodResult m) -> IO ()) ->
  ResponseHandler r k m
mkResponseHandler f = ResponseHandler $ \msg -> f (msg ^. #result)

toUntypedRspHandler :: forall r k (m :: k). (Method k) => Sing m -> ResponseHandler r k m -> Untyped.ResponseHandler
toUntypedRspHandler m h = Untyped.ResponseHandler $ \msg -> do
  case fromKnownUntypedRsp @k m msg of
    Left e -> error e
    Right tmsg -> handleResponse h tmsg

newtype Handlers r k = Handlers { untypedHandlers :: Untyped.Handlers }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

notificationHandler :: forall r k (m :: k). (Method k) => Sing m -> NotificationHandler r k m -> Handlers r k
notificationHandler m h = Handlers $ Untyped.Handlers mempty (Map.singleton (toUntypedMethod $ fromSing m) (toUntypedNotHandler m h))

requestHandler :: forall r k (m :: k). (Method k) => Sing m -> RequestHandler r k m -> Handlers r k
requestHandler m h = Handlers $ Untyped.Handlers (Map.singleton (toUntypedMethod $ fromSing m) (toUntypedReqHandler m h)) mempty

data ServerDefinition r k = ServerDefinition {initialHandlers :: Handlers r k}

type ServerFunction r k = ServerHandle r k -> IO (ServerDefinition r k)

toUntypedServerDefinition :: ServerDefinition r k -> Untyped.ServerDefinition
toUntypedServerDefinition ServerDefinition{initialHandlers = Handlers h} = Untyped.ServerDefinition h

toUntypedServerFunction :: ServerFunction r k -> Untyped.ServerFunction
toUntypedServerFunction f = fmap toUntypedServerDefinition . f . ServerHandle

{- | Add a request handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
setRequestHandler ::
  forall r k meth.
  (Method k) =>
  ServerHandle r k ->
  Sing meth ->
  RequestHandler r k meth ->
  STM ()
setRequestHandler (ServerHandle serverHandle) s cb =
  Untyped.setRequestHandler serverHandle (toUntypedMethod (fromSing s)) (toUntypedReqHandler s cb)

{- | Add a notification handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
setNotificationHandler ::
  forall r k meth.
  (Method k) =>
  ServerHandle r k ->
  Sing meth ->
  NotificationHandler r k meth ->
  STM ()
setNotificationHandler (ServerHandle serverHandle) s cb =
  Untyped.setNotificationHandler serverHandle (toUntypedMethod (fromSing s)) (toUntypedNotHandler s cb)

addNotificationHandler ::
  forall r k meth.
  (Method k) =>
  ServerHandle r k ->
  Sing meth ->
  NotificationHandler r k meth ->
  STM ()
addNotificationHandler (ServerHandle serverHandle) s cb =
  Untyped.addNotificationHandler serverHandle (toUntypedMethod (fromSing s)) (toUntypedNotHandler s cb)

setHandlers ::
  forall r k.
  ServerHandle r k ->
  Handlers r k ->
  STM ()
setHandlers (ServerHandle serverHandle) (Handlers h) = Untyped.setHandlers serverHandle h

addHandlers ::
  forall r k.
  ServerHandle r k ->
  Handlers r k ->
  STM ()
addHandlers (ServerHandle serverHandle) (Handlers h) = Untyped.addHandlers serverHandle h

sendRequest ::
  forall r k (meth :: k).
  (Method k, RequestOk r meth) =>
  ServerHandle r k ->
  Sing meth ->
  MethodParams meth ->
  ResponseHandler r k meth ->
  IO Id
sendRequest (ServerHandle handle) m p h =
  bring @_ @IsJsonParts (Proxy @k) m $
    Untyped.sendRequest handle (toUntypedMethod $ fromSing m) (Just $ J.toJSON p) (toUntypedRspHandler m h)

-- | Send a request and wait synchronously for the response.
requestAwaitResponse ::
  forall r k (meth :: k).
  (Method k, RequestOk r meth) =>
  ServerHandle r k ->
  Sing meth ->
  MethodParams meth ->
  IO (Either (ResponseError meth) (MethodResult meth))
requestAwaitResponse handle m p = do
  done <- newEmptyMVar
  let
    h :: ResponseHandler r k meth
    h = mkResponseHandler @r @k @meth (liftIO . putMVar done)
  void $ sendRequest handle m p h
  takeMVar done
