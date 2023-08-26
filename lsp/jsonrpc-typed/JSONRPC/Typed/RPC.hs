module JSONRPC.Typed.RPC where

import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (handle)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans
import Data.Aeson qualified as J
import Data.Generics.Labels ()
import Data.Singletons
import Data.String (IsString (..))
import JSONRPC.Id
import JSONRPC.Message qualified as Untyped
import JSONRPC.RPC qualified as Untyped
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import Prelude hiding (id)

newtype RpcHandle r k = RpcHandle Untyped.RpcHandle

class (Untyped.MonadRpc m) => MonadRpc r k m | m -> r k

getRpcHandle :: MonadRpc r k m => m (RpcHandle r k)
getRpcHandle = RpcHandle <$> Untyped.getRpcHandle

newtype RpcT r k m a = RpcT (Untyped.RpcT m a)
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix, Untyped.MonadRpc)

runRpcT :: RpcHandle r k -> RpcT r k m a -> m a
runRpcT (RpcHandle env) (RpcT act) = Untyped.runRpcT env act

instance MonadIO m => MonadRpc r k (RpcT r k m)

sendNotification ::
  forall r k (meth :: k).
  (Method k, NotificationOk r meth) =>
  RpcHandle r k ->
  Sing meth ->
  MethodParams meth ->
  IO ()
sendNotification (RpcHandle handle) m p = Untyped.sendMsg handle $ Untyped.Not $ toUntypedNot $ NotificationMessage m p

sendResponse ::
  forall r k (meth :: k).
  (Method k, ResponseOk r meth) =>
  RpcHandle r k ->
  Sing meth ->
  Id ->
  Either (ResponseError meth) (MethodResult meth) ->
  IO ()
sendResponse (RpcHandle handle) m i r = Untyped.sendMsg handle $ Untyped.Rsp $ toUntypedRsp m $ ResponseMessage i r

sendRequest ::
  forall r k (meth :: k).
  (Method k, RequestOk r meth) =>
  RpcHandle r k ->
  Sing meth ->
  MethodParams meth ->
  IO Id
sendRequest (RpcHandle handle) m p =
  bring @_ @IsJsonParts (Proxy @k) m $
    Untyped.sendRequest handle (toUntypedMethod $ fromSing m) (Just $ J.toJSON p)

requestExpectResponse ::
  forall r k (meth :: k).
  ( Method k
  , SingI r
  , RequestOk r meth
  ) =>
  RpcHandle r k ->
  Sing meth ->
  MethodParams meth ->
  IO (Either (ResponseError meth) (MethodResult meth))
requestExpectResponse (RpcHandle handle) m p = do
  let um = toUntypedMethod $ fromSing m
  rm <- bring @_ @IsJsonParts (Proxy @k) m $ Untyped.requestExpectResponse handle um (Just $ J.toJSON p)
  case fromKnownUntypedRsp @k m rm of
    Right tmsg -> pure $ tmsg ^. #result
    Left e -> throwM $ Untyped.UnexpectedMessage (fromString e) (Untyped.Rsp rm)

expectNotification ::
  forall r k (meth :: k).
  ( Method k
  , NotificationOk (OtherRole r) meth
  ) =>
  RpcHandle r k ->
  Sing meth ->
  IO (NotificationMessage meth)
expectNotification (RpcHandle handle) m = do
  msg <- Untyped.expectNotification handle
  case fromUntypedNot @k msg >>= assertNotType m of
    Right tmsg -> pure tmsg
    Left e -> throwM $ Untyped.UnexpectedMessage (fromString e) (Untyped.Not msg)

expectRequest ::
  forall r k (meth :: k).
  ( Method k
  , RequestOk (OtherRole r) meth
  ) =>
  RpcHandle r k ->
  Sing meth ->
  IO (RequestMessage meth)
expectRequest (RpcHandle handle) m = do
  msg <- Untyped.expectRequest handle
  case fromUntypedReq @k msg >>= assertReqType m of
    Right tmsg -> pure tmsg
    Left e -> throwM $ Untyped.UnexpectedMessage (fromString e) (Untyped.Req msg)

expectResponse ::
  forall r k (meth :: k).
  ( Method k
  , ResponseOk (OtherRole r) meth
  ) =>
  RpcHandle r k ->
  Sing meth ->
  IO (ResponseMessage meth)
expectResponse (RpcHandle handle) m = do
  msg <- Untyped.expectResponse handle
  case fromKnownUntypedRsp @k m msg of
    Right tmsg -> pure tmsg
    Left e -> throwM $ Untyped.UnexpectedMessage (fromString e) (Untyped.Rsp msg)

expectResponseFor ::
  forall r k (meth :: k).
  ( Method k
  , ResponseOk (OtherRole r) meth
  ) =>
  RpcHandle r k ->
  Sing meth ->
  Id ->
  IO (ResponseMessage meth)
expectResponseFor handle ms i = do
  msg <- expectResponse handle ms
  unless (msg ^. #id == i) $
    throwM $
      Untyped.UnexpectedMessage ("Expected a response with id " <> fromString (show i)) (Untyped.Rsp (toUntypedRsp ms msg))
  pure msg
