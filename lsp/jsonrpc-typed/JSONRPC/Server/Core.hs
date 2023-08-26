module JSONRPC.Server.Core where

import Control.Concurrent
import Control.Concurrent.STM
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
import Data.Map qualified as Map
import Data.Monoid (Ap (..))
import GHC.Generics
import JSONRPC.Id
import JSONRPC.Message
import JSONRPC.Method
import JSONRPC.RPC qualified as RPC
import Control.Lens
import qualified Data.Set as Set
import Data.Foldable (for_)

-- | The map for tracking pending responses and their handlers.
type ResponseMap = Map.Map Id ResponseHandler

data ServerHandle = ServerHandle
  { handlers :: TVar Handlers
  , pendingResponses :: TVar ResponseMap
  -- ^ Responses we are waiting to receive
  , rpcHandle :: RPC.RpcHandle
  }
  deriving stock (Generic)

newtype ServerT m a = ServerT {unServerT :: ReaderT ServerHandle m a}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix)
  deriving (Semigroup, Monoid) via (Ap (ServerT m) a)

-- for deriving the instance of MonadUnliftIO
type role ServerT nominal nominal

runServerT :: ServerHandle -> ServerT m a -> m a
runServerT env = flip runReaderT env . unServerT
{-# INLINE runServerT #-}

class (Monad m, MonadIO m) => MonadServer m where
  getServerHandle :: m ServerHandle

instance MonadIO m => RPC.MonadRpc (ServerT m) where
  getRpcHandle = rpcHandle <$> getServerHandle

instance MonadIO m => MonadServer (ServerT m) where
  getServerHandle = ServerT ask

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

newtype RequestHandler = RequestHandler { handleRequest :: RequestMessage -> IO ResponseMessage }
newtype NotificationHandler = NotificationHandler { handleNotification :: NotificationMessage -> IO () }
  deriving newtype (Semigroup, Monoid)

-- | A handler for response messages.
newtype ResponseHandler = ResponseHandler { handleResponse :: ResponseMessage -> IO () }
  deriving newtype (Semigroup, Monoid)

data Handlers = Handlers
  { reqHandlers :: Map.Map Method RequestHandler
  , notHandlers :: Map.Map Method NotificationHandler
  } deriving stock Generic

instance Semigroup Handlers where
  Handlers r1 n1 <> Handlers r2 n2 = Handlers (r1 <> r2) (Map.unionWith (<>) n1 n2)
instance Monoid Handlers where
  mempty = Handlers mempty mempty

notificationHandler :: Method -> NotificationHandler -> Handlers
notificationHandler m h = Handlers mempty (Map.singleton m h)

requestHandler :: Method -> RequestHandler -> Handlers
requestHandler m h = Handlers (Map.singleton m h) mempty

handledMethods :: Handlers -> Set.Set Method
handledMethods h = Set.fromList $ Map.keys h.reqHandlers ++ Map.keys h.notHandlers

notHandlerFor :: Method -> Lens' Handlers (Maybe NotificationHandler)
notHandlerFor m = #notHandlers . at m

reqHandlerFor :: Method -> Lens' Handlers (Maybe RequestHandler)
reqHandlerFor m = #reqHandlers . at m

data ServerDefinition = ServerDefinition { initialHandlers :: Handlers }

type ServerFunction = ServerHandle -> IO ServerDefinition

new :: RPC.RpcHandle -> ServerFunction -> IO ServerHandle
new rpcHandle serverFun = do
  handlersVar <- newTVarIO mempty
  pendingVar <- newTVarIO mempty
  let env = ServerHandle{handlers = handlersVar, pendingResponses = pendingVar, rpcHandle = rpcHandle}
  ServerDefinition{initialHandlers} <- serverFun env
  atomically $ writeTVar handlersVar initialHandlers
  pure env

{- | Add a request handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
setRequestHandler ::
  ServerHandle ->
  Method ->
  RequestHandler ->
  STM ()
setRequestHandler handle s cb =
  modifyTVar (handlers handle) $ \old ->
    old & reqHandlerFor s ?~ cb

{- |
Directly add a response handler.
-}
setResponseHandler ::
  ServerHandle ->
  Id ->
  ResponseHandler ->
  STM ()
setResponseHandler handle lid cb =
  modifyTVar (handle.pendingResponses) $ \old ->
    Map.insert lid cb old

addResponseHandler ::
  ServerHandle ->
  Id ->
  ResponseHandler ->
  STM ()
addResponseHandler handle lid cb =
  modifyTVar (handle.pendingResponses) $ \old ->
    Map.insertWith (<>) lid cb old

{- | Add a notification handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
setNotificationHandler ::
  ServerHandle ->
  Method ->
  NotificationHandler ->
  STM ()
setNotificationHandler handle method cb =
  modifyTVar (handlers handle) $ \old ->
    old & notHandlerFor method ?~ cb

addNotificationHandler ::
  ServerHandle ->
  Method ->
  NotificationHandler ->
  STM ()
addNotificationHandler handle method cb =
  modifyTVar (handlers handle) $ \old ->
    old & notHandlerFor method %~ \h -> h <> Just cb

setHandlers ::
  ServerHandle ->
  Handlers ->
  STM ()
setHandlers handle hs = do
  -- kinda ugly but not sure of a nicer approach atm
  for_ (Map.toList $ reqHandlers hs) $ \(m, h) ->
    setRequestHandler handle m h
  for_ (Map.toList $ notHandlers hs) $ \(m, h) ->
    setNotificationHandler handle m h

addHandlers ::
  ServerHandle ->
  Handlers ->
  STM ()
addHandlers handle h = modifyTVar (handlers handle) $ \old -> old <> h

sendRequest ::
  ServerHandle ->
  Method ->
  Maybe J.Value ->
  ResponseHandler ->
  IO Id
sendRequest handle meth p resHandler = do
  -- Some duplication here with the bare RPC version to avoid raciness
  reqId <- atomically $ do
    reqId <- RPC.freshId handle.rpcHandle
    addResponseHandler handle reqId resHandler
    pure reqId

  RPC.sendMsg handle.rpcHandle $ Req $ RequestMessage reqId meth p
  return reqId

-- | Send a request and wait synchronously for the response.
requestAwaitResponse ::
  ServerHandle ->
  Method ->
  Maybe J.Value ->
  IO ResponseMessage
requestAwaitResponse handle m p = do
  done <- newEmptyMVar
  void $ sendRequest handle m p $ ResponseHandler $ \msg -> liftIO $ putMVar done msg
  liftIO $ takeMVar done
