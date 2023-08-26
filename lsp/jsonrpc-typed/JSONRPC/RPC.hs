module JSONRPC.RPC where

import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Applicative
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import JSONRPC.IO qualified as IO
import JSONRPC.Id
import JSONRPC.Message
import JSONRPC.Method
import Language.LSP.Protocol.Types (type (|?) (..))
import System.IO
import Prelude hiding (id)

-- | The connection for sending and receiving messages.
data Connection = Connection
  { sendMessage :: Message -> IO ()
  , recvMessage :: IO Message
  }
  deriving stock (Generic)

-- | The state of the RPC system.
data RpcHandle = RpcHandle
  { conn :: Connection
  -- ^ Our connection for messages
  , nextId :: TVar Int32
  -- ^ An incrementing counter for generating unique IDs
  }
  deriving stock (Generic)

-- | A monad with access to the 'RpcHandle'.
class (Monad m, MonadIO m) => MonadRpc m where
  getRpcHandle :: m RpcHandle

-- | A convenient monad transformer which mostly just has a 'MonadRpc' instance.
newtype RpcT m a = RpcT {unRpcT :: ReaderT RpcHandle m a}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix)

runRpcT :: RpcHandle -> RpcT m a -> m a
runRpcT env = flip runReaderT env . unRpcT

instance MonadIO m => MonadRpc (RpcT m) where
  getRpcHandle = RpcT ask

{- | Set up a 'RpcHandle' using some 'Handle's as the connections.
Returns the 'RpcHandle' and an 'IO' action to run the connection threads.
-}
initRpcWithHandles ::
  LogAction IO (WithSeverity IO.IoLog) ->
  Handle ->
  Handle ->
  IO (RpcHandle, IO ())
initRpcWithHandles logger hin hout = do
  hSetBuffering hin NoBuffering
  hSetEncoding hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BS.hPut hout out
      hFlush hout

  initRpcWith logger clientIn clientOut

{- | Set up a 'RpcHandle' using some functions as the connections.
Returns the 'RpcHandle' and an 'IO' action to run the connection threads.
-}
initRpcWith ::
  LogAction IO (WithSeverity IO.IoLog) ->
  -- | Input from the client.
  IO BS.ByteString ->
  -- | Server output.
  (BS.ByteString -> IO ()) ->
  IO (RpcHandle, IO ())
initRpcWith logger clientIn clientOut = do
  cout <- atomically newTChan :: IO (TChan Message)
  cin <- atomically newTChan :: IO (TChan Message)

  let sendServerMsg :: Message -> IO ()
      sendServerMsg msg = atomically $ writeTChan cout msg
      recvServerMsg :: IO Message
      recvServerMsg = atomically $ readTChan cout

      sendClientMsg :: Message -> IO ()
      sendClientMsg msg = atomically $ writeTChan cin msg
      recvClientMsg :: IO Message
      recvClientMsg = atomically $ readTChan cin

  let cnn = Connection sendServerMsg recvClientMsg
  rpcHandle <- atomically $ do
    idVar <- newTVar 0
    pure $ RpcHandle{conn = cnn, nextId = idVar}

  let sending =
        IO.sendThread
          logger
          recvServerMsg
          clientOut
      receiving =
        IO.recvThread
          logger
          clientIn
          sendClientMsg
          sendServerMsg

  -- Bind the threads together so that either of them terminating will terminate both
  let action = sending `Async.race_` receiving
  return (rpcHandle, action)

-- | Get a fresh ID.
freshId :: RpcHandle -> STM Id
freshId handle = stateTVar (handle.nextId) $ \cur ->
  let !next = cur + 1 in (IdInt cur, next)

sendMsg :: RpcHandle -> Message -> IO ()
sendMsg handle msg = (handle.conn.sendMessage) msg

recvMsg :: RpcHandle -> IO Message
recvMsg handle = handle.conn.recvMessage

sendNotification ::
  RpcHandle ->
  Method ->
  Maybe J.Value ->
  IO ()
sendNotification handle m p = sendMsg handle $ Not $ NotificationMessage m p

sendResponse ::
  RpcHandle ->
  Method ->
  Id ->
  Either ResponseError J.Value ->
  IO ()
sendResponse handle _m i (Right res) = sendMsg handle $ Rsp $ ResponseMessage (InL i) (Just res) Nothing
sendResponse handle _m i (Left err) = sendMsg handle $ Rsp $ ResponseMessage (InL i) Nothing (Just err)

sendRequest ::
  RpcHandle ->
  Method ->
  Maybe J.Value ->
  IO Id
sendRequest handle meth p = do
  reqId <- atomically $ freshId handle
  sendMsg handle $ Req $ RequestMessage reqId meth p
  return reqId

-- | Exception for unexpected messages.
data UnexpectedMessage = UnexpectedMessage Text Message
  deriving (Show)

instance Exception UnexpectedMessage

-- | Send a request and expect to immediately receive a resonse.
requestExpectResponse ::
  RpcHandle ->
  Method ->
  Maybe J.Value ->
  IO ResponseMessage
requestExpectResponse handle ms p = do
  i <- sendRequest handle ms p
  expectResponseFor handle i

-- | Expect a notification.
expectNotification ::
  RpcHandle ->
  IO NotificationMessage
expectNotification handle = do
  m <- recvMsg handle
  case m of
    Not msg -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a notification" msg

-- | Expect a request.
expectRequest ::
  RpcHandle ->
  IO RequestMessage
expectRequest handle = do
  m <- recvMsg handle
  case m of
    Req msg -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a request" msg

-- | Expect a response.
expectResponse ::
  RpcHandle ->
  IO ResponseMessage
expectResponse handle = do
  m <- recvMsg handle
  case m of
    Rsp msg -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a response" msg

-- | Expect a response for a specific ID.
expectResponseFor ::
  RpcHandle ->
  Id ->
  IO ResponseMessage
expectResponseFor handle i = do
  msg <- expectResponse handle
  case msg.id of
    InL mid | mid == i -> pure ()
    _ -> throwM $ UnexpectedMessage ("Expected a response with id " <> fromString (show i)) (Rsp msg)
  pure msg
