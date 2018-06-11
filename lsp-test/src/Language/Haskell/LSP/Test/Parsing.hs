{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.LSP.Test.Parsing where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit hiding (await)
import Data.Conduit.Parser
import Data.Maybe
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (error)
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Messages
import System.IO

data SessionContext = SessionContext
  {
    serverIn :: Handle,
    rootDir :: FilePath,
    messageChan :: Chan FromServerMessage,
    requestMap :: MVar RequestMap
  }

newtype SessionState = SessionState
  {
    curReqId :: LspId
  }

type ParserStateReader a s r m = ConduitParser a (StateT s (ReaderT r m))

-- | A session representing one instance of launching and connecting to a server.
-- 
-- You can send and receive messages to the server within 'Session' via 'getMessage',
-- 'sendRequest' and 'sendNotification'.
--
-- @
-- runSession \"path\/to\/root\/dir\" $ do
--   docItem <- getDocItem "Desktop/simple.hs" "haskell"
--   sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams docItem)
--   diagnostics <- getMessage :: Session PublishDiagnosticsNotification
-- @
type Session = ParserStateReader FromServerMessage SessionState SessionContext IO

-- | Matches if the message is a notification.
anyNotification :: Monad m => ConduitParser FromServerMessage m FromServerMessage
anyNotification = satisfy isServerNotification

notification :: forall m a. (Monad m, FromJSON a) => ConduitParser FromServerMessage m (NotificationMessage ServerMethod a)
notification = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (NotificationMessage ServerMethod a)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | Matches if the message is a request.
anyRequest :: Monad m => ConduitParser FromServerMessage m FromServerMessage
anyRequest = satisfy isServerRequest

request :: forall m a b. (Monad m, FromJSON a, FromJSON b) => ConduitParser FromServerMessage m (RequestMessage ServerMethod a b)
request = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (RequestMessage ServerMethod a b)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | Matches if the message is a response.
anyResponse :: Monad m => ConduitParser FromServerMessage m FromServerMessage
anyResponse = satisfy isServerResponse

response :: forall m a. (Monad m, FromJSON a) => ConduitParser FromServerMessage m (ResponseMessage a)
response = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (ResponseMessage a)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | A version of encode that encodes FromServerMessages as if they
-- weren't wrapped.
encodeMsg :: FromServerMessage -> B.ByteString
encodeMsg = encode . genericToJSON (defaultOptions { sumEncoding = UntaggedValue })

decodeMsg :: FromJSON a => B.ByteString -> a
decodeMsg x = fromMaybe (error $ "Unexpected message type\nGot:\n " ++ show x)
                  (decode x)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: Monad m => ConduitParser FromServerMessage m FromServerMessage
loggingNotification = satisfy shouldSkip
  where
    shouldSkip (NotLogMessage _) = True
    shouldSkip (NotShowMessage _) = True
    shouldSkip (ReqShowMessage _) = True
    shouldSkip _ = False

publishDiagnosticsNotification :: Monad m => ConduitParser FromServerMessage m PublishDiagnosticsNotification
publishDiagnosticsNotification = do
  NotPublishDiagnostics diags <- satisfy test
  return diags
  where test (NotPublishDiagnostics _) = True
        test _ = False

satisfy :: Monad m => (a -> Bool) -> ConduitParser a m a
satisfy pred = do
  x <- await
  if pred x
    then return x
    else empty

runSession' :: Chan FromServerMessage -> SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSession' chan context state session = runReaderT (runStateT conduit state) context
  where conduit = runConduit $ chanSource chan .| runConduitParser session

get :: Monad m => ParserStateReader a s r m s
get = lift Control.Monad.Trans.State.get

put :: Monad m => s -> ParserStateReader a s r m ()
put = lift . Control.Monad.Trans.State.put

modify :: Monad m => (s -> s) -> ParserStateReader a s r m ()
modify = lift . Control.Monad.Trans.State.modify

ask :: Monad m => ParserStateReader a s r m r
ask = lift $ lift Control.Monad.Trans.Reader.ask
