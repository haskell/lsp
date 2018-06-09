{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.LSP.Test.Parsing where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (error)
import Language.Haskell.LSP.Test.Messages
import Language.Haskell.LSP.Test.Decoding
import System.IO
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Conduit hiding (await)
import Data.Conduit.Parser

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
notification :: Monad m => ConduitParser FromServerMessage m FromServerMessage
notification = satisfy isServerNotification

-- | Matches if the message is a request.
request :: Monad m => ConduitParser FromServerMessage m FromServerMessage
request = satisfy isServerRequest

-- | Matches if the message is a response.
response :: Monad m => ConduitParser FromServerMessage m FromServerMessage
response = satisfy isServerResponse

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

chanSource :: MonadIO m => Chan o -> ConduitT i o m b
chanSource c = do
  x <- liftIO $ readChan c
  yield x
  chanSource c

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