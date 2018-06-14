module Language.Haskell.LSP.Test.Session where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Conduit
import Data.Conduit.Parser
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import System.Directory
import System.IO

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan FromServerMessage
  , requestMap :: MVar RequestMap
  , initRsp :: MVar InitializeResponse
  }

data SessionState = SessionState
  {
    curReqId :: LspId
  , vfs :: VFS
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



-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSessionWithHandles :: Handle -- ^ Server in
                      -> Handle -- ^ Server out
                      -> (Handle -> Session ()) -- ^ Server listener
                      -> FilePath
                      -> Session a
                      -> IO a
runSessionWithHandles serverIn serverOut serverHandler rootDir session = do
  absRootDir <- canonicalizePath rootDir
  
  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  meaninglessChan <- newChan
  initRsp <- newEmptyMVar

  let context = SessionContext serverIn absRootDir messageChan reqMap initRsp
      initState = SessionState (IdInt 9) mempty

  threadId <- forkIO $ void $ runSession' meaninglessChan context initState (serverHandler serverOut)
  (result, _) <- runSession' messageChan context initState session

  killThread threadId

  return result