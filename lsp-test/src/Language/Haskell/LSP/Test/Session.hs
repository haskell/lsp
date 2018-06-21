{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.LSP.Test.Session
  ( Session
  , SessionConfig(..)
  , SessionContext(..)
  , SessionState(..)
  , MonadSessionConfig(..)
  , runSessionWithHandles
  , get
  , put
  , modify
  , ask)

where

import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State (get, put, modify)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Conduit hiding (await)
import Data.Conduit.Parser
import Data.Default
import Data.Foldable
import Data.List
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.TH.ClientCapabilities
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import System.Directory
import System.IO

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

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig
  {
    capabilities :: ClientCapabilities, -- ^ Specific capabilities the client should advertise.
    timeout :: Int -- ^ Maximum time to wait for a request in seconds.
  }

instance Default SessionConfig where
  def = SessionConfig def 60

class Monad m => MonadSessionConfig m where
  sessionConfig :: m SessionConfig

instance Monad m => MonadSessionConfig (StateT SessionState (ReaderT SessionContext m)) where
  sessionConfig = config <$> lift Reader.ask

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan FromServerMessage
  , requestMap :: MVar RequestMap
  , initRsp :: MVar InitializeResponse
  , config :: SessionConfig
  }

data SessionState = SessionState
  {
    curReqId :: LspId
  , vfs :: VFS
  }

type ParserStateReader a s r m = ConduitParser a (StateT s (ReaderT r m))

type SessionProcessor = ConduitT FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO))

runSession :: Chan FromServerMessage -> SessionProcessor () -> SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSession chan preprocessor context state session = runReaderT (runStateT conduit state) context
  where conduit = runConduit $ chanSource chan .| preprocessor .| runConduitParser (catchError session handler)
        handler e@(Unexpected "ConduitParser.empty") = do
          
          -- Horrible way to get last item in conduit:
          -- Add a fake message so we can tell when to stop
          liftIO $ writeChan chan (RspShutdown (ResponseMessage "EMPTY" IdRspNull Nothing Nothing))
          x <- peek
          case x of
            Just x -> do
              lastMsg <- skipToEnd x
              name <- getParserName
              liftIO $ throw (UnexpectedMessageException (T.unpack name) lastMsg)
            Nothing -> throw e

        handler e = throw e
        
        skipToEnd x = do
          y <- peek
          case y of
            Just (RspShutdown (ResponseMessage "EMPTY" IdRspNull Nothing Nothing)) -> return x
            Just _ -> await >>= skipToEnd
            Nothing -> return x

get :: Monad m => ParserStateReader a s r m s
get = lift State.get

put :: Monad m => s -> ParserStateReader a s r m ()
put = lift . State.put

modify :: Monad m => (s -> s) -> ParserStateReader a s r m ()
modify = lift . State.modify

ask :: Monad m => ParserStateReader a s r m r
ask = lift $ lift Reader.ask

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSessionWithHandles :: Handle -- ^ Server in
                      -> Handle -- ^ Server out
                      -> (Handle -> Session ()) -- ^ Server listener
                      -> SessionConfig
                      -> FilePath
                      -> Session a
                      -> IO a
runSessionWithHandles serverIn serverOut serverHandler config rootDir session = do
  absRootDir <- canonicalizePath rootDir

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  meaninglessChan <- newChan
  initRsp <- newEmptyMVar

  let context = SessionContext serverIn absRootDir messageChan reqMap initRsp config
      initState = SessionState (IdInt 0) mempty

  threadId <- forkIO $ void $ runSession meaninglessChan processor context initState (serverHandler serverOut)
  (result, _) <- runSession messageChan processor context initState session

  killThread threadId

  return result

  where processor :: SessionProcessor ()
        processor = awaitForever $ \msg -> do
          processTextChanges msg
          yield msg


processTextChanges :: FromServerMessage -> SessionProcessor ()
processTextChanges (ReqApplyWorkspaceEdit r) = do
  List changeParams <- case r ^. params . edit . documentChanges of
    Just cs -> mapM applyTextDocumentEdit cs
    Nothing -> case r ^. params . edit . changes of
      Just cs -> mapM (uncurry applyTextEdit) (List (HashMap.toList cs))
      Nothing -> return (List [])

  let groupedParams = groupBy (\a b -> (a ^. textDocument == b ^. textDocument)) changeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams $ \p -> do
    h <- serverIn <$> lift (lift Reader.ask)
    let msg = NotificationMessage "2.0" TextDocumentDidChange p
    liftIO $ B.hPut h $ addHeader (encode msg)

  where applyTextDocumentEdit (TextDocumentEdit docId (List edits)) = do
          oldVFS <- vfs <$> lift State.get
          let changeEvents = map (\e -> TextDocumentContentChangeEvent (Just (e ^. range)) Nothing (e ^. newText)) edits
              params = DidChangeTextDocumentParams docId (List changeEvents)
          newVFS <- liftIO $ changeVFS oldVFS (fmClientDidChangeTextDocumentNotification params)
          lift $ State.modify (\s -> s { vfs = newVFS })
          return params

        applyTextEdit uri edits = applyTextDocumentEdit (TextDocumentEdit (VersionedTextDocumentIdentifier uri 0) edits)

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. textDocument) (List events)
processTextChanges _ = return ()
