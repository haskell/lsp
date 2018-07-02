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
  , modifyM
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
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.TH.ClientCapabilities
import Language.Haskell.LSP.Types hiding (error)
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
    capabilities :: ClientCapabilities -- ^ Specific capabilities the client should advertise. Default is yes to everything.
  , timeout :: Int -- ^ Maximum time to wait for a request in seconds. Defaults to 60.
  , logStdErr :: Bool -- ^ When True redirects the servers stderr output to haskell-lsp-test's stdout. Defaults to False
  }

instance Default SessionConfig where
  def = SessionConfig def 60 False

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
  , curDiagnostics :: Map.Map Uri [Diagnostic]
  }

type ParserStateReader a s r m = ConduitParser a (StateT s (ReaderT r m))

type SessionProcessor = ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO))

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

modifyM :: Monad m => (s -> m s) -> ParserStateReader a s r m ()
modifyM f = do
  old <- lift State.get
  new <- lift $ lift $ lift $ f old
  lift $ State.put new

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
      initState = SessionState (IdInt 0) mempty mempty

  threadId <- forkIO $ void $ runSession meaninglessChan processor context initState (serverHandler serverOut)
  (result, _) <- runSession messageChan processor context initState session

  killThread threadId

  return result

  where processor :: SessionProcessor ()
        processor = awaitForever $ \msg -> do
          processTextChanges msg
          yield msg


processTextChanges :: FromServerMessage -> SessionProcessor ()
processTextChanges (NotPublishDiagnostics n) = do
  let List diags = n ^. params . diagnostics
      doc = n ^. params . uri
  lift $ State.modify (\s ->
    let newDiags = Map.insert doc diags (curDiagnostics s) 
      in s { curDiagnostics = newDiags })

processTextChanges (ReqApplyWorkspaceEdit r) = do

  allChangeParams <- case r ^. params . edit . documentChanges of
    Just (List cs) -> do
      mapM_ (checkIfNeedsOpened . (^. textDocument . uri)) cs
      return $ map getParams cs
    Nothing -> case r ^. params . edit . changes of
      Just cs -> do
        mapM_ checkIfNeedsOpened (HashMap.keys cs)
        return $ concatMap (uncurry getChangeParams) (HashMap.toList cs)
      Nothing -> error "No changes!"

  oldVFS <- vfs <$> lift State.get
  newVFS <- liftIO $ changeFromServerVFS oldVFS r
  lift $ State.modify (\s -> s { vfs = newVFS })

  let groupedParams = groupBy (\a b -> (a ^. textDocument == b ^. textDocument)) allChangeParams
      mergedParams = map mergeParams groupedParams

  ctx <- lift $ lift Reader.ask

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams $ \p -> do
    let h = serverIn ctx
        msg = NotificationMessage "2.0" TextDocumentDidChange p
    liftIO $ B.hPut h $ addHeader (encode msg)

  where checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> lift State.get
          ctx <- lift $ lift Reader.ask

          -- if its not open, open it
          unless (uri `Map.member` oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" TextDocumentDidOpen (DidOpenTextDocumentParams item)
            liftIO $ B.hPut (serverIn ctx) $ addHeader (encode msg)

            oldVFS <- vfs <$> lift State.get
            newVFS <- liftIO $ openVFS oldVFS msg
            lift $ State.modify (\s -> s { vfs = newVFS })

        getParams (TextDocumentEdit docId (List edits)) =
          let changeEvents = map (\e -> TextDocumentContentChangeEvent (Just (e ^. range)) Nothing (e ^. newText)) edits
            in DidChangeTextDocumentParams docId (List changeEvents)

        textDocumentVersions uri = map (VersionedTextDocumentIdentifier uri) [0..]

        textDocumentEdits uri edits = map (\(v, e) -> TextDocumentEdit v (List [e])) $ zip (textDocumentVersions uri) edits

        getChangeParams uri (List edits) = map getParams (textDocumentEdits uri (reverse edits))

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. textDocument) (List events)
processTextChanges _ = return ()
