{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.LSP.Test.Session
  ( Session
  , SessionConfig(..)
  , defaultConfig
  , SessionMessage(..)
  , SessionContext(..)
  , SessionState(..)
  , runSessionWithHandles
  , get
  , put
  , modify
  , modifyM
  , ask
  , asks
  , sendMessage
  , updateState
  , withTimeout
  , logMsg
  , LogMsgType(..)
  )

where

import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
#if __GLASGOW_HASKELL__ >= 806
import Control.Monad.Fail
#endif
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State (get, put)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Conduit as Conduit
import Data.Conduit.Parser as Parser
import Data.Default
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Function
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types.Capabilities
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding (error)
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import System.Console.ANSI
import System.Directory
import System.IO

-- | A session representing one instance of launching and connecting to a server.
-- 
-- You can send and receive messages to the server within 'Session' via 'getMessage',
-- 'sendRequest' and 'sendNotification'.
--

type Session = ParserStateReader FromServerMessage SessionState SessionContext IO

#if __GLASGOW_HASKELL__ >= 806
instance MonadFail Session where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> get
    liftIO $ throw (UnexpectedMessage s lastMsg)
#endif

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig
  { messageTimeout :: Int  -- ^ Maximum time to wait for a message in seconds, defaults to 60.
  , logStdErr      :: Bool -- ^ Redirect the server's stderr to this stdout, defaults to False.
  , logMessages    :: Bool -- ^ Trace the messages sent and received to stdout, defaults to False.
  , logColor       :: Bool -- ^ Add ANSI color to the logged messages, defaults to True.
  , lspConfig      :: Maybe Value -- ^ The initial LSP config as JSON value, defaults to Nothing.
  }

-- | The configuration used in 'Language.Haskell.LSP.Test.runSession'.
defaultConfig :: SessionConfig
defaultConfig = SessionConfig 60 False False True Nothing

instance Default SessionConfig where
  def = defaultConfig

data SessionMessage = ServerMessage FromServerMessage
                    | TimeoutMessage Int
  deriving Show

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan SessionMessage
  , requestMap :: MVar RequestMap
  , initRsp :: MVar InitializeResponse
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  }

class Monad m => HasReader r m where
  ask :: m r
  asks :: (r -> b) -> m b
  asks f = f <$> ask

instance Monad m => HasReader r (ParserStateReader a s r m) where
  ask = lift $ lift Reader.ask

instance Monad m => HasReader SessionContext (ConduitM a b (StateT s (ReaderT SessionContext m))) where
  ask = lift $ lift Reader.ask

data SessionState = SessionState
  {
    curReqId :: LspId
  , vfs :: VFS
  , curDiagnostics :: Map.Map Uri [Diagnostic]
  , curTimeoutId :: Int
  , overridingTimeout :: Bool
  -- ^ The last received message from the server.
  -- Used for providing exception information
  , lastReceivedMessage :: Maybe FromServerMessage
  }

class Monad m => HasState s m where
  get :: m s

  put :: s -> m ()

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  modifyM :: (HasState s m, Monad m) => (s -> m s) -> m ()
  modifyM f = get >>= f >>= put

instance Monad m => HasState s (ParserStateReader a s r m) where
  get = lift State.get
  put = lift . State.put

instance Monad m => HasState SessionState (ConduitM a b (StateT SessionState m))
 where
  get = lift State.get
  put = lift . State.put

type ParserStateReader a s r m = ConduitParser a (StateT s (ReaderT r m))

runSession :: SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSession context state session = runReaderT (runStateT conduit state) context
  where
    conduit = runConduit $ chanSource .| watchdog .| updateStateC .| runConduitParser (catchError session handler)

    handler (Unexpected "ConduitParser.empty") = do
      lastMsg <- fromJust . lastReceivedMessage <$> get
      name <- getParserName
      liftIO $ throw (UnexpectedMessage (T.unpack name) lastMsg)

    handler e = throw e

    chanSource = do
      msg <- liftIO $ readChan (messageChan context)
      yield msg
      chanSource

    watchdog :: ConduitM SessionMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
    watchdog = Conduit.awaitForever $ \msg -> do
      curId <- curTimeoutId <$> get
      case msg of
        ServerMessage sMsg -> yield sMsg
        TimeoutMessage tId -> when (curId == tId) $ throw Timeout

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSessionWithHandles :: Handle -- ^ Server in
                      -> Handle -- ^ Server out
                      -> (Handle -> SessionContext -> IO ()) -- ^ Server listener
                      -> SessionConfig
                      -> ClientCapabilities
                      -> FilePath -- ^ Root directory
                      -> Session a
                      -> IO a
runSessionWithHandles serverIn serverOut serverHandler config caps rootDir session = do
  absRootDir <- canonicalizePath rootDir

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  initRsp <- newEmptyMVar

  mainThreadId <- myThreadId

  let context = SessionContext serverIn absRootDir messageChan reqMap initRsp config caps
      initState = SessionState (IdInt 0) mempty mempty 0 False Nothing
      launchServerHandler = forkIO $ catch (serverHandler serverOut context)
                                           (throwTo mainThreadId :: SessionException -> IO ())
  (result, _) <- bracket launchServerHandler killThread $
    const $ runSession context initState session

  return result

updateStateC :: ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
updateStateC = awaitForever $ \msg -> do
  updateState msg
  yield msg

updateState :: (MonadIO m, HasReader SessionContext m, HasState SessionState m) => FromServerMessage -> m ()
updateState (NotPublishDiagnostics n) = do
  let List diags = n ^. params . diagnostics
      doc = n ^. params . uri
  modify (\s ->
    let newDiags = Map.insert doc diags (curDiagnostics s)
      in s { curDiagnostics = newDiags })

updateState (ReqApplyWorkspaceEdit r) = do

  allChangeParams <- case r ^. params . edit . documentChanges of
    Just (List cs) -> do
      mapM_ (checkIfNeedsOpened . (^. textDocument . uri)) cs
      return $ map getParams cs
    Nothing -> case r ^. params . edit . changes of
      Just cs -> do
        mapM_ checkIfNeedsOpened (HashMap.keys cs)
        return $ concatMap (uncurry getChangeParams) (HashMap.toList cs)
      Nothing -> error "No changes!"

  modifyM $ \s -> do
    newVFS <- liftIO $ changeFromServerVFS (vfs s) r
    return $ s { vfs = newVFS }

  let groupedParams = groupBy (\a b -> (a ^. textDocument == b ^. textDocument)) allChangeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams (sendMessage . NotificationMessage "2.0" TextDocumentDidChange)

  -- Update VFS to new document versions
  let sortedVersions = map (sortBy (compare `on` (^. textDocument . version))) groupedParams
      latestVersions = map ((^. textDocument) . last) sortedVersions
      bumpedVersions = map (version . _Just +~ 1) latestVersions

  forM_ bumpedVersions $ \(VersionedTextDocumentIdentifier uri v) ->
    modify $ \s ->
      let oldVFS = vfs s
          update (VirtualFile oldV t) = VirtualFile (fromMaybe oldV v) t
          newVFS = Map.adjust update uri oldVFS
      in s { vfs = newVFS }

  where checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> get
          ctx <- ask

          -- if its not open, open it
          unless (uri `Map.member` oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" TextDocumentDidOpen (DidOpenTextDocumentParams item)
            liftIO $ B.hPut (serverIn ctx) $ addHeader (encode msg)

            modifyM $ \s -> do
              newVFS <- liftIO $ openVFS (vfs s) msg
              return $ s { vfs = newVFS }

        getParams (TextDocumentEdit docId (List edits)) =
          let changeEvents = map (\e -> TextDocumentContentChangeEvent (Just (e ^. range)) Nothing (e ^. newText)) edits
            in DidChangeTextDocumentParams docId (List changeEvents)

        textDocumentVersions uri = map (VersionedTextDocumentIdentifier uri . Just) [0..]

        textDocumentEdits uri edits = map (\(v, e) -> TextDocumentEdit v (List [e])) $ zip (textDocumentVersions uri) edits

        getChangeParams uri (List edits) = map getParams (textDocumentEdits uri (reverse edits))

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. textDocument) (List events)
updateState _ = return ()

sendMessage :: (MonadIO m, HasReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- serverIn <$> ask
  logMsg LogClient msg
  liftIO $ B.hPut h (addHeader $ encode msg)

-- | Execute a block f that will throw a 'TimeoutException'
-- after duration seconds. This will override the global timeout
-- for waiting for messages to arrive defined in 'SessionConfig'.
withTimeout :: Int -> Session a -> Session a
withTimeout duration f = do
  chan <- asks messageChan
  timeoutId <- curTimeoutId <$> get
  modify $ \s -> s { overridingTimeout = True }
  liftIO $ forkIO $ do
    threadDelay (duration * 1000000)
    writeChan chan (TimeoutMessage timeoutId)
  res <- f
  modify $ \s -> s { curTimeoutId = timeoutId + 1,
                     overridingTimeout = False
                   }
  return res

data LogMsgType = LogServer | LogClient
  deriving Eq

-- | Logs the message if the config specified it
logMsg :: (ToJSON a, MonadIO m, HasReader SessionContext m)
       => LogMsgType -> a -> m ()
logMsg t msg = do
  shouldLog <- asks $ logMessages . config
  shouldColor <- asks $ logColor . config
  liftIO $ when shouldLog $ do
    when shouldColor $ setSGR [SetColor Foreground Dull color]
    putStrLn $ arrow ++ showPretty msg
    when shouldColor $ setSGR [Reset]

  where arrow
          | t == LogServer  = "<-- "
          | otherwise       = "--> "
        color
          | t == LogServer  = Magenta
          | otherwise       = Cyan

        showPretty = B.unpack . encodePretty

