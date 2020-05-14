{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.LSP.Test.Session
  ( Session(..)
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
  , getCurTimeoutId
  , bumpTimeoutId
  , logMsg
  , LogMsgType(..)
  )

where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State
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
import Language.Haskell.LSP.Types.Lens
import qualified Language.Haskell.LSP.Types.Lens as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
import System.Timeout

-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.Haskell.LSP.Test.message',
-- 'Language.Haskell.LSP.Test.sendRequest' and
-- 'Language.Haskell.LSP.Test.sendNotification'.

newtype Session a = Session (ConduitParser FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative)

#if __GLASGOW_HASKELL__ >= 806
instance MonadFail Session where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> get
    liftIO $ throw (UnexpectedMessage s lastMsg)
#endif

-- | Stuff you can configure for a 'Session'.
data SessionConfig = SessionConfig
  { messageTimeout :: Int  -- ^ Maximum time to wait for a message in seconds, defaults to 60.
  , logStdErr      :: Bool
  -- ^ Redirect the server's stderr to this stdout, defaults to False.
  -- Can be overriden with @LSP_TEST_LOG_STDERR@.
  , logMessages    :: Bool
  -- ^ Trace the messages sent and received to stdout, defaults to False.
  -- Can be overriden with the environment variable @LSP_TEST_LOG_MESSAGES@.
  , logColor       :: Bool -- ^ Add ANSI color to the logged messages, defaults to True.
  , lspConfig      :: Maybe Value -- ^ The initial LSP config as JSON value, defaults to Nothing.
  , ignoreLogNotifications :: Bool
  -- ^ Whether or not to ignore 'Language.Haskell.LSP.Types.ShowMessageNotification' and
  -- 'Language.Haskell.LSP.Types.LogMessageNotification', defaults to False.
  --
  -- @since 0.9.0.0
  }

-- | The configuration used in 'Language.Haskell.LSP.Test.runSession'.
defaultConfig :: SessionConfig
defaultConfig = SessionConfig 60 False False True Nothing False

instance Default SessionConfig where
  def = defaultConfig

data SessionMessage = ServerMessage FromServerMessage
                    | TimeoutMessage Int
  deriving Show

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan SessionMessage -- ^ Where all messages come through
  -- Keep curTimeoutId in SessionContext, as its tied to messageChan
  , curTimeoutId :: MVar Int -- ^ The current timeout we are waiting on
  , requestMap :: MVar RequestMap
  , initRsp :: MVar InitializeResponse
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  }

class Monad m => HasReader r m where
  ask :: m r
  asks :: (r -> b) -> m b
  asks f = f <$> ask

instance HasReader SessionContext Session where
  ask  = Session (lift $ lift Reader.ask)

instance Monad m => HasReader r (ConduitM a b (StateT s (ReaderT r m))) where
  ask = lift $ lift Reader.ask

getCurTimeoutId :: (HasReader SessionContext m, MonadIO m) => m Int
getCurTimeoutId = asks curTimeoutId >>= liftIO . readMVar

-- Pass this the timeoutid you *were* waiting on
bumpTimeoutId :: (HasReader SessionContext m, MonadIO m) => Int -> m ()
bumpTimeoutId prev = do
  v <- asks curTimeoutId
  -- when updating the curtimeoutid, account for the fact that something else
  -- might have bumped the timeoutid in the meantime
  liftIO $ modifyMVar_ v (\x -> pure (max x (prev + 1)))

data SessionState = SessionState
  {
    curReqId :: LspId
  , vfs :: VFS
  , curDiagnostics :: Map.Map NormalizedUri [Diagnostic]
  , overridingTimeout :: Bool
  -- ^ The last received message from the server.
  -- Used for providing exception information
  , lastReceivedMessage :: Maybe FromServerMessage
  , curDynCaps :: Map.Map T.Text Registration
  -- ^ The capabilities that the server has dynamically registered with us so
  -- far
  }

class Monad m => HasState s m where
  get :: m s

  put :: s -> m ()

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  modifyM :: (HasState s m, Monad m) => (s -> m s) -> m ()
  modifyM f = get >>= f >>= put

instance HasState SessionState Session where
  get = Session (lift State.get)
  put = Session . lift . State.put

instance Monad m => HasState s (StateT s m) where
  get = State.get
  put = State.put

instance (Monad m, (HasState s m)) => HasState s (ConduitM a b m)
 where
  get = lift get
  put = lift . put

instance (Monad m, (HasState s m)) => HasState s (ConduitParser a m)
 where
  get = lift get
  put = lift . put

runSession :: SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSession context state (Session session) = runReaderT (runStateT conduit state) context
  where
    conduit = runConduit $ chanSource .| watchdog .| updateStateC .| runConduitParser (catchError session handler)

    handler (Unexpected "ConduitParser.empty") = do
      lastMsg <- fromJust . lastReceivedMessage <$> get
      name <- getParserName
      liftIO $ throw (UnexpectedMessage (T.unpack name) lastMsg)

    handler e = throw e

    chanSource = do
      msg <- liftIO $ readChan (messageChan context)
      unless (ignoreLogNotifications (config context) && isLogNotification msg) $
        yield msg
      chanSource

    isLogNotification (ServerMessage (NotShowMessage _)) = True
    isLogNotification (ServerMessage (NotLogMessage _)) = True
    isLogNotification _ = False

    watchdog :: ConduitM SessionMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
    watchdog = Conduit.awaitForever $ \msg -> do
      curId <- getCurTimeoutId
      case msg of
        ServerMessage sMsg -> yield sMsg
        TimeoutMessage tId -> when (curId == tId) $ lastReceivedMessage <$> get >>= throw . Timeout

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSessionWithHandles :: Handle -- ^ Server in
                      -> Handle -- ^ Server out
                      -> ProcessHandle -- ^ Server process
                      -> (Handle -> SessionContext -> IO ()) -- ^ Server listener
                      -> SessionConfig
                      -> ClientCapabilities
                      -> FilePath -- ^ Root directory
                      -> Session () -- ^ To exit the Server properly
                      -> Session a
                      -> IO a
runSessionWithHandles serverIn serverOut serverProc serverHandler config caps rootDir exitServer session = do
  absRootDir <- canonicalizePath rootDir

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering
  -- This is required to make sure that we don’t get any
  -- newline conversion or weird encoding issues.
  hSetBinaryMode serverIn True
  hSetBinaryMode serverOut True

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  timeoutIdVar <- newMVar 0
  initRsp <- newEmptyMVar

  mainThreadId <- myThreadId

  let context = SessionContext serverIn absRootDir messageChan timeoutIdVar reqMap initRsp config caps
      initState vfs = SessionState (IdInt 0) vfs mempty False Nothing mempty
      runSession' ses = initVFS $ \vfs -> runSession context (initState vfs) ses

      errorHandler = throwTo mainThreadId :: SessionException -> IO ()
      serverListenerLauncher =
        forkIO $ catch (serverHandler serverOut context) errorHandler
      server = (Just serverIn, Just serverOut, Nothing, serverProc)
      serverAndListenerFinalizer tid = do
        finally (timeout (messageTimeout config * 1^6)
                         (runSession' exitServer))
                (cleanupProcess server >> killThread tid)

  (result, _) <- bracket serverListenerLauncher
                         serverAndListenerFinalizer
                         (const $ runSession' session)
  return result

updateStateC :: ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
updateStateC = awaitForever $ \msg -> do
  updateState msg
  yield msg

updateState :: (MonadIO m, HasReader SessionContext m, HasState SessionState m)
            => FromServerMessage -> m ()

-- Keep track of dynamic capability registration
updateState (ReqRegisterCapability req) = do
  let List newRegs = (\r -> (r ^. LSP.id, r)) <$> req ^. params . registrations
  modify $ \s ->
    s { curDynCaps = Map.union (Map.fromList newRegs) (curDynCaps s) }

updateState (ReqUnregisterCapability req) = do
  let List unRegs = (^. LSP.id) <$> req ^. params . unregistrations
  modify $ \s ->
    let newCurDynCaps = foldr' Map.delete (curDynCaps s) unRegs
    in s { curDynCaps = newCurDynCaps }

updateState (NotPublishDiagnostics n) = do
  let List diags = n ^. params . diagnostics
      doc = n ^. params . uri
  modify $ \s ->
    let newDiags = Map.insert (toNormalizedUri doc) diags (curDiagnostics s)
      in s { curDiagnostics = newDiags }

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

  let groupedParams = groupBy (\a b -> a ^. textDocument == b ^. textDocument) allChangeParams
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
          update (VirtualFile oldV file_ver t) = VirtualFile (fromMaybe oldV v) (file_ver + 1) t
          newVFS = updateVFS (Map.adjust update (toNormalizedUri uri)) oldVFS
      in s { vfs = newVFS }

  where checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> get
          ctx <- ask

          -- if its not open, open it
          unless (toNormalizedUri uri `Map.member` vfsMap oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = NotificationMessage "2.0" TextDocumentDidOpen (DidOpenTextDocumentParams item)
            liftIO $ B.hPut (serverIn ctx) $ addHeader (encode msg)

            modifyM $ \s -> do
              let (newVFS,_) = openVFS (vfs s) msg
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

-- | Execute a block f that will throw a 'Language.Haskell.LSP.Test.Exception.Timeout' exception
-- after duration seconds. This will override the global timeout
-- for waiting for messages to arrive defined in 'SessionConfig'.
withTimeout :: Int -> Session a -> Session a
withTimeout duration f = do
  chan <- asks messageChan
  timeoutId <- getCurTimeoutId
  modify $ \s -> s { overridingTimeout = True }
  liftIO $ forkIO $ do
    threadDelay (duration * 1000000)
    writeChan chan (TimeoutMessage timeoutId)
  res <- f
  bumpTimeoutId timeoutId
  modify $ \s -> s { overridingTimeout = False }
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
