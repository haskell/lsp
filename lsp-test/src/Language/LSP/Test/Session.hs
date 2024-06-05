{- ORMOLU_DISABLE -}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module Language.LSP.Test.Session
  ( Session(..)
  , SessionConfig(..)
  , defaultConfig
  , SessionMessage(..)
  , SessionContext(..)
  , SessionState(..)
  , runSession'
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
  , documentChangeUri
  )

where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail
#endif
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens ()
import Data.Conduit as Conduit
import Data.Conduit.Parser as Parser
import Data.Default
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Function
import Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message as LSP
import Language.LSP.VFS
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
#ifndef mingw32_HOST_OS
import System.Process (waitForProcess)
#endif
import System.Timeout ( timeout )
import Data.IORef
import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Data.String (fromString)
import Data.Either (partitionEithers)

-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session a = Session (ConduitParser FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadThrow)

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
  , lspConfig      :: Object
  -- ^ The initial LSP config as JSON object, defaults to the empty object.
  -- This should include the config section for the server if it has one, i.e. if
  -- the server has a 'mylang' config section, then the config should be an object
  -- with a 'mylang' key whose value is the actual config for the server. You
  -- can also include other config sections if your server may request those.
  , ignoreLogNotifications :: Bool
  -- ^ Whether or not to ignore @window/showMessage@ and @window/logMessage@ notifications
  -- from the server, defaults to True.
  , ignoreConfigurationRequests :: Bool
  -- ^ Whether or not to ignore @workspace/configuration@ requests from the server,
  -- defaults to True.
  , ignoreRegistrationRequests :: Bool
  -- ^ Whether or not to ignore @client/registerCapability@ and @client/unregisterCapability@
  -- requests from the server, defaults to True.
  , initialWorkspaceFolders :: Maybe [WorkspaceFolder]
  -- ^ The initial workspace folders to send in the @initialize@ request.
  -- Defaults to Nothing.
  }

-- | The configuration used in 'Language.LSP.Test.runSession'.
defaultConfig :: SessionConfig
defaultConfig = SessionConfig 60 False False True mempty True True True Nothing

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
  , curTimeoutId :: IORef Int -- ^ The current timeout we are waiting on
  , requestMap :: MVar RequestMap
  , initRsp :: MVar (TResponseMessage Method_Initialize)
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
getCurTimeoutId = asks curTimeoutId >>= liftIO . readIORef

-- Pass this the timeoutid you *were* waiting on
bumpTimeoutId :: (HasReader SessionContext m, MonadIO m) => Int -> m ()
bumpTimeoutId prev = do
  v <- asks curTimeoutId
  -- when updating the curtimeoutid, account for the fact that something else
  -- might have bumped the timeoutid in the meantime
  liftIO $ atomicModifyIORef' v (\x -> (max x (prev + 1), ()))

data SessionState = SessionState
  {
    curReqId :: !Int32
  , vfs :: !VFS
  , curDiagnostics :: !(Map.Map NormalizedUri [Diagnostic])
  , overridingTimeout :: !Bool
  -- ^ The last received message from the server.
  -- Used for providing exception information
  , lastReceivedMessage :: !(Maybe FromServerMessage)
  , curDynCaps :: !(Map.Map T.Text SomeRegistration)
  -- ^ The capabilities that the server has dynamically registered with us so
  -- far
  , curLspConfig :: Object
  , curProgressSessions :: !(Set.Set ProgressToken)
  , ignoringLogNotifications :: Bool
  , ignoringConfigurationRequests :: Bool
  , ignoringRegistrationRequests :: Bool
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

runSessionMonad :: SessionContext -> SessionState -> Session a -> IO (a, SessionState)
runSessionMonad context state (Session session) = runReaderT (runStateT conduit state) context
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
      curId <- getCurTimeoutId
      case msg of
        ServerMessage sMsg -> yield sMsg
        TimeoutMessage tId -> when (curId == tId) $ get >>= throw . Timeout . lastReceivedMessage

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSession' :: Handle -- ^ Server in
            -> Handle -- ^ Server out
            -> Maybe ProcessHandle -- ^ Server process
            -> (Handle -> SessionContext -> IO ()) -- ^ Server listener
            -> SessionConfig
            -> ClientCapabilities
            -> FilePath -- ^ Root directory
            -> Session () -- ^ To exit the Server properly
            -> Session a
            -> IO a
runSession' serverIn serverOut mServerProc serverHandler config caps rootDir exitServer session = do
  absRootDir <- canonicalizePath rootDir

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering
  -- This is required to make sure that we don’t get any
  -- newline conversion or weird encoding issues.
  hSetBinaryMode serverIn True
  hSetBinaryMode serverOut True

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  timeoutIdVar <- newIORef 0
  initRsp <- newEmptyMVar

  mainThreadId <- myThreadId

  let context = SessionContext
        serverIn
        absRootDir
        messageChan
        timeoutIdVar
        reqMap
        initRsp
        config
        caps
      initState = SessionState
        0
        emptyVFS
        mempty
        False
        Nothing
        mempty
        (lspConfig config)
        mempty
        (ignoreLogNotifications config)
        (ignoreConfigurationRequests config)
        (ignoreRegistrationRequests config)
      runSession' = runSessionMonad context initState

      errorHandler = throwTo mainThreadId :: SessionException -> IO ()
      serverListenerLauncher =
        forkIO $ catch (serverHandler serverOut context) errorHandler
      msgTimeoutMs = messageTimeout config * 10^6
      serverAndListenerFinalizer tid = do
        let cleanup
              | Just sp <- mServerProc = do
                  -- Give the server some time to exit cleanly
                  -- It makes the server hangs in windows so we have to avoid it
#ifndef mingw32_HOST_OS
                  timeout msgTimeoutMs (waitForProcess sp)
#endif
                  cleanupProcess (Just serverIn, Just serverOut, Nothing, sp)
              | otherwise = pure ()
        finally (timeout msgTimeoutMs (runSession' exitServer))
                -- Make sure to kill the listener first, before closing
                -- handles etc via cleanupProcess
                (killThread tid >> cleanup)

  (result, _) <- bracket serverListenerLauncher
                         serverAndListenerFinalizer
                         (const $ runSessionMonad context initState session)
  return result

updateStateC :: ConduitM FromServerMessage FromServerMessage (StateT SessionState (ReaderT SessionContext IO)) ()
updateStateC = awaitForever $ \msg -> do
  state <- get @SessionState
  updateState msg
  case msg of
    FromServerMess SMethod_WindowWorkDoneProgressCreate req ->
      sendMessage $ TResponseMessage "2.0" (Just $ req ^. L.id) (Right Null)
    FromServerMess SMethod_WorkspaceApplyEdit r -> do
      sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) (Right $ ApplyWorkspaceEditResult True Nothing Nothing)
    FromServerMess SMethod_WorkspaceConfiguration r -> do
      let requestedSections = mapMaybe (\i -> i ^? L.section . _Just) $ r ^. L.params . L.items
      let o = curLspConfig state
      -- check for each requested section whether we have it
      let configsOrErrs = flip fmap requestedSections $ \section ->
            case o ^. at (fromString $ T.unpack section) of
              Just config -> Right config
              Nothing -> Left section

      let (errs, configs) = partitionEithers configsOrErrs

      -- we have to return exactly the number of sections requested, so if we can't find all of them then that's an error
      sendMessage $ TResponseMessage "2.0" (Just $ r ^. L.id) $
        if null errs
        then Right configs
        else Left $ TResponseError (InL LSPErrorCodes_RequestFailed) ("No configuration for requested sections: " <> T.pack (show errs)) Nothing
    _ -> pure ()
  unless (
    (ignoringLogNotifications state && isLogNotification msg)
    || (ignoringConfigurationRequests state && isConfigRequest msg)
    || (ignoringRegistrationRequests state && isRegistrationRequest msg)) $
    yield msg

  where

    isLogNotification (FromServerMess SMethod_WindowShowMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowLogMessage _) = True
    isLogNotification (FromServerMess SMethod_WindowShowDocument _) = True
    isLogNotification _ = False

    isConfigRequest (FromServerMess SMethod_WorkspaceConfiguration _) = True
    isConfigRequest _ = False

    isRegistrationRequest (FromServerMess SMethod_ClientRegisterCapability _) = True
    isRegistrationRequest (FromServerMess SMethod_ClientUnregisterCapability _) = True
    isRegistrationRequest _ = False

-- extract Uri out from DocumentChange
-- didn't put this in `lsp-types` because TH was getting in the way
documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. L.textDocument . L.uri
documentChangeUri (InR (InL x)) = x ^. L.uri
documentChangeUri (InR (InR (InL x))) = x ^. L.oldUri
documentChangeUri (InR (InR (InR x))) = x ^. L.uri

updateState :: (MonadIO m, HasReader SessionContext m, HasState SessionState m)
            => FromServerMessage -> m ()
updateState (FromServerMess SMethod_Progress req) = case req ^. L.params . L.value of
  v | Just _ <- v ^? _workDoneProgressBegin ->
    modify $ \s -> s { curProgressSessions = Set.insert (req ^. L.params . L.token) $ curProgressSessions s }
  v | Just _ <- v ^? _workDoneProgressEnd ->
    modify $ \s -> s { curProgressSessions = Set.delete (req ^. L.params . L.token) $ curProgressSessions s }
  _ -> pure ()

-- Keep track of dynamic capability registration
updateState (FromServerMess SMethod_ClientRegisterCapability req) = do
  let
    regs :: [SomeRegistration]
    regs = req ^.. L.params . L.registrations . traversed . to toSomeRegistration . _Just
  let newRegs = (\sr@(SomeRegistration r) -> (r ^. L.id, sr)) <$> regs
  modify $ \s ->
    s { curDynCaps = Map.union (Map.fromList newRegs) (curDynCaps s) }

updateState (FromServerMess SMethod_ClientUnregisterCapability req) = do
  let unRegs = (^. L.id) <$> req ^. L.params . L.unregisterations
  modify $ \s ->
    let newCurDynCaps = foldr' Map.delete (curDynCaps s) unRegs
    in s { curDynCaps = newCurDynCaps }

updateState (FromServerMess SMethod_TextDocumentPublishDiagnostics n) = do
  let diags = n ^. L.params . L.diagnostics
      doc = n ^. L.params . L.uri
  modify $ \s ->
    let newDiags = Map.insert (toNormalizedUri doc) diags (curDiagnostics s)
      in s { curDiagnostics = newDiags }

updateState (FromServerMess SMethod_WorkspaceApplyEdit r) = do

  -- First, prefer the versioned documentChanges field
  allChangeParams <- case r ^. L.params . L.edit . L.documentChanges of
    Just cs -> do
      mapM_ (checkIfNeedsOpened . documentChangeUri) cs
      -- replace the user provided version numbers with the VFS ones + 1
      -- (technically we should check that the user versions match the VFS ones)
      cs' <- traverseOf (traverse . _L . L.textDocument . _versionedTextDocumentIdentifier) bumpNewestVersion cs
      return $ mapMaybe getParamsFromDocumentChange cs'
    -- Then fall back to the changes field
    Nothing -> case r ^. L.params . L.edit . L.changes of
      Just cs -> do
        mapM_ checkIfNeedsOpened (Map.keys cs)
        concat <$> mapM (uncurry getChangeParams) (Map.toList cs)
      Nothing ->
        error "WorkspaceEdit contains neither documentChanges nor changes!"

  modifyM $ \s -> do
    let newVFS = flip execState (vfs s) $ changeFromServerVFS logger r
    return $ s { vfs = newVFS }

  let groupedParams = groupBy (\a b -> a ^. L.textDocument == b ^. L.textDocument) allChangeParams
      mergedParams = map mergeParams groupedParams

  -- TODO: Don't do this when replaying a session
  forM_ mergedParams (sendMessage . TNotificationMessage "2.0" SMethod_TextDocumentDidChange)

  -- Update VFS to new document versions
  let sortedVersions = map (sortBy (compare `on` (^. L.textDocument . L.version))) groupedParams
      latestVersions = map ((^. L.textDocument) . last) sortedVersions

  forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
    modify $ \s ->
      let oldVFS = vfs s
          update (VirtualFile _ file_ver t) = VirtualFile v (file_ver +1) t
          newVFS = oldVFS & vfsMap . ix (toNormalizedUri uri) %~ update
      in s { vfs = newVFS }

  where
        logger = LogAction $ \(WithSeverity msg sev) -> case sev of { Error -> error $ show msg; _ -> pure () }
        checkIfNeedsOpened uri = do
          oldVFS <- vfs <$> get

          -- if its not open, open it
          unless (has (vfsMap . ix (toNormalizedUri uri)) oldVFS) $ do
            let fp = fromJust $ uriToFilePath uri
            contents <- liftIO $ T.readFile fp
            let item = TextDocumentItem (filePathToUri fp) "" 0 contents
                msg = TNotificationMessage "2.0" SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams item)
            sendMessage msg

            modifyM $ \s -> do
              let newVFS = flip execState (vfs s) $ openVFS logger msg
              return $ s { vfs = newVFS }

        getParamsFromTextDocumentEdit :: TextDocumentEdit -> Maybe DidChangeTextDocumentParams
        getParamsFromTextDocumentEdit (TextDocumentEdit docId edits) =
          DidChangeTextDocumentParams <$> docId ^? _versionedTextDocumentIdentifier <*> pure (map editToChangeEvent edits)

        -- TODO: move somewhere reusable
        editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
        editToChangeEvent (InR e) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial { _range = e ^. L.range , _rangeLength = Nothing , _text = e ^. L.newText }
        editToChangeEvent (InL e) = TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial { _range = e ^. L.range , _rangeLength = Nothing , _text = e ^. L.newText }

        getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
        getParamsFromDocumentChange (InL textDocumentEdit) = getParamsFromTextDocumentEdit textDocumentEdit
        getParamsFromDocumentChange _ = Nothing

        bumpNewestVersion (VersionedTextDocumentIdentifier uri _) =
          head <$> textDocumentVersions uri

        -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
        -- where n is the current version
        textDocumentVersions uri = do
          vfs <- vfs <$> get
          let curVer = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . lsp_version
          pure $ map (VersionedTextDocumentIdentifier uri) [curVer + 1..]

        textDocumentEdits uri edits = do
          vers <- textDocumentVersions uri
          pure $ zipWith (\v e -> TextDocumentEdit (review _versionedTextDocumentIdentifier v) [InL e]) vers edits

        getChangeParams uri edits = do
          edits <- textDocumentEdits uri (reverse edits)
          pure $ mapMaybe getParamsFromTextDocumentEdit edits

        mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
        mergeParams params = let events = concat (toList (map (toList . (^. L.contentChanges)) params))
                              in DidChangeTextDocumentParams (head params ^. L.textDocument) events
updateState _ = return ()

sendMessage :: (MonadIO m, HasReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- serverIn <$> ask
  logMsg LogClient msg
  liftIO $ B.hPut h (addHeader $ encode msg) `catch` (throw . MessageSendError (toJSON msg))

-- | Execute a block f that will throw a 'Language.LSP.Test.Exception.Timeout' exception
-- after duration seconds. This will override the global timeout
-- for waiting for messages to arrive defined in 'SessionConfig'.
withTimeout :: Int -> Session a -> Session a
withTimeout duration f = do
  chan <- asks messageChan
  timeoutId <- getCurTimeoutId
  modify $ \s -> s { overridingTimeout = True }
  tid <- liftIO $ forkIO $ do
    threadDelay (duration * 1000000)
    writeChan chan (TimeoutMessage timeoutId)
  res <- f
  liftIO $ killThread tid
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
