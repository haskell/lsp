{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE BinaryLiterals       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}


module Language.Haskell.LSP.Core (
    processMessage
  , VFSData(..)
  , InitializeCallbacks(..)

  -- * Handlers
  , Handlers
  , Handler

  , Options(..)
  
  -- * LspT and LspM
  , LspT(..)
  , LspM
  , LanguageContextEnv(..)

  , clientCapabilities
  , config
  , rootPath
  , workspaceFolders

  , sendRequest
  , sendNotification
  
  -- * VFS
  , getVirtualFile
  , getVirtualFiles
  , persistVirtualFile
  , getVersionedTextDoc
  , reverseFileMap
  
  -- * Diagnostics
  , publishDiagnostics
  , flushDiagnosticsBySource
  
  -- * Progress
  , withProgress
  , withIndefiniteProgress
  , Progress(..)
  , ProgressCancellable(..)
  , ProgressCancelledException
  
  -- * Dynamic registration
  , registerCapability
  , unregisterCapability
  , RegistrationToken
  
  , setupLogger
  , reverseSortEdit
  , initializeRequestHandler
  , runReaderT
  , FromServerMessage
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Lens ( (<&>), (^.), (^?), _Just )
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Functor.Product
import           Data.IxMap
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Map (DMap)
import qualified Data.HashMap.Strict as HM
import           Data.Kind
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid hiding (Product)
import qualified Data.Text as T
import           Data.Text ( Text )
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.UUID as UUID
import qualified Language.Haskell.LSP.Types.Capabilities    as J
import Language.Haskell.LSP.Types as J hiding (Progress)
import qualified Language.Haskell.LSP.Types.Lens as J
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Diagnostics
import           System.Directory
import           System.Exit
import           System.IO
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import           System.Log.Logger
import qualified System.Log.Logger as L
import           System.Random

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
  
newtype LspT config m a = LspT { runLspT :: ReaderT (LanguageContextEnv config) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadTransControl, MonadFix)

instance MonadBase b m => MonadBase b (LspT config m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (LspT config m) where
    type StM (LspT config m) a = ComposeSt (LspT config) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type LspM config = LspT config IO

data LanguageContextEnv config =
  LanguageContextEnv
  { resHandlers            :: !(Handlers config)
  , resParseConfig         :: !(J.Value -> LspM config (Either T.Text config))
  , resSendMessage         :: !(FromServerMessage -> IO ())
  , resState               :: !(TVar (LanguageContextState config))
  , resClientCapabilities  :: !J.ClientCapabilities
  , resRootPath            :: !(Maybe FilePath)
  }

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

-- | A mapping from methods to the static 'Handler's that should be used to
-- handle responses when they come in from the client.
type Handlers config = forall t (m :: Method FromClient t). SMethod m -> Maybe (Handler m config)

-- | The type of a handler that handles requests and notifications coming in
-- from the server or client
type family Handler (m :: Method p t) (config :: Type) = (result :: Type) | result -> config t m where
  Handler (m :: Method p Request)      config = RequestMessage m -> (Either ResponseError (ResponseParams m) -> LspM config ()) -> LspM config ()
  Handler (m :: Method p Notification) config = NotificationMessage m -> LspM config ()

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextState config =
  LanguageContextState
  { resVFS                 :: !VFSData
  , resDiagnostics         :: !DiagnosticStore
  , resConfig              :: !(Maybe config)
  , resWorkspaceFolders    :: ![WorkspaceFolder]
  , resProgressData        :: !ProgressData
  , resPendingResponses    :: !(ResponseMap config)
  , resRegistrationsNot    :: !(RegistrationMap config Notification)
  , resRegistrationsReq    :: !(RegistrationMap config Request)
  , resLspId               :: !Int
  }

type ResponseMap config = IxMap LspId (Product SMethod (ServerResponseCallback config))

type RegistrationMap (config :: Type) (t :: MethodType) = DMap SMethod (Product RegistrationId (RegistrationHandler config t))

data RegistrationToken (m :: Method FromClient t) = RegistrationToken (SMethod m) (RegistrationId m)
newtype RegistrationId (m :: Method FromClient t) = RegistrationId Text
  deriving Eq

newtype RegistrationHandler config (t :: MethodType) (m :: Method FromClient t) = RegistrationHandler (Handler m config)

data ProgressData = ProgressData { progressNextId :: !Int
                                 , progressCancel :: !(Map.Map ProgressToken (IO ())) }

data VFSData =
  VFSData
    { vfsData :: !VFS
    , reverseMap :: !(Map.Map FilePath FilePath)
    }

modifyState :: (LanguageContextState config -> LanguageContextState config) -> LspM config ()
modifyState f = do
  tvarDat <- LspT $ asks resState
  liftIO $ atomically $ modifyTVar' tvarDat f

stateState :: (LanguageContextState config -> (a,LanguageContextState config)) -> LspM config a
stateState f = do
  tvarDat <- LspT $ asks resState
  liftIO $ atomically $ stateTVar tvarDat f

getsState :: (LanguageContextState config -> a) -> LspM config a
getsState f = do
  tvarDat <- LspT $ asks resState
  liftIO $ f <$> readTVarIO tvarDat

-- ---------------------------------------------------------------------

-- | Language Server Protocol options that the server may configure.
-- If you set handlers for some requests, you may need to set some of these options.
data Options =
  Options
    { textDocumentSync                 :: Maybe J.TextDocumentSyncOptions
    -- |  The characters that trigger completion automatically.
    , completionTriggerCharacters      :: Maybe [Char]
    -- | The list of all possible characters that commit a completion. This field can be used
    -- if clients don't support individual commmit characters per completion item. See
    -- `_commitCharactersSupport`.
    , completionAllCommitCharacters    :: Maybe [Char]
    -- | The characters that trigger signature help automatically.
    , signatureHelpTriggerCharacters   :: Maybe [Char]
    -- | List of characters that re-trigger signature help.
    -- These trigger characters are only active when signature help is already showing. All trigger characters
    -- are also counted as re-trigger characters.
    , signatureHelpRetriggerCharacters :: Maybe [Char]
    -- | CodeActionKinds that this server may return.
    -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    -- may list out every specific kind they provide.
    , codeActionKinds                  :: Maybe [CodeActionKind]
    -- | The list of characters that triggers on type formatting.
    -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
    -- The first character is mandatory, so a 'NonEmpty' should be passed.
    , documentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
    -- | The commands to be executed on the server.
    -- If you set `executeCommandHandler`, you **must** set this.
    , executeCommandCommands           :: Maybe [Text]
    -- | Information about the server that can be advertised to the client.
    , serverInfo                       :: Maybe J.ServerInfo
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing

-- | A package indicating the perecentage of progress complete and a
-- an optional message to go with it during a 'withProgress'
--
-- @since 0.10.0.0
data Progress = Progress (Maybe Double) (Maybe Text)

-- | Thrown if the user cancels a 'Cancellable' 'withProgress'/'withIndefiniteProgress'/ session
--
-- @since 0.11.0.0
data ProgressCancelledException = ProgressCancelledException
  deriving Show
instance E.Exception ProgressCancelledException

-- | Whether or not the user should be able to cancel a 'withProgress'/'withIndefiniteProgress'
-- session
--
-- @since 0.11.0.0
data ProgressCancellable = Cancellable | NotCancellable

-- | Contains all the callbacks to use for initialized the language server.
-- it is parameterized over a config type variable representing the type for the
-- specific configuration data the language server needs to use.
data InitializeCallbacks config =
  InitializeCallbacks
    { onInitialConfiguration :: InitializeRequest -> Either T.Text config
      -- ^ Invoked on the first message from the language client, containg the client configuration
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and can be
      -- accessed via 'config'.
    , onConfigurationChange :: J.Value -> LspM config (Either T.Text config)
      -- ^ @onConfigurationChange newConfig@ is called whenever the
      -- clients sends a message with a changed client configuration. This
      -- callback should return either the parsed configuration data or an error
      -- indicating what went wrong. The parsed configuration object will be
      -- stored internally and can be accessed via 'config'.
    , onStartup :: LspM config (Maybe ResponseError)
      -- ^ Once the initial configuration has been received, this callback will be invoked to offer
      -- the language server implementation the chance to create any processes or start new threads
      -- that may be necesary for the server lifecycle.
    }

-- | A function that a 'Handler' is passed that can be used to respond to a
-- request with either an error, or the response params.
newtype ServerResponseCallback config (m :: Method FromServer Request)
  = ServerResponseCallback (Either ResponseError (ResponseParams m) -> LspM config ())

-- | Return value signals if response handler was inserted succesfully
-- Might fail if the id was already in the map
addResponseHandler :: LspId m -> (Product SMethod (ServerResponseCallback config)) m -> LspM config Bool
addResponseHandler lid h = do
  stateState $ \ctx@LanguageContextState{resPendingResponses} ->
    case insertIxMap lid h resPendingResponses of
      Just m -> (True, ctx { resPendingResponses = m})
      Nothing -> (False, ctx)

sendNotification :: forall (m :: Method FromServer Notification) config. SServerMethod m -> MessageParams m -> LspM config ()
sendNotification m params =
  let msg = NotificationMessage "2.0" m params
  in case splitServerMethod m of
        IsServerNot -> sendToClient $ fromServerNot msg
        IsServerEither -> sendToClient $ FromServerMess m $ NotMess msg

sendRequest :: forall (m :: Method FromServer Request) config.
               SServerMethod m
            -> MessageParams m
            -> (Either ResponseError (ResponseParams m) -> LspM config ())
            -> LspM config (LspId m)
sendRequest m params resHandler = do
  reqId <- IdInt <$> freshLspId
  success <- addResponseHandler reqId (Pair m (ServerResponseCallback resHandler))
  unless success $ error "haskell-lsp: could not send FromServer request as id is reused"

  let msg = RequestMessage "2.0" reqId m params
  ~() <- case splitServerMethod m of
    IsServerReq -> sendToClient $ fromServerReq msg
    IsServerEither -> sendToClient $ FromServerMess m $ ReqMess msg
  return reqId

-- ---------------------------------------------------------------------

-- | Invokes the registered dynamic or static handlers for the given message and
-- method, as well as doing some bookkeeping.
handle :: SClientMethod m -> ClientMessage m -> LspM config ()
handle m msg =
  case m of
    SWorkspaceDidChangeWorkspaceFolders -> handle' (Just updateWorkspaceFolders) m msg
    SWorkspaceDidChangeConfiguration    -> handle' (Just handleConfigChange) m msg
    STextDocumentDidOpen                -> handle' (Just $ vfsFunc openVFS) m msg
    STextDocumentDidChange              -> handle' (Just $ vfsFunc changeFromClientVFS) m msg
    STextDocumentDidClose               -> handle' (Just $ vfsFunc closeVFS) m msg
    SWindowWorkDoneProgressCancel       -> handle' (Just progressCancelHandler) m msg
    _ -> handle' Nothing m msg


handle' :: forall t (m :: Method FromClient t) (config :: Type).
           Maybe (ClientMessage m -> LspM config ())
           -- ^ An action to be run before invoking the handler, used for
           -- bookkeeping stuff like the vfs etc.
        -> SClientMethod m
        -> ClientMessage m
        -> LspM config ()
handle' mAction m msg = do
  maybe (return ()) (\f -> f msg) mAction
  
  dynReqHandlers <- getsState resRegistrationsReq
  dynNotHandlers <- getsState resRegistrationsNot
  staticHandlers <- LspT $ asks resHandlers
  let mStaticHandler = staticHandlers m
  
  case splitClientMethod m of
    IsClientNot -> case pickHandler dynNotHandlers mStaticHandler of
      Just h -> h msg
      Nothing
        | SExit <- m -> exitNotificationHandler msg
        | otherwise -> reportMissingHandler

    IsClientReq -> case pickHandler dynReqHandlers mStaticHandler of
      Just h -> h msg (mkRspCb msg)
      Nothing
        | SShutdown <- m -> shutdownRequestHandler msg (mkRspCb msg)
        | otherwise -> reportMissingHandler

    IsClientEither -> case msg of
      NotMess noti -> case pickHandler dynNotHandlers mStaticHandler of
        Just h -> h noti
        Nothing -> reportMissingHandler
      ReqMess req -> case pickHandler dynReqHandlers mStaticHandler of
        Just h -> h req (mkRspCb req)
        Nothing -> reportMissingHandler
  where
    -- | Checks to see if there's a dynamic handler, and uses it in favour of the
    -- static handler, if it exists.
    pickHandler :: RegistrationMap config t -> Maybe (Handler m config) -> Maybe (Handler m config)
    pickHandler dynHandlerMap mStaticHandler = case (DMap.lookup m dynHandlerMap, mStaticHandler) of
      (Just (Pair _ (RegistrationHandler h)), _) -> Just h
      (Nothing, Just h) -> Just h
      (Nothing, Nothing) -> Nothing
    
    -- '$/' notifications should/could be ignored by server.
    -- Don't log errors in that case.
    -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
    reportMissingHandler :: LspM config ()
    reportMissingHandler
      | isOptionalNotification m = return ()
      | otherwise = do
          let errorMsg = T.pack $ unwords ["haskell-lsp:no handler for: ", show m]
          sendErrorLog errorMsg
    isOptionalNotification (SCustomMethod method)
      | "$/" `T.isPrefixOf` method = True
    isOptionalNotification _  = False

    -- | Makes the callback function passed to a 'Handler'
    mkRspCb :: RequestMessage (m1 :: Method FromClient Request)
            -> ((Either ResponseError (ResponseParams m1))
            -> LspM config ())
    mkRspCb req (Left  err) = sendToClient $
      FromServerRsp (req ^. J.method) $ ResponseMessage "2.0" (Just (req ^. J.id)) (Left err)
    mkRspCb req (Right rsp) = sendToClient $ 
      FromServerRsp (req ^. J.method) $ ResponseMessage "2.0" (Just (req ^. J.id)) (Right rsp)

handleConfigChange :: DidChangeConfigurationNotification -> LspM config ()
handleConfigChange req = do
  parseConfig <- LspT $ asks resParseConfig
  res <- parseConfig (req ^. J.params . J.settings)
  case res of
    Left err -> do
      let msg = T.pack $ unwords
            ["haskell-lsp:configuration parse error.", show req, show err]
      sendErrorLog msg
    Right newConfig ->
      modifyState $ \ctx -> ctx { resConfig = Just newConfig }

vfsFunc :: (VFS -> b -> (VFS, [String])) -> b -> LspM config ()
vfsFunc modifyVfs req = do
  join $ stateState $ \ctx@LanguageContextState{resVFS = VFSData vfs rm} ->
    let (vfs', ls) = modifyVfs vfs req
    in (liftIO $ mapM_ (debugM "haskell-lsp.vfsFunc") ls,ctx{ resVFS = VFSData vfs' rm})

-- | Updates the list of workspace folders
updateWorkspaceFolders :: Message WorkspaceDidChangeWorkspaceFolders -> LspM config ()
updateWorkspaceFolders (NotificationMessage _ _ params) = do
  let List toRemove = params ^. J.event . J.removed
      List toAdd = params ^. J.event . J.added
      newWfs oldWfs = foldr L.delete oldWfs toRemove <> toAdd
  modifyState $ \c -> c {resWorkspaceFolders = newWfs $ resWorkspaceFolders c}

-- ---------------------------------------------------------------------

-- | Return the 'VirtualFile' associated with a given 'NormalizedUri', if there is one.
getVirtualFile :: NormalizedUri -> LspM config (Maybe VirtualFile)
getVirtualFile uri = getsState $ Map.lookup uri . vfsMap . vfsData . resVFS

getVirtualFiles :: LspM config VFS
getVirtualFiles = getsState $ vfsData . resVFS

-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: NormalizedUri -> LspM config (Maybe FilePath)
persistVirtualFile uri = do
  join $ stateState $ \ctx@LanguageContextState{resVFS = vfs} ->
    case persistFileVFS (vfsData vfs) uri of
      Nothing -> (return Nothing, ctx)
      Just (fn, write) ->
        let revMap = case uriToFilePath (fromNormalizedUri uri) of
              Just uri_fp -> Map.insert fn uri_fp $ reverseMap vfs
              -- TODO: Does the VFS make sense for URIs which are not files?
              -- The reverse map should perhaps be (FilePath -> URI)
              Nothing -> reverseMap vfs
            act = do
              liftIO write
              pure (Just fn)
        in (act, ctx{resVFS = vfs {reverseMap = revMap} })

-- | Given a text document identifier, annotate it with the latest version.
getVersionedTextDoc :: TextDocumentIdentifier -> LspM config VersionedTextDocumentIdentifier
getVersionedTextDoc doc = do
  let uri = doc ^. J.uri
  mvf <- getVirtualFile (toNormalizedUri uri)
  let ver = case mvf of
        Just (VirtualFile lspver _ _) -> Just lspver
        Nothing -> Nothing
  return (VersionedTextDocumentIdentifier uri ver)

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: LspM config (FilePath -> FilePath)
reverseFileMap = do
  vfs <- getsState resVFS
  let f fp = fromMaybe fp . Map.lookup fp . reverseMap $ vfs
  return f

-- ---------------------------------------------------------------------

-- | The current configuration from the client as set via the @initialize@ and
-- @workspace/didChangeConfiguration@ requests.
config :: LspM config (Maybe config)
config = getsState resConfig

-- ---------------------------------------------------------------------

defaultProgressData :: ProgressData
defaultProgressData = ProgressData 0 Map.empty

-- ---------------------------------------------------------------------

processMessage :: BSL.ByteString -> LspM config ()
processMessage jsonStr = do
  tvarDat <- LspT $ asks resState
  join $ liftIO $ atomically $ fmap handleErrors $ runExceptT $ do
      val <- except $ J.eitherDecode jsonStr
      ctx <- lift   $ readTVar tvarDat
      msg <- except $ J.parseEither (parser $ resPendingResponses ctx) val
      lift $ case msg of
        FromClientMess m mess ->
          pure $ handle m mess
        FromClientRsp (Pair (ServerResponseCallback f) (Const newMap)) res -> do
          modifyTVar' tvarDat (\c -> c { resPendingResponses = newMap })
          pure $ f (res ^. J.result)
  where
    parser :: ResponseMap config -> J.Value -> J.Parser (FromClientMessage' (Product (ServerResponseCallback config) (Const (ResponseMap config))))
    parser rm = parseClientMessage $ \i ->
      let (mhandler, newMap) = pickFromIxMap i rm
        in (\(Pair m handler) -> (m,Pair handler (Const newMap))) <$> mhandler

    handleErrors = either (sendErrorLog . errMsg) id

    errMsg err = TL.toStrict $ TL.unwords
      [ "haskell-lsp:incoming message parse error."
      , TL.decodeUtf8 jsonStr
      , TL.pack err
      ] <> "\n"


-- ---------------------------------------------------------------------

sendToClient :: FromServerMessage -> LspM config ()
sendToClient msg = do
  f <- LspT $ asks resSendMessage
  liftIO $ f msg

-- ---------------------------------------------------------------------

sendErrorLog :: Text -> LspM config ()
sendErrorLog msg =
  sendToClient $ fromServerNot $
    NotificationMessage "2.0" SWindowLogMessage (LogMessageParams MtError msg)

-- ---------------------------------------------------------------------

initializeErrorHandler :: (ResponseError -> IO ()) -> E.SomeException -> IO (Maybe a)
initializeErrorHandler sendResp e = do
    sendResp $ ResponseError InternalError msg Nothing
    pure Nothing
  where
    msg = T.pack $ unwords ["Error on initialize:", show e]

-- |=====================================================================
--
-- Handlers

freshLspId :: LspM config Int
freshLspId = do
  stateState $ \c ->
    (resLspId c, c{resLspId = resLspId c+1})

-- | Call this to initialize the session
initializeRequestHandler
  :: InitializeCallbacks config
  -> VFS
  -> (Handlers config)
  -> Options
  -> (FromServerMessage -> IO ())
  -> Message Initialize
  -> IO (Maybe (LanguageContextEnv config))
initializeRequestHandler InitializeCallbacks{..} vfs handlers options sendFunc req = do
  let sendResp = sendFunc . FromServerRsp SInitialize
  flip E.catch (initializeErrorHandler $ sendResp . makeResponseError (req ^. J.id)) $ do

    let params = req ^. J.params

    let rootDir = getFirst $ foldMap First [ params ^. J.rootUri  >>= uriToFilePath
                                           , params ^. J.rootPath <&> T.unpack ]

    case rootDir of
      Nothing -> return ()
      Just dir -> do
        debugM "haskell-lsp.initializeRequestHandler" $ "Setting current dir to project root:" ++ dir
        unless (null dir) $ setCurrentDirectory dir

    let initialWfs = case params ^. J.workspaceFolders of
          Just (List xs) -> xs
          Nothing -> []
        initialConfigRes = onInitialConfiguration req
        initialConfig = either (const Nothing) Just initialConfigRes

    tvarCtx <- newTVarIO $
      LanguageContextState
        (VFSData vfs mempty)
        mempty
        initialConfig
        initialWfs
        defaultProgressData
        emptyIxMap
        mempty
        mempty
        0

    -- Launch the given process once the project root directory has been set
    let env = LanguageContextEnv handlers onConfigurationChange sendFunc tvarCtx (params ^. J.capabilities) rootDir

    initializationResult <- flip runReaderT env $ runLspT onStartup
    case initializationResult of
      Just errResp -> do
        sendResp $ makeResponseError (req ^. J.id) errResp
      Nothing -> do
        let serverCaps = inferServerCapabilities (params ^. J.capabilities) options handlers
        sendResp $ makeResponseMessage (req ^. J.id) (InitializeResult serverCaps (serverInfo options))


    case initialConfigRes of
      Right _ -> pure ()
      Left err -> do
        let msg = T.pack $ unwords
              ["haskell-lsp:configuration parse error.", show req, show err]
        runReaderT (runLspT (sendErrorLog msg)) env

    return $ Just env
  
  where 
    makeResponseMessage rid result = ResponseMessage "2.0" (Just rid) (Right result)
    makeResponseError origId err = ResponseMessage "2.0" (Just origId) (Left err)

clientCapabilities :: LspM config J.ClientCapabilities
clientCapabilities = LspT $ asks resClientCapabilities

rootPath :: LspM config (Maybe FilePath)
rootPath = LspT $ asks resRootPath

-- | The current workspace folders, if the client supports workspace folders.
workspaceFolders :: LspM config (Maybe [WorkspaceFolder])
workspaceFolders = do
  clientCaps <- clientCapabilities
  let clientSupportsWfs = fromMaybe False $ do
        let (J.ClientCapabilities mw _ _ _) = clientCaps
        (J.WorkspaceClientCapabilities _ _ _ _ _ _ mwf _) <- mw
        mwf
  if clientSupportsWfs
    then Just <$> getsState resWorkspaceFolders
    else pure Nothing

-- | Sends a @client/registerCapability@ request and dynamically registers
-- a 'Method' with a 'Handler'. Returns 'Nothing' if the client does not
-- support dynamic registration for the specified method, otherwise a
-- 'RegistrationToken' which can be used to unregister it later.
registerCapability :: forall (config :: Type) t (m :: Method FromClient t).
                      SClientMethod m
                   -> RegistrationOptions m
                   -> Handler m config
                   -> LspM config (Maybe (RegistrationToken m))
registerCapability method regOpts f = do
  clientCaps <- LspT $ asks resClientCapabilities
  handlers <- LspT $ asks resHandlers
  let alreadyStaticallyRegistered = isJust $ handlers method
  go clientCaps alreadyStaticallyRegistered
  where
    -- If the server has already registered statically, don't dynamically register
    -- as per the spec
    go _clientCaps True = pure Nothing
    go clientCaps  False
      -- First, check to see if the client supports dynamic registration on this method
      | dynamicSupported clientCaps = do
          uuid <- liftIO $ UUID.toText <$> getStdRandom random
          let registration = J.Registration uuid method regOpts
              params = J.RegistrationParams (J.List [J.SomeRegistration registration])
              regId = RegistrationId uuid
              pair = Pair regId (RegistrationHandler f)
          
          ~() <- case splitClientMethod method of
            IsClientNot -> modifyState $ \ctx ->
              let newRegs = DMap.insert method pair (resRegistrationsNot ctx)
                in ctx { resRegistrationsNot = newRegs }
            IsClientReq -> modifyState $ \ctx ->
              let newRegs = DMap.insert method pair (resRegistrationsReq ctx)
                in ctx { resRegistrationsReq = newRegs }
            IsClientEither -> error "Cannot register capability for custom methods"
          
          -- TODO: handle the scenario where this returns an error
          _ <- sendRequest SClientRegisterCapability params $ \_res -> pure ()

          pure (Just (RegistrationToken method regId))
      | otherwise        = pure Nothing
      
    -- Also I'm thinking we should move this function to somewhere in messages.hs so
    -- we don't forget to update it when adding new methods...
    capDyn :: J.HasDynamicRegistration a (Maybe Bool) => Maybe a -> Bool
    capDyn (Just x) = fromMaybe False $ x ^. J.dynamicRegistration
    capDyn Nothing  = False
  
    -- | Checks if client capabilities declares that the method supports dynamic registration
    dynamicSupported clientCaps = case method of
      SWorkspaceDidChangeConfiguration -> capDyn $ clientCaps ^? J.workspace . _Just . J.didChangeConfiguration . _Just
      SWorkspaceDidChangeWatchedFiles  -> capDyn $ clientCaps ^? J.workspace . _Just . J.didChangeWatchedFiles . _Just
      SWorkspaceSymbol                 -> capDyn $ clientCaps ^? J.workspace . _Just . J.symbol . _Just
      SWorkspaceExecuteCommand         -> capDyn $ clientCaps ^? J.workspace . _Just . J.executeCommand . _Just
      STextDocumentDidOpen             -> capDyn $ clientCaps ^? J.textDocument . _Just . J.synchronization . _Just
      STextDocumentDidChange           -> capDyn $ clientCaps ^? J.textDocument . _Just . J.synchronization . _Just
      STextDocumentDidClose            -> capDyn $ clientCaps ^? J.textDocument . _Just . J.synchronization . _Just
      STextDocumentCompletion          -> capDyn $ clientCaps ^? J.textDocument . _Just . J.completion . _Just
      STextDocumentHover               -> capDyn $ clientCaps ^? J.textDocument . _Just . J.hover . _Just
      STextDocumentSignatureHelp       -> capDyn $ clientCaps ^? J.textDocument . _Just . J.signatureHelp . _Just
      STextDocumentDeclaration         -> capDyn $ clientCaps ^? J.textDocument . _Just . J.declaration . _Just
      STextDocumentDefinition          -> capDyn $ clientCaps ^? J.textDocument . _Just . J.definition . _Just
      STextDocumentTypeDefinition      -> capDyn $ clientCaps ^? J.textDocument . _Just . J.typeDefinition . _Just
      STextDocumentImplementation      -> capDyn $ clientCaps ^? J.textDocument . _Just . J.implementation . _Just
      STextDocumentReferences          -> capDyn $ clientCaps ^? J.textDocument . _Just . J.references . _Just
      STextDocumentDocumentHighlight   -> capDyn $ clientCaps ^? J.textDocument . _Just . J.documentHighlight . _Just
      STextDocumentDocumentSymbol      -> capDyn $ clientCaps ^? J.textDocument . _Just . J.documentSymbol . _Just
      STextDocumentCodeAction          -> capDyn $ clientCaps ^? J.textDocument . _Just . J.codeAction . _Just
      STextDocumentCodeLens            -> capDyn $ clientCaps ^? J.textDocument . _Just . J.codeLens . _Just
      STextDocumentDocumentLink        -> capDyn $ clientCaps ^? J.textDocument . _Just . J.documentLink . _Just
      STextDocumentDocumentColor       -> capDyn $ clientCaps ^? J.textDocument . _Just . J.colorProvider . _Just
      STextDocumentColorPresentation   -> capDyn $ clientCaps ^? J.textDocument . _Just . J.colorProvider . _Just
      STextDocumentFormatting          -> capDyn $ clientCaps ^? J.textDocument . _Just . J.formatting . _Just
      STextDocumentRangeFormatting     -> capDyn $ clientCaps ^? J.textDocument . _Just . J.rangeFormatting . _Just
      STextDocumentOnTypeFormatting    -> capDyn $ clientCaps ^? J.textDocument . _Just . J.onTypeFormatting . _Just
      STextDocumentRename              -> capDyn $ clientCaps ^? J.textDocument . _Just . J.rename . _Just
      STextDocumentFoldingRange        -> capDyn $ clientCaps ^? J.textDocument . _Just . J.foldingRange . _Just
      STextDocumentSelectionRange      -> capDyn $ clientCaps ^? J.textDocument . _Just . J.selectionRange . _Just
      _                                -> False
  
-- | Sends a @client/unregisterCapability@ request and removes the handler
-- for that associated registration.
unregisterCapability :: RegistrationToken m -> LspM config ()
unregisterCapability (RegistrationToken m (RegistrationId uuid)) = do
  ~() <- case splitClientMethod m of
    IsClientReq -> do
      reqRegs <- getsState resRegistrationsReq
      let newMap = DMap.delete m reqRegs
      modifyState (\ctx -> ctx { resRegistrationsReq = newMap })
    IsClientNot -> do
      notRegs <- getsState resRegistrationsNot
      let newMap = DMap.delete m notRegs
      modifyState (\ctx -> ctx { resRegistrationsNot = newMap })
    IsClientEither -> error "Cannot unregister capability for custom methods"

  let unregistration = J.Unregistration uuid (J.SomeClientMethod m)
      params = J.UnregistrationParams (J.List [unregistration])
  void $ sendRequest SClientUnregisterCapability params $ \_res -> pure ()

--------------------------------------------------------------------------------
-- PROGRESS
--------------------------------------------------------------------------------

storeProgress :: ProgressToken -> Async a -> LspM config ()
storeProgress n a = do
  let f = Map.insert n (cancelWith a ProgressCancelledException) . progressCancel
  modifyState $ \ctx -> ctx { resProgressData = (resProgressData ctx) { progressCancel = f (resProgressData ctx)}}

deleteProgress :: ProgressToken -> LspM config ()
deleteProgress n = do
  let f = Map.delete n . progressCancel
  modifyState $ \ctx -> ctx { resProgressData = (resProgressData ctx) { progressCancel = f (resProgressData ctx)}}

-- Get a new id for the progress session and make a new one
getNewProgressId :: LspM config ProgressToken
getNewProgressId = do
  stateState $ \ctx@LanguageContextState{resProgressData} ->
    let x = progressNextId resProgressData
        ctx' = ctx { resProgressData = resProgressData { progressNextId = x + 1 }}
    in (ProgressNumericToken x, ctx')

withProgressBase :: Bool -> Text -> ProgressCancellable -> ((Progress -> LspM c ()) -> LspM c a) -> LspM c a
withProgressBase indefinite title cancellable f = do
  env <- LspT ask
  let sf x = runReaderT (runLspT (sendToClient x)) env

  progId <- getNewProgressId

  let initialPercentage
        | indefinite = Nothing
        | otherwise = Just 0
      cancellable' = case cancellable of
                      Cancellable -> True
                      NotCancellable -> False

  -- Create progress token
  -- FIXME  : This needs to wait until the request returns before
  -- continuing!!!
  _ <- sendRequest SWindowWorkDoneProgressCreate
        (WorkDoneProgressCreateParams progId) $ \res -> do
          case res of
            -- An error ocurred when the client was setting it up
            -- No need to do anything then, as per the spec
            Left _err -> pure ()
            Right () -> pure ()

  -- Send initial notification
  sendNotification SProgress $
    fmap Begin $ ProgressParams progId $
      WorkDoneProgressBeginParams title (Just cancellable') Nothing initialPercentage

  aid <- liftBaseWith $ \runInBase ->
    async $ runInBase $ f (updater progId (sf . fromServerNot))
  storeProgress progId aid
  res <- liftIO $ wait aid

  -- Send done notification
  sendNotification SProgress $
    End <$> (ProgressParams progId (WorkDoneProgressEndParams Nothing))
  -- Delete the progress cancellation from the map
  -- If we don't do this then it's easy to leak things as the map contains any IO action.
  deleteProgress progId


  return res
  where updater progId sf (Progress percentage msg) =
          liftIO $ sf $ NotificationMessage "2.0" SProgress $
            fmap Report $ ProgressParams progId $
              WorkDoneProgressReportParams Nothing msg percentage

clientSupportsProgress :: J.ClientCapabilities -> Bool
clientSupportsProgress (J.ClientCapabilities _ _ wc _) = fromMaybe False $ do
  (J.WindowClientCapabilities mProgress) <- wc
  mProgress

-- | Wrapper for reporting progress to the client during a long running
-- task.
-- 'withProgress' @title cancellable f@ starts a new progress reporting
-- session, and finishes it once f is completed.
-- f is provided with an update function that allows it to report on
-- the progress during the session.
-- If @cancellable@ is 'Cancellable', @f@ will be thrown a
-- 'ProgressCancelledException' if the user cancels the action in
-- progress.
withProgress :: Text -> ProgressCancellable -> ((Progress -> LspM config ()) -> LspM config a) -> LspM config a
withProgress title cancellable f = do
  clientCaps <- clientCapabilities
  if clientSupportsProgress clientCaps
    then withProgressBase False title cancellable f
    else f (const $ return ())
  where

-- | Same as 'withProgress', but for processes that do not report the
-- precentage complete.
--
-- @since 0.10.0.0
withIndefiniteProgress :: Text -> ProgressCancellable -> LspM config a -> LspM config a
withIndefiniteProgress title cancellable f = do
  clientCaps <- clientCapabilities
  if clientSupportsProgress clientCaps
    then withProgressBase True title cancellable (const f)
    else f

-- | Infers the capabilities based on registered handlers, and sets the appropriate options.
-- A provider should be set to Nothing if the server does not support it, unless it is a
-- static option.
inferServerCapabilities :: J.ClientCapabilities -> Options -> Handlers config -> J.ServerCapabilities
inferServerCapabilities clientCaps o h =
  J.ServerCapabilities
    { J._textDocumentSync                 = sync
    , J._hoverProvider                    = supportedBool J.STextDocumentHover
    , J._completionProvider               = completionProvider
    , J._declarationProvider              = supportedBool J.STextDocumentDeclaration
    , J._signatureHelpProvider            = signatureHelpProvider
    , J._definitionProvider               = supportedBool J.STextDocumentDefinition
    , J._typeDefinitionProvider           = supportedBool J.STextDocumentTypeDefinition
    , J._implementationProvider           = supportedBool J.STextDocumentImplementation
    , J._referencesProvider               = supportedBool J.STextDocumentReferences
    , J._documentHighlightProvider        = supportedBool J.STextDocumentDocumentHighlight
    , J._documentSymbolProvider           = supportedBool J.STextDocumentDocumentSymbol
    , J._codeActionProvider               = codeActionProvider
    , J._codeLensProvider                 = supported' J.STextDocumentCodeLens $ J.CodeLensOptions
                                              (Just False)
                                              (supported J.SCodeLensResolve)
    , J._documentFormattingProvider       = supportedBool J.STextDocumentFormatting
    , J._documentRangeFormattingProvider  = supportedBool J.STextDocumentRangeFormatting
    , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , J._renameProvider                   = supportedBool J.STextDocumentRename
    , J._documentLinkProvider             = supported' J.STextDocumentDocumentLink $ J.DocumentLinkOptions
                                              (Just False)
                                              (supported J.SDocumentLinkResolve)
    , J._colorProvider                    = supportedBool J.STextDocumentDocumentColor
    , J._foldingRangeProvider             = supportedBool J.STextDocumentFoldingRange
    , J._executeCommandProvider           = executeCommandProvider
    , J._selectionRangeProvider           = supportedBool J.STextDocumentSelectionRange
    , J._workspaceSymbolProvider          = supported J.SWorkspaceSymbol
    , J._workspace                        = Just workspace
    -- TODO: Add something for experimental
    , J._experimental                     = Nothing :: Maybe J.Value
    }
  where
    
    -- | For when we just return a simple @true@/@false@ to indicate if we
    -- support the capability
    supportedBool = Just . J.L . supported_b

    supported' m b
      | supported_b m = Just b
      | otherwise = Nothing

    supported :: forall m. J.SClientMethod m -> Maybe Bool
    supported = Just . supported_b

    supported_b :: forall m. J.SClientMethod m -> Bool
    supported_b m = isJust (h m)

    singleton :: a -> [a]
    singleton x = [x]

    completionProvider
      | supported_b J.STextDocumentCompletion = Just $
          J.CompletionOptions
            Nothing
            (map singleton <$> completionTriggerCharacters o)
            (map singleton <$> completionAllCommitCharacters o)
            (supported J.SCompletionItemResolve)
      | otherwise = Nothing

    clientSupportsCodeActionKinds = isJust $
      clientCaps ^? J.textDocument . _Just . J.codeAction . _Just . J.codeActionLiteralSupport

    codeActionProvider
      | clientSupportsCodeActionKinds
      , supported_b J.STextDocumentCodeAction = Just $
          maybe (J.L True) (J.R . J.CodeActionOptions Nothing . Just . J.List)
                (codeActionKinds o)
      | supported_b J.STextDocumentCodeAction = Just (J.L True)
      | otherwise = Just (J.L False)

    signatureHelpProvider
      | supported_b J.STextDocumentSignatureHelp = Just $
          J.SignatureHelpOptions
            Nothing
            (J.List . map singleton <$> signatureHelpTriggerCharacters o)
            (J.List . map singleton <$> signatureHelpRetriggerCharacters o)
      | otherwise = Nothing

    documentOnTypeFormattingProvider
      | supported_b J.STextDocumentOnTypeFormatting
      , Just (first :| rest) <- documentOnTypeFormattingTriggerCharacters o = Just $
          J.DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singleton) rest))
      | supported_b J.STextDocumentOnTypeFormatting
      , Nothing <- documentOnTypeFormattingTriggerCharacters o =
          error "documentOnTypeFormattingTriggerCharacters needs to be set if a documentOnTypeFormattingHandler is set"
      | otherwise = Nothing

    executeCommandProvider
      | supported_b J.SWorkspaceExecuteCommand
      , Just cmds <- executeCommandCommands o = Just (J.ExecuteCommandOptions Nothing (J.List cmds))
      | supported_b J.SWorkspaceExecuteCommand
      , Nothing <- executeCommandCommands o =
          error "executeCommandCommands needs to be set if a executeCommandHandler is set"
      | otherwise = Nothing

    sync = case textDocumentSync o of
            Just x -> Just (J.L x)
            Nothing -> Nothing

    workspace = J.WorkspaceServerCapabilities workspaceFolder
    workspaceFolder = supported' J.SWorkspaceDidChangeWorkspaceFolders $
        -- sign up to receive notifications
        J.WorkspaceFoldersServerCapabilities (Just True) (Just (J.R True))

progressCancelHandler :: J.WorkDoneProgressCancelNotification -> LspM config ()
progressCancelHandler (J.NotificationMessage _ _ (J.WorkDoneProgressCancelParams tid)) = do
  mact <- getsState $ Map.lookup tid . progressCancel . resProgressData
  case mact of
    Nothing -> return ()
    Just cancelAction -> liftIO $ cancelAction

exitNotificationHandler :: Handler J.Exit c
exitNotificationHandler =  \_ -> liftIO $ do
  noticeM "haskell-lsp.exitNotificationHandler" "Got exit, exiting"
  exitSuccess

-- | Default Shutdown handler
shutdownRequestHandler :: Handler J.Shutdown c
shutdownRequestHandler = \_req k -> do
  k $ Right J.Empty

-- ---------------------------------------------------------------------

-- | Aggregate all diagnostics pertaining to a particular version of a document,
-- by source, and sends a @textDocument/publishDiagnostics@ notification with
-- the total (limited by the first parameter) whenever it is updated.
publishDiagnostics :: Int -> NormalizedUri -> TextDocumentVersion -> DiagnosticsBySource -> LspM config ()
publishDiagnostics maxDiagnosticCount uri version diags = join $ stateState $ \ctx ->
  let ds = updateDiagnostics (resDiagnostics ctx) uri version diags
      ctx' = ctx{resDiagnostics = ds}
      mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
      act = case mdp of
        Nothing -> return ()
        Just params ->
          sendToClient $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params
      in (act,ctx')

-- ---------------------------------------------------------------------

-- | Remove all diagnostics from a particular source, and send the updates to
-- the client.
flushDiagnosticsBySource :: Int -- ^ Max number of diagnostics to send
                         -> Maybe DiagnosticSource -> LspM config ()
flushDiagnosticsBySource maxDiagnosticCount msource = join $ stateState $ \ctx ->
  let ds = flushBySource (resDiagnostics ctx) msource
      ctx' = ctx {resDiagnostics = ds}
      -- Send the updated diagnostics to the client
      act = forM_ (HM.keys ds) $ \uri -> do
        let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
        case mdp of
          Nothing -> return ()
          Just params -> do
            sendToClient $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params
      in (act,ctx')

-- =====================================================================
--
--  utility


--
--  Logger
--
setupLogger :: Maybe FilePath -> [String] -> Priority -> IO ()
setupLogger mLogFile extraLogNames level = do

  logStream <- case mLogFile of
    Just logFile -> openFile logFile AppendMode
    Nothing      -> return stderr
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormatter  = L.tfLogFormatter logDateFormat logFormat
      logHandler = LH.setFormatter logHandle logFormatter

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger "haskell-lsp" $ L.setHandlers [logHandler]
  L.updateGlobalLogger "haskell-lsp" $ L.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    L.updateGlobalLogger logName $ L.setHandlers [logHandler]
    L.updateGlobalLogger logName $ L.setLevel level
  where
    logFormat = "$time [$tid] $prio $loggername:\t$msg"
    logDateFormat = "%Y-%m-%d %H:%M:%S%Q"

-- ---------------------------------------------------------------------

-- | The changes in a workspace edit should be applied from the end of the file
-- toward the start. Sort them into this order.
reverseSortEdit :: J.WorkspaceEdit -> J.WorkspaceEdit
reverseSortEdit (J.WorkspaceEdit cs dcs) = J.WorkspaceEdit cs' dcs'
  where
    cs' :: Maybe J.WorkspaceEditMap
    cs' = (fmap . fmap ) sortTextEdits cs

    dcs' :: Maybe (J.List J.TextDocumentEdit)
    dcs' = (fmap . fmap ) sortTextDocumentEdits dcs

    sortTextEdits :: J.List J.TextEdit -> J.List J.TextEdit
    sortTextEdits (J.List edits) = J.List (L.sortBy down edits)

    sortTextDocumentEdits :: J.TextDocumentEdit -> J.TextDocumentEdit
    sortTextDocumentEdits (J.TextDocumentEdit td (J.List edits)) = J.TextDocumentEdit td (J.List edits')
      where
        edits' = L.sortBy down edits

    down (J.TextEdit r1 _) (J.TextEdit r2 _) = r2 `compare` r1
