{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Language.LSP.Server.Core where

import           Colog.Core                             (LogAction (..),
                                                         Severity (..),
                                                         WithSeverity (..),
                                                         (<&))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception                      as E
import           Control.Lens                           (_Just, at, (^.), (^?))
import           Control.Monad
import           Control.Monad.Catch                    (MonadCatch, MonadMask,
                                                         MonadThrow)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                             as J
import           Data.Default
import           Data.Functor.Product
import qualified Data.HashMap.Strict                    as HM
import           Data.IxMap
import           Data.Kind
import qualified Data.List                              as L
import           Data.List.NonEmpty                     (NonEmpty (..))
import qualified Data.Map.Strict                        as Map
import           Data.Maybe
import           Data.Monoid                            (Ap (..))
import           Data.Ord                               (Down (Down))
import           Data.Row
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.UUID                              as UUID
import           Language.LSP.Diagnostics
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens             as L
import           Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Message          as L
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types            as L
import           Language.LSP.Protocol.Utils.SMethodMap (SMethodMap)
import qualified Language.LSP.Protocol.Utils.SMethodMap as SMethodMap
import           Language.LSP.Protocol.Utils.Misc (prettyJSON)
import           Language.LSP.VFS
import           Prettyprinter
import           System.Random                          hiding (next)

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

data LspCoreLog =
  -- TODO: arguably it would be nicer to have the config object itself in there, but
  -- then we're going to need 'Pretty config' constraints everywhere
  NewConfig J.Value
  | ConfigurationParseError J.Value T.Text
  | ConfigurationNotSupported
  | BadConfigurationResponse ResponseError
  | WrongConfigSections [J.Value]
  deriving Show

instance Pretty LspCoreLog where
  pretty (NewConfig config) = "LSP: set new config:" <+> prettyJSON config
  pretty (ConfigurationNotSupported) = "LSP: not requesting configuration since the client does not support workspace/configuration"
  pretty (ConfigurationParseError settings err) =
    vsep [
      "LSP: configuration parse error:"
      , pretty err
      , "when parsing"
      , prettyJSON settings
      ]
  pretty (BadConfigurationResponse err) = "LSP: error when requesting configuration: " <+> pretty err
  pretty (WrongConfigSections sections) = "LSP: expected only one configuration section, got: " <+> (prettyJSON $ J.toJSON sections)

newtype LspT config m a = LspT { unLspT :: ReaderT (LanguageContextEnv config) m a }
  deriving (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix)
  deriving (Semigroup, Monoid) via (Ap (LspT config m) a)

-- for deriving the instance of MonadUnliftIO
type role LspT representational representational nominal

runLspT :: LanguageContextEnv config -> LspT config m a -> m a
runLspT env = flip runReaderT env . unLspT

{-# INLINE runLspT #-}

type LspM config = LspT config IO

class MonadUnliftIO m => MonadLsp config m | m -> config where
  getLspEnv :: m (LanguageContextEnv config)

instance MonadUnliftIO m => MonadLsp config (LspT config m) where
  {-# SPECIALIZE instance MonadLsp config (LspT config IO) #-}
  {-# INLINE getLspEnv #-}
  getLspEnv = LspT ask

instance MonadLsp c m => MonadLsp c (ReaderT r m) where
  {-# SPECIALIZE instance MonadLsp config (ReaderT r (LspT config IO)) #-}
  {-# INLINE getLspEnv #-}
  getLspEnv = lift getLspEnv

instance MonadLsp c m => MonadLsp c (IdentityT m) where
  getLspEnv = lift getLspEnv

data LanguageContextEnv config =
  LanguageContextEnv
  { resHandlers           :: !(Handlers IO)
  , resConfigSection      :: T.Text
  , resParseConfig        :: !(config -> J.Value -> Either T.Text config)
  , resOnConfigChange     :: !(config -> IO ())
  , resSendMessage        :: !(FromServerMessage -> IO ())
  -- We keep the state in a TVar to be thread safe
  , resState              :: !(LanguageContextState config)
  , resClientCapabilities :: !L.ClientCapabilities
  , resRootPath           :: !(Maybe FilePath)
  }

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

-- | A mapping from methods to the static 'Handler's that should be used to
-- handle responses when they come in from the client. To build up a 'Handlers',
-- you should 'mconcat' a list of 'notificationHandler' and 'requestHandler's:
--
-- @
-- mconcat [
--   notificationHandler SInitialized $ \notif -> pure ()
-- , requestHandler STextDocumentHover $ \req responder -> pure ()
-- ]
-- @
data Handlers m
  = Handlers
  { reqHandlers :: !(SMethodMap (ClientMessageHandler m Request))
  , notHandlers :: !(SMethodMap (ClientMessageHandler m Notification))
  }
instance Semigroup (Handlers config) where
  Handlers r1 n1 <> Handlers r2 n2 = Handlers (r1 <> r2) (n1 <> n2)
instance Monoid (Handlers config) where
  mempty = Handlers mempty mempty

notificationHandler :: forall (m :: Method ClientToServer Notification) f. SMethod m -> Handler f m -> Handlers f
notificationHandler m h = Handlers mempty (SMethodMap.singleton m (ClientMessageHandler h))

requestHandler :: forall (m :: Method ClientToServer Request) f. SMethod m -> Handler f m -> Handlers f
requestHandler m h = Handlers (SMethodMap.singleton m (ClientMessageHandler h)) mempty

-- | Wrapper to restrict 'Handler's to  ClientToServer' 'Method's
newtype ClientMessageHandler f (t :: MessageKind) (m :: Method ClientToServer t) = ClientMessageHandler (Handler f m)

-- | The type of a handler that handles requests and notifications coming in
-- from the server or client
type family Handler (f :: Type -> Type) (m :: Method from t) = (result :: Type) | result -> f t m where
  Handler f (m :: Method _from Request)      = TRequestMessage m -> (Either ResponseError (MessageResult m) -> f ()) -> f ()
  Handler f (m :: Method _from Notification) = TNotificationMessage m -> f ()

-- | How to convert two isomorphic data structures between each other.
data m <~> n
  = Iso
  { forward  :: forall a. m a -> n a
  , backward :: forall a. n a -> m a
  }

transmuteHandlers :: (m <~> n) -> Handlers m -> Handlers n
transmuteHandlers nat = mapHandlers (\i m k -> forward nat (i m (backward nat . k))) (\i m -> forward nat (i m))

mapHandlers
  :: (forall (a :: Method ClientToServer Request). Handler m a -> Handler n a)
  -> (forall (a :: Method ClientToServer Notification). Handler m a -> Handler n a)
  -> Handlers m -> Handlers n
mapHandlers mapReq mapNot (Handlers reqs nots) = Handlers reqs' nots'
  where
    reqs' = SMethodMap.map (\(ClientMessageHandler i) -> ClientMessageHandler $ mapReq i) reqs
    nots' = SMethodMap.map (\(ClientMessageHandler i) -> ClientMessageHandler $ mapNot i) nots

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextState config =
  LanguageContextState
  { resVFS              :: !(TVar VFSData)
  , resDiagnostics      :: !(TVar DiagnosticStore)
  , resConfig           :: !(TVar config)
  , resWorkspaceFolders :: !(TVar [WorkspaceFolder])
  , resProgressData     :: !ProgressData
  , resPendingResponses :: !(TVar ResponseMap)
  , resRegistrationsNot :: !(TVar (RegistrationMap Notification))
  , resRegistrationsReq :: !(TVar (RegistrationMap Request))
  , resLspId            :: !(TVar Int32)
  }

type ResponseMap = IxMap LspId (Product SMethod ServerResponseCallback)

type RegistrationMap (t :: MessageKind) = SMethodMap (Product RegistrationId (ClientMessageHandler IO t))

data RegistrationToken (m :: Method ClientToServer t) = RegistrationToken (SMethod m) (RegistrationId m)
newtype RegistrationId (m :: Method ClientToServer t) = RegistrationId Text
  deriving Eq

data ProgressData = ProgressData { progressNextId :: !(TVar Int32)
                                 , progressCancel :: !(TVar (Map.Map ProgressToken (IO ()))) }

data VFSData =
  VFSData
    { vfsData    :: !VFS
    , reverseMap :: !(Map.Map FilePath FilePath)
    }

{-# INLINE modifyState #-}
modifyState :: MonadLsp config m => (LanguageContextState config -> TVar a) -> (a -> a) -> m ()
modifyState sel f = do
  tvarDat <- sel . resState <$> getLspEnv
  liftIO $ atomically $ modifyTVar' tvarDat f

{-# INLINE stateState #-}
stateState :: MonadLsp config m => (LanguageContextState config -> TVar s) -> (s -> (a,s)) -> m a
stateState sel f = do
  tvarDat <- sel . resState <$> getLspEnv
  liftIO $ atomically $ stateTVar tvarDat f

{-# INLINE getsState #-}
getsState :: MonadLsp config m => (LanguageContextState config -> TVar a) -> m a
getsState f = do
  tvarDat <- f . resState <$> getLspEnv
  liftIO $ readTVarIO tvarDat

-- ---------------------------------------------------------------------

-- | Language Server Protocol options that the server may configure.
-- If you set handlers for some requests, you may need to set some of these options.
data Options =
  Options
    { optTextDocumentSync                 :: Maybe L.TextDocumentSyncOptions
    -- |  The characters that trigger completion automatically.
    , optCompletionTriggerCharacters      :: Maybe [Char]
    -- | The list of all possible characters that commit a completion. This field can be used
    -- if clients don't support individual commit characters per completion item. See
    -- `_commitCharactersSupport`.
    , optCompletionAllCommitCharacters    :: Maybe [Char]
    -- | The characters that trigger signature help automatically.
    , optSignatureHelpTriggerCharacters   :: Maybe [Char]
    -- | List of characters that re-trigger signature help.
    -- These trigger characters are only active when signature help is already showing. All trigger characters
    -- are also counted as re-trigger characters.
    , optSignatureHelpRetriggerCharacters :: Maybe [Char]
    -- | CodeActionKinds that this server may return.
    -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    -- may list out every specific kind they provide.
    , optCodeActionKinds                  :: Maybe [CodeActionKind]
    -- | The list of characters that triggers on type formatting.
    -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
    -- The first character is mandatory, so a 'NonEmpty' should be passed.
    , optDocumentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
    -- | The commands to be executed on the server.
    -- If you set `executeCommandHandler`, you **must** set this.
    , optExecuteCommandCommands           :: Maybe [Text]
    -- | Information about the server that can be advertised to the client.
    , optServerInfo                       :: Maybe (Rec ("name" .== Text .+ "version" .== Maybe Text))
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing

defaultOptions :: Options
defaultOptions = def

-- | A package indicating the percentage of progress complete and a
-- an optional message to go with it during a 'withProgress'
--
-- @since 0.10.0.0
data ProgressAmount = ProgressAmount (Maybe UInt) (Maybe Text)

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

-- See Note [LSP configuration] for discussion of the configuration-related fields
-- | Contains all the callbacks to use for initialized the language server.
-- it is parameterized over a config type variable representing the type for the
-- specific configuration data the language server needs to use.
data ServerDefinition config = forall m a.
  ServerDefinition
    { defaultConfig :: config
      -- ^ The default value we initialize the config variable to.
    , configSection :: T.Text
      -- ^ The "config section" that this server uses. This is used to identify the settings
      -- that are relevant to the server.
    , parseConfig :: config -> J.Value -> Either T.Text config
      -- ^ @parseConfig oldConfig newConfigObject@ is called whenever we
      -- get updated configuration from the client.
      --
      -- @parseConfig@ is called on the object corresponding to the server's
      -- config section, it should not itself try to look for the config section.
      --
      -- Note that the 'J.Value' may represent only a partial object in the case where we
      -- are handling a @workspace/didChangeConfiguration@ request where the client sends
      -- only the changed settings. This is also the main circumstance where the old configuration
      -- argument is useful. It is generally fine for servers to ignore this case and just
      -- assume that the 'J.Value' represents a full new config and ignore the old configuration.
      -- This will only be problematic in the case of clients which behave as above and *also*
      -- don't support @workspace/configuration@, which is discouraged.
      --
    , onConfigChange :: config -> m ()
      -- ^ This callback is called any time the configuration is updated, with
      -- the new config. Servers that want to react to config changes should provide
      -- a callback here, it is not sufficient to just add e.g. a @workspace/didChangeConfiguration@
      -- handler.
    , doInitialize :: LanguageContextEnv config -> TMessage Method_Initialize -> IO (Either ResponseError a)
      -- ^ Called *after* receiving the @initialize@ request and *before*
      -- returning the response. This callback will be invoked to offer the
      -- language server implementation the chance to create any processes or
      -- start new threads that may be necessary for the server lifecycle. It can
      -- also return an error in the initialization if necessary.
    , staticHandlers :: ClientCapabilities -> Handlers m
      -- ^ Handlers for any methods you want to statically support.
      -- The handlers here cannot be unregistered during the server's lifetime
      -- and will be registered statically in the initialize request.
      -- The handlers provided can depend on the client capabilities, which
      -- are static across the lifetime of the server.
    , interpretHandler :: a -> (m <~> IO)
      -- ^ How to run the handlers in your own monad of choice, @m@.
      -- It is passed the result of 'doInitialize', so typically you will want
      -- to thread along the 'LanguageContextEnv' as well as any other state you
      -- need to run your monad. @m@ should most likely be built on top of
      -- 'LspT'.
      --
      -- @
      --  ServerDefinition { ...
      --  , doInitialize = \env _req -> pure $ Right env
      --  , interpretHandler = \env -> Iso
      --     (runLspT env) -- how to convert from IO ~> m
      --     liftIO        -- how to convert from m ~> IO
      --  }
      -- @
    , options :: Options
      -- ^ Configurable options for the server's capabilities.
    }

-- | A function that a 'Handler' is passed that can be used to respond to a
-- request with either an error, or the response params.
newtype ServerResponseCallback (m :: Method ServerToClient Request)
  = ServerResponseCallback (Either ResponseError (MessageResult m) -> IO ())

-- | Return value signals if response handler was inserted successfully
-- Might fail if the id was already in the map
addResponseHandler :: MonadLsp config f => LspId m -> (Product SMethod ServerResponseCallback) m -> f Bool
addResponseHandler lid h = do
  stateState resPendingResponses $ \pending ->
    case insertIxMap lid h pending of
      Just !m -> (True, m)
      Nothing -> (False, pending)

sendNotification
  :: forall (m :: Method ServerToClient Notification) f config. MonadLsp config f
  => SServerMethod m
  -> MessageParams m
  -> f ()
sendNotification m params =
  let msg = TNotificationMessage "2.0" m params
  in case splitServerMethod m of
        IsServerNot    -> sendToClient $ fromServerNot msg
        IsServerEither -> sendToClient $ FromServerMess m $ NotMess msg

sendRequest :: forall (m :: Method ServerToClient Request) f config. MonadLsp config f
            => SServerMethod m
            -> MessageParams m
            -> (Either ResponseError (MessageResult m) -> f ())
            -> f (LspId m)
sendRequest m params resHandler = do
  reqId <- IdInt <$> freshLspId
  rio <- askRunInIO
  success <- addResponseHandler reqId (Pair m (ServerResponseCallback (rio . resHandler)))
  unless success $ error "LSP: could not send FromServer request as id is reused"

  let msg = TRequestMessage "2.0" reqId m params
  ~() <- case splitServerMethod m of
    IsServerReq    -> sendToClient $ fromServerReq msg
    IsServerEither -> sendToClient $ FromServerMess m $ ReqMess msg
  return reqId

-- ---------------------------------------------------------------------

-- | Return the 'VirtualFile' associated with a given 'NormalizedUri', if there is one.
getVirtualFile :: MonadLsp config m => NormalizedUri -> m (Maybe VirtualFile)
getVirtualFile uri = do
  dat <- vfsData <$> getsState resVFS
  pure $ dat ^. vfsMap . at uri

{-# INLINE getVirtualFile #-}

getVirtualFiles :: MonadLsp config m => m VFS
getVirtualFiles = vfsData <$> getsState resVFS

{-# INLINE getVirtualFiles #-}

-- | Take an atomic snapshot of the current state of the virtual file system.
snapshotVirtualFiles :: LanguageContextEnv c -> STM VFS
snapshotVirtualFiles env = vfsData <$> readTVar (resVFS $ resState env)

{-# INLINE snapshotVirtualFiles #-}


-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: MonadLsp config m => LogAction m (WithSeverity VfsLog) -> NormalizedUri -> m (Maybe FilePath)
persistVirtualFile logger uri = do
  join $ stateState resVFS $ \vfs ->
    case persistFileVFS logger (vfsData vfs) uri of
      Nothing -> (return Nothing, vfs)
      Just (fn, write) ->
        let !revMap = case uriToFilePath (fromNormalizedUri uri) of
              Just uri_fp -> Map.insert fn uri_fp $ reverseMap vfs
              -- TODO: Does the VFS make sense for URIs which are not files?
              -- The reverse map should perhaps be (FilePath -> URI)
              Nothing     -> reverseMap vfs
            !vfs' = vfs {reverseMap = revMap}
            act = do
              write
              pure (Just fn)
        in (act, vfs')

-- | Given a text document identifier, annotate it with the latest version.
getVersionedTextDoc :: MonadLsp config m => TextDocumentIdentifier -> m VersionedTextDocumentIdentifier
getVersionedTextDoc doc = do
  let uri = doc ^. L.uri
  mvf <- getVirtualFile (toNormalizedUri uri)
  let ver = case mvf of
        Just (VirtualFile lspver _ _) -> lspver
        Nothing                       -> 0
  return (VersionedTextDocumentIdentifier uri ver)

{-# INLINE getVersionedTextDoc #-}

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: MonadLsp config m => m (FilePath -> FilePath)
reverseFileMap = do
  vfs <- getsState resVFS
  let f fp = fromMaybe fp . Map.lookup fp . reverseMap $ vfs
  return f

{-# INLINE reverseFileMap #-}

-- ---------------------------------------------------------------------

sendToClient :: MonadLsp config m => FromServerMessage -> m ()
sendToClient msg = do
  f <- resSendMessage <$> getLspEnv
  liftIO $ f msg

{-# INLINE sendToClient #-}

-- ---------------------------------------------------------------------

freshLspId :: MonadLsp config m => m Int32
freshLspId = do
  stateState resLspId $ \cur ->
    let !next = cur+1 in (cur, next)

{-# INLINE freshLspId #-}

-- ---------------------------------------------------------------------

-- | The current configuration from the client as set via the @initialize@ and
-- @workspace/didChangeConfiguration@ requests, as well as by calls to
-- 'setConfig'.
getConfig :: MonadLsp config m => m config
getConfig = getsState resConfig

{-# INLINE getConfig #-}

setConfig :: MonadLsp config m => config -> m ()
setConfig config = stateState resConfig (const ((), config))

{-# INLINE setConfig #-}

getClientCapabilities :: MonadLsp config m => m L.ClientCapabilities
getClientCapabilities = resClientCapabilities <$> getLspEnv

{-# INLINE getClientCapabilities #-}

getRootPath :: MonadLsp config m => m (Maybe FilePath)
getRootPath = resRootPath <$> getLspEnv

{-# INLINE getRootPath #-}

-- | The current workspace folders, if the client supports workspace folders.
getWorkspaceFolders :: MonadLsp config m => m (Maybe [WorkspaceFolder])
getWorkspaceFolders = do
  clientCaps <- getClientCapabilities
  let clientSupportsWfs = fromMaybe False $ clientCaps ^? L.workspace . _Just . L.workspaceFolders . _Just
  if clientSupportsWfs
    then Just <$> getsState resWorkspaceFolders
    else pure Nothing

{-# INLINE getWorkspaceFolders #-}

-- | Sends a @client/registerCapability@ request and dynamically registers
-- a 'Method' with a 'Handler'. Returns 'Nothing' if the client does not
-- support dynamic registration for the specified method, otherwise a
-- 'RegistrationToken' which can be used to unregister it later.
registerCapability :: forall f t (m :: Method ClientToServer t) config.
                      MonadLsp config f
                   => SClientMethod m
                   -> RegistrationOptions m
                   -> Handler f m
                   -> f (Maybe (RegistrationToken m))
registerCapability method regOpts f = do
  clientCaps <- resClientCapabilities <$> getLspEnv
  handlers <- resHandlers <$> getLspEnv
  let alreadyStaticallyRegistered = case splitClientMethod method of
        IsClientNot    -> SMethodMap.member method $ notHandlers handlers
        IsClientReq    -> SMethodMap.member method $ reqHandlers handlers
        IsClientEither -> error "Cannot register capability for custom methods"
  go clientCaps alreadyStaticallyRegistered
  where
    -- If the server has already registered statically, don't dynamically register
    -- as per the spec
    go _clientCaps True = pure Nothing
    go clientCaps  False
      -- First, check to see if the client supports dynamic registration on this method
      | dynamicRegistrationSupported method clientCaps = do
          uuid <- liftIO $ UUID.toText <$> getStdRandom random
          let registration = L.TRegistration uuid method (Just regOpts)
              params = L.RegistrationParams [toUntypedRegistration registration]
              regId = RegistrationId uuid
          rio <- askUnliftIO
          ~() <- case splitClientMethod method of
            IsClientNot -> modifyState resRegistrationsNot $ \oldRegs ->
              let pair = Pair regId (ClientMessageHandler (unliftIO rio . f))
                in SMethodMap.insert method pair oldRegs
            IsClientReq -> modifyState resRegistrationsReq $ \oldRegs ->
              let pair = Pair regId (ClientMessageHandler (\msg k -> unliftIO rio $ f msg (liftIO . k)))
                in SMethodMap.insert method pair oldRegs
            IsClientEither -> error "Cannot register capability for custom methods"

          -- TODO: handle the scenario where this returns an error
          _ <- sendRequest SMethod_ClientRegisterCapability params $ \_res -> pure ()

          pure (Just (RegistrationToken method regId))
      | otherwise        = pure Nothing

-- | Sends a @client/unregisterCapability@ request and removes the handler
-- for that associated registration.
unregisterCapability :: MonadLsp config f => RegistrationToken m -> f ()
unregisterCapability (RegistrationToken m (RegistrationId uuid)) = do
  ~() <- case splitClientMethod m of
    IsClientReq    -> modifyState resRegistrationsReq $ SMethodMap.delete m
    IsClientNot    -> modifyState resRegistrationsNot $ SMethodMap.delete m
    IsClientEither -> error "Cannot unregister capability for custom methods"

  let unregistration = L.TUnregistration uuid m
      params = L.UnregistrationParams [toUntypedUnregistration unregistration]
  void $ sendRequest SMethod_ClientUnregisterCapability params $ \_res -> pure ()

--------------------------------------------------------------------------------
-- PROGRESS
--------------------------------------------------------------------------------

storeProgress :: MonadLsp config m => ProgressToken -> Async a -> m ()
storeProgress n a = modifyState (progressCancel . resProgressData) $ Map.insert n (cancelWith a ProgressCancelledException)

{-# INLINE storeProgress #-}

deleteProgress :: MonadLsp config m => ProgressToken -> m ()
deleteProgress n = modifyState (progressCancel . resProgressData) $ Map.delete n

{-# INLINE deleteProgress #-}

-- Get a new id for the progress session and make a new one
getNewProgressId :: MonadLsp config m => m ProgressToken
getNewProgressId = do
  stateState (progressNextId . resProgressData) $ \cur ->
    let !next = cur+1
    in (L.ProgressToken $ L.InL cur, next)

{-# INLINE getNewProgressId #-}

withProgressBase :: MonadLsp c m => Bool -> Text -> ProgressCancellable -> ((ProgressAmount -> m ()) -> m a) -> m a
withProgressBase indefinite title cancellable f = do

  progId <- getNewProgressId

  let initialPercentage
        | indefinite = Nothing
        | otherwise = Just 0
      cancellable' = case cancellable of
                      Cancellable    -> True
                      NotCancellable -> False

  -- Create progress token
  -- FIXME  : This needs to wait until the request returns before
  -- continuing!!!
  _ <- sendRequest SMethod_WindowWorkDoneProgressCreate
        (WorkDoneProgressCreateParams progId) $ \res -> do
          case res of
            -- An error occurred when the client was setting it up
            -- No need to do anything then, as per the spec
            Left _err -> pure ()
            Right _   -> pure ()

  -- Send the begin and done notifications via 'bracket_' so that they are always fired
  res <- withRunInIO $ \runInBase ->
    E.bracket_
      -- Send begin notification
      (runInBase $ sendNotification SMethod_Progress $
        ProgressParams progId $ J.toJSON $
          WorkDoneProgressBegin L.AString title (Just cancellable') Nothing initialPercentage)

      -- Send end notification
      (runInBase $ sendNotification SMethod_Progress $
        ProgressParams progId $ J.toJSON $ (WorkDoneProgressEnd L.AString Nothing)) $ do

      -- Run f asynchronously
      aid <- async $ runInBase $ f (updater progId)
      runInBase $ storeProgress progId aid
      wait aid

  -- Delete the progress cancellation from the map
  -- If we don't do this then it's easy to leak things as the map contains any IO action.
  deleteProgress progId

  return res
  where updater progId (ProgressAmount percentage msg) = do
          sendNotification SMethod_Progress $ ProgressParams progId $ J.toJSON $
              WorkDoneProgressReport L.AString Nothing msg percentage

clientSupportsProgress :: L.ClientCapabilities -> Bool
clientSupportsProgress caps = fromMaybe False $ caps ^? L.window . _Just . L.workDoneProgress . _Just

{-# INLINE clientSupportsProgress #-}

-- | Wrapper for reporting progress to the client during a long running
-- task.
-- 'withProgress' @title cancellable f@ starts a new progress reporting
-- session, and finishes it once f is completed.
-- f is provided with an update function that allows it to report on
-- the progress during the session.
-- If @cancellable@ is 'Cancellable', @f@ will be thrown a
-- 'ProgressCancelledException' if the user cancels the action in
-- progress.
withProgress :: MonadLsp c m => Text -> ProgressCancellable -> ((ProgressAmount -> m ()) -> m a) -> m a
withProgress title cancellable f = do
  clientCaps <- getClientCapabilities
  if clientSupportsProgress clientCaps
    then withProgressBase False title cancellable f
    else f (const $ return ())

-- | Same as 'withProgress', but for processes that do not report the
-- precentage complete.
--
-- @since 0.10.0.0
withIndefiniteProgress :: MonadLsp c m => Text -> ProgressCancellable -> m a -> m a
withIndefiniteProgress title cancellable f = do
  clientCaps <- getClientCapabilities
  if clientSupportsProgress clientCaps
    then withProgressBase True title cancellable (const f)
    else f

-- ---------------------------------------------------------------------

-- | Aggregate all diagnostics pertaining to a particular version of a document,
-- by source, and sends a @textDocument/publishDiagnostics@ notification with
-- the total (limited by the first parameter) whenever it is updated.
publishDiagnostics :: MonadLsp config m => Int -> NormalizedUri -> Maybe L.Int32 -> DiagnosticsBySource -> m ()
publishDiagnostics maxDiagnosticCount uri version diags = join $ stateState resDiagnostics $ \oldDiags->
  let !newDiags = updateDiagnostics oldDiags uri version diags
      mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
      act = case mdp of
        Nothing -> return ()
        Just params ->
          sendToClient $ L.fromServerNot $ L.TNotificationMessage "2.0" L.SMethod_TextDocumentPublishDiagnostics params
      in (act,newDiags)

-- ---------------------------------------------------------------------

-- | Remove all diagnostics from a particular source, and send the updates to
-- the client.
flushDiagnosticsBySource :: MonadLsp config m => Int -- ^ Max number of diagnostics to send
                         -> Maybe Text -> m ()
flushDiagnosticsBySource maxDiagnosticCount msource = join $ stateState resDiagnostics $ \oldDiags ->
  let !newDiags = flushBySource oldDiags msource
      -- Send the updated diagnostics to the client
      act = forM_ (HM.keys newDiags) $ \uri -> do
        let mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
        case mdp of
          Nothing -> return ()
          Just params -> do
            sendToClient $ L.fromServerNot $ L.TNotificationMessage "2.0" L.SMethod_TextDocumentPublishDiagnostics params
      in (act,newDiags)

-- ---------------------------------------------------------------------

-- | The changes in a workspace edit should be applied from the end of the file
-- toward the start. Sort them into this order.
reverseSortEdit :: L.WorkspaceEdit -> L.WorkspaceEdit
reverseSortEdit (L.WorkspaceEdit cs dcs anns) = L.WorkspaceEdit cs' dcs' anns
  where
    cs' :: Maybe (Map.Map Uri [TextEdit])
    cs' = (fmap . fmap ) sortTextEdits cs

    dcs' :: Maybe [L.DocumentChange]
    dcs' = (fmap . fmap) sortOnlyTextDocumentEdits dcs

    sortTextEdits :: [L.TextEdit] -> [L.TextEdit]
    sortTextEdits edits = L.sortOn (Down . (^. L.range)) edits

    sortOnlyTextDocumentEdits :: L.DocumentChange -> L.DocumentChange
    sortOnlyTextDocumentEdits (L.InL (L.TextDocumentEdit td edits)) = L.InL $ L.TextDocumentEdit td edits'
      where
        edits' = L.sortOn (Down . editRange) edits
    sortOnlyTextDocumentEdits (L.InR others) = L.InR others

    editRange :: L.TextEdit L.|? L.AnnotatedTextEdit -> L.Range
    editRange (L.InR e) = e ^. L.range
    editRange (L.InL e) = e ^. L.range

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

-- | Given a new config object, try to update our config with it.
tryChangeConfig :: (m ~ LspM config) => LogAction m (WithSeverity LspCoreLog) -> J.Value -> m ()
tryChangeConfig logger newConfigObject = do
  parseCfg <- LspT $ asks resParseConfig
  res <- stateState resConfig $ \oldConfig -> case parseCfg oldConfig newConfigObject of
    Left err         -> (Left err, oldConfig)
    Right newConfig -> (Right newConfig, newConfig)
  case res of
    Left err -> do
      logger <& ConfigurationParseError newConfigObject err `WithSeverity` Warning
    Right newConfig -> do
      logger <& NewConfig newConfigObject `WithSeverity` Debug
      cb <- LspT $ asks resOnConfigChange
      liftIO $ cb newConfig

-- | Send a `worksapce/configuration` request to update the server's config.
--
-- This is called automatically in response to `workspace/didChangeConfiguration` notifications
-- from the client, so should not normally be called manually.
requestConfigUpdate :: (m ~ LspM config) => LogAction m (WithSeverity LspCoreLog) -> m ()
requestConfigUpdate logger = do
  caps <- LspT $ asks resClientCapabilities
  let supportsConfiguration = fromMaybe False $ caps ^? L.workspace . _Just . L.configuration . _Just
  if supportsConfiguration
  then do
    section <- LspT $ asks resConfigSection
    void $ sendRequest SMethod_WorkspaceConfiguration (ConfigurationParams [ConfigurationItem Nothing (Just section)]) $ \case
      Right [newConfigObject] -> tryChangeConfig logger newConfigObject
      Right sections -> logger <& WrongConfigSections sections `WithSeverity` Error
      Left err -> logger <& BadConfigurationResponse err `WithSeverity` Error
  else
    logger <& ConfigurationNotSupported `WithSeverity` Debug

{- Note [LSP configuration]
LSP configuration is a huge mess.
- The configuration model of the client is not specified
- Many of the configuration messages are not specified in what they should return

In particular, configuration appears in three places:
1. The `initializationOptions` field of the `initialize` request.
  - The contents of this are unspecified. "User provided initialization options".
2. The `settings` field of the `workspace/didChangeConfiguration` notification.
  - The contents of this are unspecified. "The actual changed settings".
3. The `section` field of the response to the `workspace/configuration` request.
  - This at least says it should be the settings corresponding to the sections
    specified in the request.

It's very hard to know what to do here. In particular, the first two cases seem
like they could include arbitrary configuration from the client that might not
relate to you. How you locate "your" settings is unclear.

We are on firmer ground with case 3. Then at least it seems that we can pick
a configuration section, just always ask for that, and require clients to use
that for our settings. Furthermore, this is the method that is encouraged by the
specification designers: https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-420589320.

For this reason we mostly try and rely on `workspace/configuration`. That means
three things:
- We require servers to give a specific configuration section for us to use
  when requesting configuration.
- We can try and make sense of `initializationOptions`, but regardless we should
  send a `workspace/configuration` request afterwards (in the handler for the
  `initialized` notification, which is the earliest we can send messages:
  https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-953772465)
- We can try and make sense of `didChangeConfiguration`, but regardless we should
  send a `workspace/configuration` request afterwards

We do try to make sense of the first two cases also, especially because clients do
not have to support `workspace/configuration`! In practice,
many clients seem to follow the sensible approach laid out here:
https://github.com/microsoft/language-server-protocol/issues/972#issuecomment-626668243

To make this work, we try to be tolerant by using the following strategy.
When we receive a configuration object from any of the sources above, we first
check to see if it has a field corresponding to our configuration section. If it
does, then we assume that it our config and try to parse it. If it does not, we
try to parse the entire config object. This hopefully lets us handle a variety
of sensible cases where the client sends us mostly our config, either wrapped
in our section or not.
-}
