{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CUSKs #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Language.LSP.Server.Core where

import Colog.Core (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  (<&),
 )
import Control.Concurrent.Extra as C
import Control.Concurrent.STM
import Control.Lens (at, (^.), (^?), _Just)
import Control.Monad
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Aeson qualified as J
import Data.Default
import Data.Functor.Product
import Data.HashMap.Strict qualified as HM
import Data.IxMap
import Data.Kind
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (Ap (..))
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Capabilities
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Message qualified as L
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Protocol.Utils.Misc (prettyJSON)
import Language.LSP.Protocol.Utils.SMethodMap (SMethodMap)
import Language.LSP.Protocol.Utils.SMethodMap qualified as SMethodMap
import Language.LSP.VFS hiding (end)
import Prettyprinter

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- ---------------------------------------------------------------------

data LspCoreLog
  = -- TODO: arguably it would be nicer to have the config object itself in there, but
    -- then we're going to need 'Pretty config' constraints everywhere
    NewConfig J.Value
  | ConfigurationParseError J.Value T.Text
  | ConfigurationNotSupported
  | BadConfigurationResponse (TResponseError Method_WorkspaceConfiguration)
  | WrongConfigSections [J.Value]
  | forall m. CantRegister (SMethod m)

deriving instance (Show LspCoreLog)

instance Pretty LspCoreLog where
  pretty (NewConfig config) = "LSP: set new config:" <+> prettyJSON config
  pretty ConfigurationNotSupported = "LSP: not requesting configuration since the client does not support workspace/configuration"
  pretty (ConfigurationParseError settings err) =
    vsep
      [ "LSP: configuration parse error:"
      , pretty err
      , "when parsing"
      , prettyJSON settings
      ]
  pretty (BadConfigurationResponse err) = "LSP: error when requesting configuration: " <+> pretty err
  pretty (WrongConfigSections sections) = "LSP: expected only one configuration section, got: " <+> prettyJSON (J.toJSON sections)
  pretty (CantRegister m) = "LSP: can't register dynamically for:" <+> pretty m

newtype LspT config m a = LspT {unLspT :: ReaderT (LanguageContextEnv config) m a}
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

data LanguageContextEnv config = LanguageContextEnv
  { resHandlers :: !(Handlers IO)
  , resConfigSection :: T.Text
  , resParseConfig :: !(config -> J.Value -> Either T.Text config)
  , resOnConfigChange :: !(config -> IO ())
  , resSendMessage :: !(FromServerMessage -> IO ())
  , -- We keep the state in a TVar to be thread safe
    resState :: !(LanguageContextState config)
  , resClientCapabilities :: !L.ClientCapabilities
  , resRootPath :: !(Maybe FilePath)
  , resProgressStartDelay :: Int
  -- ^ The delay before starting a progress reporting session, in microseconds
  , resProgressUpdateDelay :: Int
  -- ^ The delay between sending progress updates, in microseconds
  }

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

{- | A mapping from methods to the static 'Handler's that should be used to
 handle responses when they come in from the client. To build up a 'Handlers',
 you should 'mconcat' a list of 'notificationHandler' and 'requestHandler's:

 @
 mconcat [
   notificationHandler SInitialized $ \notif -> pure ()
 , requestHandler STextDocumentHover $ \req responder -> pure ()
 ]
 @
-}
data Handlers m = Handlers
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

{- | The type of a handler that handles requests and notifications coming in
 from the server or client
-}
type family Handler (f :: Type -> Type) (m :: Method from t) = (result :: Type) | result -> f t m where
  Handler f (m :: Method _from Request) = TRequestMessage m -> (Either (TResponseError m) (MessageResult m) -> f ()) -> f ()
  Handler f (m :: Method _from Notification) = TNotificationMessage m -> f ()

-- | How to convert two isomorphic data structures between each other.
data m <~> n = Iso
  { forward :: forall a. m a -> n a
  , backward :: forall a. n a -> m a
  }

transmuteHandlers :: (m <~> n) -> Handlers m -> Handlers n
transmuteHandlers nat = mapHandlers (\i m k -> forward nat (i m (backward nat . k))) (\i m -> forward nat (i m))

mapHandlers ::
  (forall (a :: Method ClientToServer Request). Handler m a -> Handler n a) ->
  (forall (a :: Method ClientToServer Notification). Handler m a -> Handler n a) ->
  Handlers m ->
  Handlers n
mapHandlers mapReq mapNot (Handlers reqs nots) = Handlers reqs' nots'
 where
  reqs' = SMethodMap.map (\(ClientMessageHandler i) -> ClientMessageHandler $ mapReq i) reqs
  nots' = SMethodMap.map (\(ClientMessageHandler i) -> ClientMessageHandler $ mapNot i) nots

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextState config = LanguageContextState
  { resVFS :: !(TVar VFSData)
  , resDiagnostics :: !(TVar DiagnosticStore)
  , resConfig :: !(TVar config)
  , resWorkspaceFolders :: !(TVar [WorkspaceFolder])
  , resProgressData :: !ProgressData
  , resPendingResponses :: !(TVar ResponseMap)
  , resRegistrationsNot :: !(TVar (RegistrationMap Notification))
  , resRegistrationsReq :: !(TVar (RegistrationMap Request))
  , resLspId :: !(TVar Int32)
  , resShutdown :: !(C.Barrier ())
  -- ^ Has the server received 'shutdown'? Can be used to conveniently trigger e.g. thread termination,
  -- but if you need a cleanup action to terminate before exiting, then you should install a full
  -- 'shutdown' handler
  }

type ResponseMap = IxMap LspId (Product SMethod ServerResponseCallback)

type RegistrationMap (t :: MessageKind) = SMethodMap (Product RegistrationId (ClientMessageHandler IO t))

data RegistrationToken (m :: Method ClientToServer t) = RegistrationToken (SMethod m) (RegistrationId m)
newtype RegistrationId (m :: Method ClientToServer t) = RegistrationId Text
  deriving (Eq)

data ProgressData = ProgressData
  { progressNextId :: !(TVar Int32)
  , progressCancel :: !(TVar (Map.Map ProgressToken (IO ())))
  }

data VFSData = VFSData
  { vfsData :: !VFS
  , reverseMap :: !(Map.Map FilePath FilePath)
  }

{-# INLINE modifyState #-}
modifyState :: MonadLsp config m => (LanguageContextState config -> TVar a) -> (a -> a) -> m ()
modifyState sel f = do
  tvarDat <- getStateVar sel
  liftIO $ atomically $ modifyTVar' tvarDat f

{-# INLINE stateState #-}
stateState :: MonadLsp config m => (LanguageContextState config -> TVar s) -> (s -> (a, s)) -> m a
stateState sel f = do
  tvarDat <- getStateVar sel
  liftIO $ atomically $ stateTVar tvarDat f

{-# INLINE getsState #-}
getsState :: MonadLsp config m => (LanguageContextState config -> TVar a) -> m a
getsState f = do
  tvarDat <- getStateVar f
  liftIO $ readTVarIO tvarDat

{-# INLINE getStateVar #-}
getStateVar :: MonadLsp config m => (LanguageContextState config -> TVar a) -> m (TVar a)
getStateVar f = f . resState <$> getLspEnv

-- ---------------------------------------------------------------------

{- | Options that the server may configure.
 If you set handlers for some requests, you may need to set some of these options.
-}
data Options = Options
  { optTextDocumentSync :: Maybe L.TextDocumentSyncOptions
  , optCompletionTriggerCharacters :: Maybe [Char]
  -- ^  The characters that trigger completion automatically.
  , optCompletionAllCommitCharacters :: Maybe [Char]
  -- ^ The list of all possible characters that commit a completion. This field can be used
  -- if clients don't support individual commit characters per completion item. See
  -- `_commitCharactersSupport`.
  , optSignatureHelpTriggerCharacters :: Maybe [Char]
  -- ^ The characters that trigger signature help automatically.
  , optSignatureHelpRetriggerCharacters :: Maybe [Char]
  -- ^ List of characters that re-trigger signature help.
  -- These trigger characters are only active when signature help is already showing. All trigger characters
  -- are also counted as re-trigger characters.
  , optCodeActionKinds :: Maybe [CodeActionKind]
  -- ^ CodeActionKinds that this server may return.
  -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  -- may list out every specific kind they provide.
  , optDocumentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
  -- ^ The list of characters that triggers on type formatting.
  -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
  -- The first character is mandatory, so a 'NonEmpty' should be passed.
  , optExecuteCommandCommands :: Maybe [Text]
  -- ^ The commands to be executed on the server.
  -- If you set `executeCommandHandler`, you **must** set this.
  , optServerInfo :: Maybe ServerInfo
  -- ^ Information about the server that can be advertised to the client.
  , optSupportClientInitiatedProgress :: Bool
  -- ^ Whether or not to support client-initiated progress.
  , optProgressStartDelay :: Int
  -- ^ The delay before starting a progress reporting session, in microseconds
  , optProgressUpdateDelay :: Int
  -- ^ The delay between sending progress updates, in microseconds
  }

instance Default Options where
  def =
    Options
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      False
      -- See Note [Delayed progress reporting]
      0
      0

defaultOptions :: Options
defaultOptions = def

-- See Note [LSP configuration] for discussion of the configuration-related fields

{- | Contains all the callbacks to use for initialized the language server.
 it is parameterized over a config type variable representing the type for the
 specific configuration data the language server needs to use.
-}
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
  , onConfigChange :: config -> m ()
  -- ^ This callback is called any time the configuration is updated, with
  -- the new config. Servers that want to react to config changes should provide
  -- a callback here, it is not sufficient to just add e.g. a @workspace/didChangeConfiguration@
  -- handler.
  , doInitialize :: LanguageContextEnv config -> TMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) a)
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

{- | A function that a 'Handler' is passed that can be used to respond to a
 request with either an error, or the response params.
-}
newtype ServerResponseCallback (m :: Method ServerToClient Request)
  = ServerResponseCallback (Either (TResponseError m) (MessageResult m) -> IO ())

{- | Return value signals if response handler was inserted successfully
 Might fail if the id was already in the map
-}
addResponseHandler :: MonadLsp config f => LspId m -> (Product SMethod ServerResponseCallback) m -> f Bool
addResponseHandler lid h = do
  stateState resPendingResponses $ \pending ->
    case insertIxMap lid h pending of
      Just !m -> (True, m)
      Nothing -> (False, pending)

sendNotification ::
  forall (m :: Method ServerToClient Notification) f config.
  MonadLsp config f =>
  SServerMethod m ->
  MessageParams m ->
  f ()
sendNotification m params =
  let msg = TNotificationMessage "2.0" m params
   in case splitServerMethod m of
        IsServerNot -> sendToClient $ fromServerNot msg
        IsServerEither -> sendToClient $ FromServerMess m $ NotMess msg

sendRequest ::
  forall (m :: Method ServerToClient Request) f config.
  MonadLsp config f =>
  SServerMethod m ->
  MessageParams m ->
  (Either (TResponseError m) (MessageResult m) -> f ()) ->
  f (LspId m)
sendRequest m params resHandler = do
  reqId <- IdInt <$> freshLspId
  rio <- askRunInIO
  success <- addResponseHandler reqId (Pair m (ServerResponseCallback (rio . resHandler)))
  unless success $ error "LSP: could not send FromServer request as id is reused"

  let msg = TRequestMessage "2.0" reqId m params
  ~() <- case splitServerMethod m of
    IsServerReq -> sendToClient $ fromServerReq msg
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

{- | Dump the current text for a given VFS file to a file
 in the given directory and return the path to the file.
-}
persistVirtualFile :: MonadLsp config m => LogAction m (WithSeverity VfsLog) -> FilePath -> NormalizedUri -> m (Maybe FilePath)
persistVirtualFile logger dir uri = do
  join $ stateState resVFS $ \vfs ->
    case persistFileVFS logger dir (vfsData vfs) uri of
      Nothing -> (return Nothing, vfs)
      Just (fn, write) ->
        let !revMap = case uriToFilePath (fromNormalizedUri uri) of
              Just uri_fp -> Map.insert fn uri_fp $ reverseMap vfs
              -- TODO: Does the VFS make sense for URIs which are not files?
              -- The reverse map should perhaps be (FilePath -> URI)
              Nothing -> reverseMap vfs
            !vfs' = vfs{reverseMap = revMap}
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
        Nothing -> 0
  return (VersionedTextDocumentIdentifier uri ver)
{-# INLINE getVersionedTextDoc #-}

-- TODO: should this function return a URI?

{- | If the contents of a VFS has been dumped to a temporary file, map
 the temporary file name back to the original one.
-}
reverseFileMap :: MonadLsp config m => m (FilePath -> FilePath)
reverseFileMap = do
  vfs <- getsState resVFS
  let f fp = Map.findWithDefault fp fp $ reverseMap vfs
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
    let !next = cur + 1 in (cur, next)
{-# INLINE freshLspId #-}

-- ---------------------------------------------------------------------

{- | The current configuration from the client as set via the @initialize@ and
 @workspace/didChangeConfiguration@ requests, as well as by calls to
 'setConfig'.
-}
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

{- | Sends a @client/registerCapability@ request and dynamically registers
 a 'Method' with a 'Handler'. Returns 'Nothing' if the client does not
 support dynamic registration for the specified method, otherwise a
 'RegistrationToken' which can be used to unregister it later.
-}
registerCapability ::
  forall f t (m :: Method ClientToServer t) config.
  MonadLsp config f =>
  LogAction f (WithSeverity LspCoreLog) ->
  SClientMethod m ->
  RegistrationOptions m ->
  Handler f m ->
  f (Maybe (RegistrationToken m))
registerCapability logger method regOpts f = do
  handlers <- resHandlers <$> getLspEnv
  let alreadyStaticallyRegistered = case splitClientMethod method of
        IsClientNot -> SMethodMap.member method $ notHandlers handlers
        IsClientReq -> SMethodMap.member method $ reqHandlers handlers
        IsClientEither -> error "Cannot register capability for custom methods"
  go alreadyStaticallyRegistered
 where
  -- If the server has already registered statically, don't dynamically register
  -- as per the spec
  go True = pure Nothing
  go False = do
    rio <- askUnliftIO
    mtoken <- trySendRegistration logger method regOpts
    case mtoken of
      Just token@(RegistrationToken _ regId) -> do
        ~() <- case splitClientMethod method of
          IsClientNot -> modifyState resRegistrationsNot $ \oldRegs ->
            let pair = Pair regId (ClientMessageHandler (unliftIO rio . f))
             in SMethodMap.insert method pair oldRegs
          IsClientReq -> modifyState resRegistrationsReq $ \oldRegs ->
            let pair = Pair regId (ClientMessageHandler (\msg k -> unliftIO rio $ f msg (liftIO . k)))
             in SMethodMap.insert method pair oldRegs
          IsClientEither -> error "Cannot register capability for custom methods"

        pure $ Just token
      Nothing -> pure Nothing

trySendRegistration ::
  forall f t (m :: Method ClientToServer t) config.
  MonadLsp config f =>
  LogAction f (WithSeverity LspCoreLog) ->
  SClientMethod m ->
  RegistrationOptions m ->
  f (Maybe (RegistrationToken m))
trySendRegistration logger method regOpts = do
  clientCaps <- resClientCapabilities <$> getLspEnv
  -- First, check to see if the client supports dynamic registration on this method
  if dynamicRegistrationSupported method clientCaps
    then do
      rid <- T.pack . show <$> freshLspId
      let registration = L.TRegistration rid method (Just regOpts)
          params = L.RegistrationParams [toUntypedRegistration registration]
          regId = RegistrationId rid

      -- TODO: handle the scenario where this returns an error
      _ <- sendRequest SMethod_ClientRegisterCapability params $ \_res -> pure ()

      pure (Just $ RegistrationToken method regId)
    else do
      logger <& CantRegister SMethod_WorkspaceDidChangeConfiguration `WithSeverity` Warning
      pure Nothing

{- | Sends a @client/unregisterCapability@ request and removes the handler
 for that associated registration.
-}
unregisterCapability :: MonadLsp config f => RegistrationToken m -> f ()
unregisterCapability (RegistrationToken m (RegistrationId uuid)) = do
  ~() <- case splitClientMethod m of
    IsClientReq -> modifyState resRegistrationsReq $ SMethodMap.delete m
    IsClientNot -> modifyState resRegistrationsNot $ SMethodMap.delete m
    IsClientEither -> error "Cannot unregister capability for custom methods"

  let unregistration = L.TUnregistration uuid m
      params = L.UnregistrationParams [toUntypedUnregistration unregistration]
  void $ sendRequest SMethod_ClientUnregisterCapability params $ \_res -> pure ()

-- ---------------------------------------------------------------------

{- | Aggregate all diagnostics pertaining to a particular version of a document,
 by source, and sends a @textDocument/publishDiagnostics@ notification with
 the total (limited by the first parameter) whenever it is updated.
-}
publishDiagnostics :: MonadLsp config m => Int -> NormalizedUri -> Maybe L.Int32 -> DiagnosticsBySource -> m ()
publishDiagnostics maxDiagnosticCount uri version diags = join $ stateState resDiagnostics $ \oldDiags ->
  let !newDiags = updateDiagnostics oldDiags uri version diags
      mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
      act = case mdp of
        Nothing -> return ()
        Just params ->
          sendToClient $ L.fromServerNot $ L.TNotificationMessage "2.0" L.SMethod_TextDocumentPublishDiagnostics params
   in (act, newDiags)

-- ---------------------------------------------------------------------

{- | Remove all diagnostics from a particular source, and send the updates to
 the client.
-}
flushDiagnosticsBySource ::
  MonadLsp config m =>
  -- | Max number of diagnostics to send
  Int ->
  Maybe Text ->
  m ()
flushDiagnosticsBySource maxDiagnosticCount msource = join $ stateState resDiagnostics $ \oldDiags ->
  let !newDiags = flushBySource oldDiags msource
      -- Send the updated diagnostics to the client
      act = forM_ (HM.keys newDiags) $ \uri -> do
        let mdp = getDiagnosticParamsFor maxDiagnosticCount newDiags uri
        case mdp of
          Nothing -> return ()
          Just params -> do
            sendToClient $ L.fromServerNot $ L.TNotificationMessage "2.0" L.SMethod_TextDocumentPublishDiagnostics params
   in (act, newDiags)

-- ---------------------------------------------------------------------

{- | The changes in a workspace edit should be applied from the end of the file
 toward the start. Sort them into this order.
-}
reverseSortEdit :: L.WorkspaceEdit -> L.WorkspaceEdit
reverseSortEdit (L.WorkspaceEdit cs dcs anns) = L.WorkspaceEdit cs' dcs' anns
 where
  cs' :: Maybe (Map.Map Uri [TextEdit])
  cs' = (fmap . fmap) sortTextEdits cs

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
    Left err -> (Left err, oldConfig)
    Right newConfig -> (Right newConfig, newConfig)
  case res of
    Left err -> do
      logger <& ConfigurationParseError newConfigObject err `WithSeverity` Warning
    Right newConfig -> do
      logger <& NewConfig newConfigObject `WithSeverity` Debug
      cb <- LspT $ asks resOnConfigChange
      liftIO $ cb newConfig

{- | Send a `worksapce/configuration` request to update the server's config.

 This is called automatically in response to `workspace/didChangeConfiguration` notifications
 from the client, so should not normally be called manually.
-}
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
    else logger <& ConfigurationNotSupported `WithSeverity` Debug

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

-- | Checks if the server has received a 'shutdown' request.
isShuttingDown :: (m ~ LspM config) => m Bool
isShuttingDown = do
  b <- resShutdown . resState <$> getLspEnv
  r <- liftIO $ C.waitBarrierMaybe b
  pure $ case r of
    Just _ -> True
    Nothing -> False

-- | Blocks until the server receives a 'shutdown' request.
waitShuttingDown :: (m ~ LspM config) => m ()
waitShuttingDown = do
  b <- resShutdown . resState <$> getLspEnv
  liftIO $ C.waitBarrier b

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

{- Note [Client- versus server-initiated progress]
The protocol supports both client- and server-initiated progress. Client-initiated progress
is simpler: the client gives you a progress token, and then you use that to report progress.
Server-initiated progress is more complex: you need to send a request to the client to tell
them about the token you want to use, and only after that can you send updates using it.
-}

{- Note [Delayed progress reporting]
Progress updates can be very noisy by default. There are two ways this can happen:
- Creating progress notifications for very short-lived operations that don't deserve them.
  This directs the user's attention to something that then immediately ceases to exist,
  which is annoying, the more so if it happens frequently.
- Very frequently updating progress information.

Now, in theory the client could deal with this for us. Probably they _should_: working
out how to display an (accurate) series of progress notifications from the server seems
like the client's job. Nonetheless, this does not always happen, and so it is helpful
to moderate the spam.

For this reason we have configurable delays on starting progress tracking and on sending
updates. However, the defaults are set to 0, so it's opt-in.
-}

{- Note [Request cancellation]
Request cancellation is a bit strange.

We need to in fact assume that all requests are cancellable, see
https://github.com/microsoft/language-server-protocol/issues/1159.

The 'cancellable' property that we can set when making progress reports just
affects whether the client should show a 'Cancel' button to the user in the UI.
The client can still always choose to cancel for another reason.
-}

{- Note [Shutdown]
The 'shutdown' request basically tells the server to clean up and stop doing things.
In particular, it allows us to ignore or reject all further messages apart from 'exit'.

We also provide a `Barrier` that indicates whether or not we are shutdown, this can
be convenient, e.g. you can race a thread against `waitBarrier` to have it automatically
be cancelled when we receive `shutdown`.

Shutdown is a request, and the client won't send `exit` until a server responds, so if you
want to be sure that some cleanup happens, you need to ensure we don't respond to `shutdown`
until it's done. The best way to do this is just to install a specific `shutdown` handler.

After the `shutdown` request, we don't handle any more requests and notifications other than
`exit`. We also don't handle any more responses to requests we have sent but just throw the
responses away.
-}
