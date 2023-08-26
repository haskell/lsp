module Language.LSP.Server.Core where

import JSONRPC.RPC qualified as Untyped
import JSONRPC.Server qualified as Untyped
import JSONRPC.Method qualified as Untyped
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC qualified as RPC
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Colog.Core
import Control.Concurrent.Extra
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson qualified as J hiding (Error)
import Data.Aeson.Lens ()
import Data.Coerce
import Data.Default
import Data.List.NonEmpty
import Data.Map qualified as Map
import Data.Monoid (Ap (..))
import Data.Singletons (withSingI)
import Data.Text qualified as T
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Protocol.Utils.Misc
import Language.LSP.VFS
import Prettyprinter
import qualified Data.Set as Set

data LspRegistrationLog
  = forall m. RegistrationUnsupported (LSP.SMethod m)
  | forall m. HandlersAlreadyExist (LSP.SMethod m) (Set.Set Untyped.Method)
  | RegistrationFailed (ResponseError LSP.Method_ClientRegisterCapability)
  | UnregistrationFailed (ResponseError LSP.Method_ClientUnregisterCapability)
  | NoRegistration RegistrationId

deriving stock instance (Show LspRegistrationLog)

instance Pretty LspRegistrationLog where
  pretty (RegistrationUnsupported m) = "The client does not support dynamic registration for:" <+> pretty m
  pretty (HandlersAlreadyExist m ms) = "Handlers for these methods already exists, dynamically registering them may result in incoherent state:"
    <+> pretty m <+> "," <+> pretty (Set.toList ms)
  pretty (RegistrationFailed err) = "Dynamic registration failed:" <+> (withSingI LSP.SMethod_ClientRegisterCapability $ pretty err)
  pretty (UnregistrationFailed err) = "Unregistration failed:" <+> (withSingI LSP.SMethod_ClientUnregisterCapability $ pretty err)
  pretty (NoRegistration rid) = "No dynamic registration exists for id, cannot unregister:" <+> pretty rid

newtype RegistrationId = RegistrationId T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Pretty)

data RegistrationHandle = RegistrationHandle
  { registrationMethods :: !(TVar (Map.Map RegistrationId LSP.Method))
  , nextRegistrationId :: TVar L.Int32
  , serverHandle :: ServerHandle Server LSP.Method
  , clientCapabilities :: LSP.ClientCapabilities
  , logger :: LogAction IO (WithSeverity LspRegistrationLog)
  }

data LspProgressLog
  = ProgressCancel L.ProgressToken
  deriving stock (Show)

instance Pretty LspProgressLog where
  pretty (ProgressCancel tid) = "Cancelling action for token:" <+> pretty tid

data ProgressHandle = ProgressHandle
  { progressNextId :: !(TVar L.Int32)
  , progressCancel :: !(TVar (Map.Map L.ProgressToken (IO ())))
  , progressLogger :: LogAction IO (WithSeverity LspProgressLog)
  , progressStartDelay :: Int
  -- ^ The delay before starting a progress reporting session, in microseconds
  , progressUpdateDelay :: Int
  -- ^ The delay between sending progress updates, in microseconds
  , serverHandle :: ServerHandle Server LSP.Method
  , clientCapabilities :: LSP.ClientCapabilities
  }

data LspConfigLog
  = -- TODO: arguably it would be nicer to have the config object itself in there, but
    -- then we're going to need 'Pretty config' constraints everywhere
    NewConfig J.Value
  | ConfigurationParseError J.Value T.Text
  | ConfigurationNotSupported
  | BadConfigurationResponse (ResponseError LSP.Method_WorkspaceConfiguration)
  | WrongConfigSections [J.Value]
  deriving stock (Show)

instance Pretty LspConfigLog where
  pretty (NewConfig config) = "Set new config:" <+> prettyJSON config
  pretty ConfigurationNotSupported = "Not requesting configuration since the client does not support workspace/configuration"
  pretty (ConfigurationParseError settings err) =
    vsep
      [ "LSP: configuration parse error:"
      , pretty err
      , "when parsing"
      , prettyJSON settings
      ]
  pretty (BadConfigurationResponse err) = "Error when requesting configuration: " <+> (withSingI LSP.SMethod_WorkspaceConfiguration $ pretty err)
  pretty (WrongConfigSections sections) = "Expected only one configuration section, got: " <+> prettyJSON (J.toJSON sections)

data ConfigHandle config = ConfigHandle
  { configSection :: !T.Text
  , parseConfig :: !(config -> J.Value -> Either T.Text config)
  , onConfigChange :: !(config -> IO ())
  , config :: !(TVar config)
  , serverHandle :: ServerHandle Server LSP.Method
  , registrationHandle :: RegistrationHandle
  , clientCapabilities :: LSP.ClientCapabilities
  , logger :: LogAction IO (WithSeverity LspConfigLog)
  }

data LspShutdownLog
  = forall m. MessageDuringShutdown (LSP.SMethod m)
  | ShuttingDown
  | Exiting

deriving stock instance (Show LspShutdownLog)

instance Pretty LspShutdownLog where
  pretty (MessageDuringShutdown m) = "Received message during shutdown:" <+> pretty m
  pretty ShuttingDown = "Received shutdown"
  pretty Exiting = "Received exit"

data ShutdownHandle = ShutdownHandle
  { shutdownBarrier :: Barrier ()
  -- ^ Has the server received 'shutdown'? Can be used to conveniently trigger e.g. thread termination,
  -- but if you need a cleanup action to terminate before exiting, then you should install a full
  -- 'shutdown' handler
  , shutdownLogger :: LogAction IO (WithSeverity LspShutdownLog)
  }

data WorkspaceFoldersHandle = WorkspaceFoldersHandle
  { workspaceFolders :: !(TVar (Maybe [L.WorkspaceFolder]))
  }

data LanguageServerHandle config = LanguageServerHandle
  { serverHandle :: !(ServerHandle Server LSP.Method)
  , configHandle :: !(ConfigHandle config)
  , clientCapabilities :: !L.ClientCapabilities
  , rootPath :: !(Maybe FilePath)
  , vfsHandle :: !VfsHandle
  , diagnosticsHandle :: !DiagnosticsHandle
  , workspaceFoldersHandle :: !WorkspaceFoldersHandle
  , progressHandle :: !ProgressHandle
  , registrationHandle :: !RegistrationHandle
  , shutdownHandle :: !ShutdownHandle
  }

--------------------
-- LspM
--------------------

newtype LspT config m a = LspT {unLspT :: ReaderT (LanguageServerHandle config) m a}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans, MonadUnliftIO, MonadFix)
  deriving (Semigroup, Monoid) via (Ap (LspT config m) a)

-- for deriving the instance of MonadUnliftIO
type role LspT representational representational nominal

instance MonadIO m => Untyped.MonadServer (LspT config m) where
  getServerHandle = LspT $ do
    h <- ask
    pure $ coerce h.serverHandle

instance MonadIO m => MonadServer Server LSP.Method (LspT config m)

instance MonadIO m => Untyped.MonadRpc (LspT config m) where
  getRpcHandle = Untyped.rpcHandle . untypedServerHandle <$> getServerHandle

instance MonadIO m => RPC.MonadRpc Server LSP.Method (LspT config m)

runLspT :: LanguageServerHandle config -> LspT config m a -> m a
runLspT env = flip runReaderT env . unLspT
{-# INLINE runLspT #-}

type LspM config = LspT config IO

class (MonadServer Server LSP.Method m, RPC.MonadRpc Server LSP.Method m, MonadUnliftIO m) => MonadLsp config m | m -> config where
  getLspHandle :: m (LanguageServerHandle config)

instance (MonadServer Server LSP.Method m, MonadUnliftIO m) => MonadLsp config (LspT config m) where
  getLspHandle = LspT ask

--------------------
-- Server Definition
--------------------

type LspServerHandlers = Handlers Server LSP.Method


{- | Contains all the callbacks to use for initialized the language server.
 it is parameterized over a config type variable representing the type for the
 specific configuration data the language server needs to use.
-}
data ServerDefinition config = ServerDefinition
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
  , onConfigChange :: config -> IO ()
  -- ^ This callback is called any time the configuration is updated, with
  -- the new config. Servers that want to react to config changes should provide
  -- a callback here, it is not sufficient to just add e.g. a @workspace/didChangeConfiguration@
  -- handler.
  , doInitialize ::
      LanguageServerHandle config ->
      MethodParams LSP.Method_Initialize ->
      IO (Either (ResponseError LSP.Method_Initialize) LspServerHandlers)
  -- ^ Called *after* receiving the @initialize@ request and *before*
  -- returning the response. This callback will be invoked to offer the
  -- language server implementation the chance to create any processes or
  -- start new threads that may be necessary for the server lifecycle. It can
  -- also return an error in the initialization if necessary.
  , options :: Options
  -- ^ Configurable options for the server's capabilities.
  }

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
  , optCodeActionKinds :: Maybe [LSP.CodeActionKind]
  -- ^ CodeActionKinds that this server may return.
  -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  -- may list out every specific kind they provide.
  , optDocumentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
  -- ^ The list of characters that triggers on type formatting.
  -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
  -- The first character is mandatory, so a 'NonEmpty' should be passed.
  , optExecuteCommandCommands :: Maybe [T.Text]
  -- ^ The commands to be executed on the server.
  -- If you set `executeCommandHandler`, you **must** set this.
  , optServerInfo :: Maybe LSP.ServerInfo
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

{- Note [Request cancellation]
Request cancellation is a bit strange.

We need to in fact assume that all requests are cancellable, see
https://github.com/microsoft/language-server-protocol/issues/1159.

The 'cancellable' property that we can set when making progress reports just
affects whether the client should show a 'Cancel' button to the user in the UI.
The client can still always choose to cancel for another reason.
-}
