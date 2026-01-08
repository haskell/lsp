{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Language.LSP.Test.Types where

import Control.Applicative
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Lens ()
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.VFS
import System.IO
import UnliftIO.Concurrent
import UnliftIO.Exception

-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session m a = Session { unwrapSession :: ReaderT SessionContext m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadThrow, MonadReader SessionContext, MonadUnliftIO, MonadMask, MonadCatch, MonadTrans)

#if !MIN_VERSION_monad_logger(0,3,40)
instance (Alternative m) => Alternative (LoggingT m) where
  empty = LoggingT (\_ -> empty)
  LoggingT x <|> LoggingT y = LoggingT (\f -> x f <|> y f)
#endif

#if __GLASGOW_HASKELL__ >= 806
instance MonadIO m => MonadFail (Session m) where
  fail s = do
    lastMsg <- fromJust . lastReceivedMessage <$> (asks sessionState >>= readMVar)
    liftIO $ throwIO (UnexpectedMessage s lastMsg)
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

data SessionContext = SessionContext
  {
    serverIn :: Handle
  , rootDir :: FilePath
  , messageChan :: Chan FromServerMessage -- ^ Where all messages come through
  , requestMap :: MVar RequestMap
  , initRsp :: MVar (TResponseMessage Method_Initialize)
  , isShuttingDown :: MVar Bool
  , config :: SessionConfig
  , sessionCapabilities :: ClientCapabilities
  , sessionState :: MVar SessionState
  }

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
