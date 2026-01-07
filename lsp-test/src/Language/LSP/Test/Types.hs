{-# LANGUAGE DataKinds #-}

module Language.LSP.Test.Types where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..))
import Control.Applicative
import Control.Lens hiding (List, Empty)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask)
import Control.Monad.Trans.State (StateT, runStateT, execState)
import qualified Control.Monad.Trans.State as State
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens ()
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test.Compat
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Process (gracefullyWaitForProcess)
import Language.LSP.VFS
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process (ProcessHandle())
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.Timeout (timeout)

-- | A session representing one instance of launching and connecting to a server.
--
-- You can send and receive messages to the server within 'Session' via
-- 'Language.LSP.Test.message',
-- 'Language.LSP.Test.sendRequest' and
-- 'Language.LSP.Test.sendNotification'.

newtype Session m a = Session { unwrapSession :: ReaderT SessionContext m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadThrow, MonadReader SessionContext, MonadUnliftIO, MonadMask, MonadCatch, MonadTrans)

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
