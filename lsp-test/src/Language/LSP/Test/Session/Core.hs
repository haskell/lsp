-- |

module Language.LSP.Test.Session.Core where

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
import Language.LSP.Test.Types
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


sendMessage :: (MonadIO m, MonadReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- serverIn <$> ask
  logMsg LogClient msg
  liftIO $ B.hPut h (addHeader $ encode msg) `catch` (liftIO . throwIO . MessageSendError (toJSON msg))

-- | Logs the message if the config specified it
logMsg :: (ToJSON a, MonadIO m, MonadReader SessionContext m)
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

data LogMsgType = LogServer | LogClient
  deriving Eq
