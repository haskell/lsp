
module Language.LSP.Test.Session.Core where

import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Types
import UnliftIO.Exception (throwIO)


sendMessage :: (MonadLoggerIO m, MonadReader SessionContext m, ToJSON a) => a -> m ()
sendMessage msg = do
  h <- serverIn <$> ask
  logMsg LogClient msg
  liftIO $ B.hPut h (addHeader $ encode msg) `catch` (liftIO . throwIO . MessageSendError (toJSON msg))

-- | Logs the message if the config specified it
logMsg :: (ToJSON a, MonadLoggerIO m, MonadReader SessionContext m)
       => LogMsgType -> a -> m ()
logMsg t msg = do
  shouldLog <- asks $ logMessages . config
  when shouldLog $
    logDebugN $ (T.pack arrow <> T.pack (showPretty msg))

  where
    arrow
      | t == LogServer  = "<-- "
      | otherwise       = "--> "

    showPretty = B.unpack . encodePretty

data LogMsgType = LogServer | LogClient
  deriving Eq
