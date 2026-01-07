-- |

module Language.LSP.Test.Session.Core where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch)
import Data.Aeson hiding (Error, Null)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import Language.LSP.Test.Decoding
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Types
import System.Console.ANSI
import UnliftIO.Exception (throwIO)


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
