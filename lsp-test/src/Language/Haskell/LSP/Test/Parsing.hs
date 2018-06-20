{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.LSP.Test.Parsing where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit.Parser
import Data.Maybe
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (error)
import Language.Haskell.LSP.Test.Exceptions
import Language.Haskell.LSP.Test.Messages
import Language.Haskell.LSP.Test.Session

satisfy :: (MonadIO m, MonadSessionConfig m) => (a -> Bool) -> ConduitParser a m a
satisfy pred = do
  timeout <- timeout <$> lift sessionConfig
  tId <- liftIO myThreadId
  liftIO $ forkIO $ do
    threadDelay (timeout * 1000000)
    throwTo tId TimeoutException
  x <- await
  if pred x
    then return x
    else empty

-- | Matches if the message is a notification.
anyNotification :: (MonadIO m, MonadSessionConfig m) => ConduitParser FromServerMessage m FromServerMessage
anyNotification = satisfy isServerNotification

notification :: forall m a. (MonadIO m, MonadSessionConfig m, FromJSON a) => ConduitParser FromServerMessage m (NotificationMessage ServerMethod a)
notification = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (NotificationMessage ServerMethod a)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | Matches if the message is a request.
anyRequest :: (MonadIO m, MonadSessionConfig m) => ConduitParser FromServerMessage m FromServerMessage
anyRequest = satisfy isServerRequest

request :: forall m a b. (MonadIO m, MonadSessionConfig m, FromJSON a, FromJSON b) => ConduitParser FromServerMessage m (RequestMessage ServerMethod a b)
request = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (RequestMessage ServerMethod a b)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | Matches if the message is a response.
anyResponse :: (MonadIO m, MonadSessionConfig m) => ConduitParser FromServerMessage m FromServerMessage
anyResponse = satisfy isServerResponse

response :: forall m a. (MonadIO m, MonadSessionConfig m, FromJSON a) => ConduitParser FromServerMessage m (ResponseMessage a)
response = do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (ResponseMessage a)
  x <- satisfy (isJust . parser)
  return $ decodeMsg $ encodeMsg x

-- | A version of encode that encodes FromServerMessages as if they
-- weren't wrapped.
encodeMsg :: FromServerMessage -> B.ByteString
encodeMsg = encode . genericToJSON (defaultOptions { sumEncoding = UntaggedValue })

decodeMsg :: FromJSON a => B.ByteString -> a
decodeMsg x = fromMaybe (error $ "Unexpected message type\nGot:\n " ++ show x)
                  (decode x)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: (MonadIO m, MonadSessionConfig m) => ConduitParser FromServerMessage m FromServerMessage
loggingNotification = satisfy shouldSkip
  where
    shouldSkip (NotLogMessage _) = True
    shouldSkip (NotShowMessage _) = True
    shouldSkip (ReqShowMessage _) = True
    shouldSkip _ = False

publishDiagnosticsNotification :: (MonadIO m, MonadSessionConfig m) => ConduitParser FromServerMessage m PublishDiagnosticsNotification
publishDiagnosticsNotification = do
  NotPublishDiagnostics diags <- satisfy test
  return diags
  where test (NotPublishDiagnostics _) = True
        test _ = False