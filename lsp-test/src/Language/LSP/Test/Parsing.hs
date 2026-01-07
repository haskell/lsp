{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Test.Parsing (
  -- $receiving
  satisfy,
  satisfyMaybe,
  message,
  response,
  responseForId,
  customRequest,
  customNotification,
  anyRequest,
  anyResponse,
  anyNotification,
  anyMessage,
  loggingNotification,
  configurationRequest,
  loggingOrConfiguration,
  publishDiagnosticsNotification,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Conduit.Parser hiding (named)
import Data.Conduit.Parser qualified (named)
import Data.GADT.Compare
import Data.Text qualified as T
import Data.Typeable
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.LSP.Protocol.Message
import Language.LSP.Test.Exceptions
import Language.LSP.Test.Session
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Timeout

{- $receiving
 To receive a message, specify the method of the message to expect:

 @
 msg1 <- message SWorkspaceApplyEdit
 msg2 <- message STextDocumentHover
 @

 'Language.LSP.Test.Session' is actually just a parser
 that operates on messages under the hood. This means that you
 can create and combine parsers to match specific sequences of
 messages that you expect.

 For example, if you wanted to match either a definition or
 references request:

 > defOrImpl = message STextDocumentDefinition
 >          <|> message STextDocumentReferences

 If you wanted to match any number of telemetry
 notifications immediately followed by a response:

 @
 logThenDiags =
  skipManyTill (message STelemetryEvent)
               anyResponse
 @
-}

{- | Consumes and returns the next message, if it satisfies the specified predicate.

 @since 0.5.2.0
-}
satisfy :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Bool) -> Session m FromServerMessage
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

{- | Consumes and returns the result of the specified predicate if it returns `Just`.

 @since 0.6.1.0
-}
satisfyMaybe :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Maybe a) -> Session m a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (MonadLoggerIO m, MonadUnliftIO m) => (FromServerMessage -> Session m (Maybe a)) -> Session m a
satisfyMaybeM pred = do
  skipTimeout <- overridingTimeout <$> (asks sessionState >>= readMVar)
  timeoutSeconds <- asks (messageTimeout . config)
  chan <- asks messageChan

  maybeMsg <- (if skipTimeout then (Just <$>) else timeout (timeoutSeconds * 10^(6 :: Int)))
              (readChan chan)

  case maybeMsg of
    Nothing -> do
      lastMsg <- lastReceivedMessage <$> (asks sessionState >>= readMVar)
      throwIO (Timeout lastMsg)
    Just msg -> do
      modifyStatePure_ (\s -> s { lastReceivedMessage = Just msg })

      pred msg >>= \case
        Just a -> return a
        Nothing -> satisfyMaybeM pred

{- | Matches a request or a notification coming from the server.
 Doesn't match Custom Messages
-}
message :: (MonadLoggerIO n, MonadUnliftIO n) => SServerMethod m -> Session n (TMessage m)
message (SMethod_CustomMethod _) = error "message can't be used with CustomMethod, use customRequest or customNotification instead"
message m1 = satisfyMaybe $ \case
  FromServerMess m2 msg -> do
    res <- mEqServer m1 m2
    case res of
      Right HRefl -> pure msg
      Left _f -> Nothing
  _ -> Nothing

customRequest :: (MonadLoggerIO m, MonadUnliftIO m, KnownSymbol s) => Proxy s -> Session m (TMessage (Method_CustomMethod s :: Method ServerToClient Request))
customRequest p =
  let m = T.pack $ symbolVal p
   in satisfyMaybe $ \case
        FromServerMess m1 msg -> case splitServerMethod m1 of
          IsServerEither -> case msg of
            ReqMess _ -> case m1 `geq` SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

customNotification :: (MonadLoggerIO m, MonadUnliftIO m, KnownSymbol s) => Proxy s -> Session m (TMessage (Method_CustomMethod s :: Method ServerToClient Notification))
customNotification p =
  let m = T.pack $ symbolVal p
   in satisfyMaybe $ \case
        FromServerMess m1 msg -> case splitServerMethod m1 of
          IsServerEither -> case msg of
            NotMess _ -> case m1 `geq` SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

-- | Matches if the message is a notification.
anyNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyNotification = satisfy $ \case
  FromServerMess m msg -> case splitServerMethod m of
    IsServerNot -> True
    IsServerEither -> case msg of
      NotMess _ -> True
      _ -> False
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a request.
anyRequest :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyRequest = satisfy $ \case
  FromServerMess m _ -> case splitServerMethod m of
    IsServerReq -> True
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a response.
anyResponse :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyResponse = satisfy $ \case
  FromServerMess _ _ -> False
  FromServerRsp _ _ -> True

-- | Matches a response coming from the server.
response :: (MonadLoggerIO n, MonadUnliftIO n) => SMethod (m :: Method ClientToServer Request) -> Session n (TResponseMessage m)
response m1 = satisfyMaybe $ \case
  FromServerRsp m2 msg -> do
    HRefl <- runEq mEqClient m1 m2
    pure msg
  _ -> Nothing

-- | Like 'response', but matches a response for a specific id.
responseForId :: (MonadLoggerIO n, MonadUnliftIO n) => SMethod (m :: Method ClientToServer Request) -> LspId m -> Session n (TResponseMessage m)
responseForId m lid = do
  satisfyMaybe $ \msg -> do
    case msg of
      FromServerMess _ _ -> Nothing
      FromServerRsp m' rspMsg@(TResponseMessage _ lid' _) -> do
        HRefl <- runEq mEqClient m m'
        guard (Just lid == lid')
        pure rspMsg

-- | Matches any type of message.
anyMessage :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
anyMessage = satisfy (const True)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
loggingNotification = satisfy shouldSkip
 where
  shouldSkip (FromServerMess SMethod_WindowLogMessage _) = True
  shouldSkip (FromServerMess SMethod_WindowShowMessage _) = True
  shouldSkip (FromServerMess SMethod_WindowShowMessageRequest _) = True
  shouldSkip (FromServerMess SMethod_WindowShowDocument _) = True
  shouldSkip _ = False

-- | Matches if the message is a configuration request from the server.
configurationRequest :: (MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
configurationRequest = satisfy shouldSkip
 where
  shouldSkip (FromServerMess SMethod_WorkspaceConfiguration _) = True
  shouldSkip _ = False

loggingOrConfiguration :: (Alternative m, MonadLoggerIO m, MonadUnliftIO m) => Session m FromServerMessage
loggingOrConfiguration = loggingNotification <|> configurationRequest

{- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
 (textDocument/publishDiagnostics) notification.
-}
publishDiagnosticsNotification :: (MonadLoggerIO m, MonadUnliftIO m) => Session m (TMessage Method_TextDocumentPublishDiagnostics)
publishDiagnosticsNotification =   satisfyMaybe $ \msg -> case msg of
    FromServerMess SMethod_TextDocumentPublishDiagnostics diags -> Just diags
    _ -> Nothing
