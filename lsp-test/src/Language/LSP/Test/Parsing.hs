{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

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
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit.Parser hiding (named)
import Data.Conduit.Parser qualified (named)
import Data.GADT.Compare
import Data.Text qualified as T
import Data.Typeable
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.LSP.Protocol.Message
import Language.LSP.Test.Session

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
satisfy :: (FromServerMessage -> Bool) -> Session FromServerMessage
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

{- | Consumes and returns the result of the specified predicate if it returns `Just`.

 @since 0.6.1.0
-}
satisfyMaybe :: (FromServerMessage -> Maybe a) -> Session a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (FromServerMessage -> Session (Maybe a)) -> Session a
satisfyMaybeM pred = do
  skipTimeout <- overridingTimeout <$> get
  timeoutId <- getCurTimeoutId
  mtid <-
    if skipTimeout
      then pure Nothing
      else
        Just <$> do
          chan <- asks messageChan
          timeout <- asks (messageTimeout . config)
          liftIO $ forkIO $ do
            threadDelay (timeout * 1000000)
            writeChan chan (TimeoutMessage timeoutId)

  x <- Session await

  forM_ mtid $ \tid -> do
    bumpTimeoutId timeoutId
    liftIO $ killThread tid

  modify $ \s -> s{lastReceivedMessage = Just x}

  res <- pred x

  case res of
    Just a -> do
      logMsg LogServer x
      return a
    Nothing -> empty

named :: T.Text -> Session a -> Session a
named s (Session x) = Session (Data.Conduit.Parser.named s x)

{- | Matches a request or a notification coming from the server.
 Doesn't match Custom Messages
-}
message :: SServerMethod m -> Session (TMessage m)
message (SMethod_CustomMethod _) = error "message can't be used with CustomMethod, use customRequest or customNotification instead"
message m1 = named (T.pack $ "Request for: " <> show m1) $ satisfyMaybe $ \case
  FromServerMess m2 msg -> do
    res <- mEqServer m1 m2
    case res of
      Right HRefl -> pure msg
      Left _f -> Nothing
  _ -> Nothing

customRequest :: KnownSymbol s => Proxy s -> Session (TMessage (Method_CustomMethod s :: Method ServerToClient Request))
customRequest p =
  let m = T.pack $ symbolVal p
   in named m $ satisfyMaybe $ \case
        FromServerMess m1 msg -> case splitServerMethod m1 of
          IsServerEither -> case msg of
            ReqMess _ -> case m1 `geq` SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

customNotification :: KnownSymbol s => Proxy s -> Session (TMessage (Method_CustomMethod s :: Method ServerToClient Notification))
customNotification p =
  let m = T.pack $ symbolVal p
   in named m $ satisfyMaybe $ \case
        FromServerMess m1 msg -> case splitServerMethod m1 of
          IsServerEither -> case msg of
            NotMess _ -> case m1 `geq` SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

-- | Matches if the message is a notification.
anyNotification :: Session FromServerMessage
anyNotification = named "Any notification" $ satisfy $ \case
  FromServerMess m msg -> case splitServerMethod m of
    IsServerNot -> True
    IsServerEither -> case msg of
      NotMess _ -> True
      _ -> False
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a request.
anyRequest :: Session FromServerMessage
anyRequest = named "Any request" $ satisfy $ \case
  FromServerMess m _ -> case splitServerMethod m of
    IsServerReq -> True
    _ -> False
  FromServerRsp _ _ -> False

-- | Matches if the message is a response.
anyResponse :: Session FromServerMessage
anyResponse = named "Any response" $ satisfy $ \case
  FromServerMess _ _ -> False
  FromServerRsp _ _ -> True

-- | Matches a response coming from the server.
response :: SMethod (m :: Method ClientToServer Request) -> Session (TResponseMessage m)
response m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  FromServerRsp m2 msg -> do
    HRefl <- runEq mEqClient m1 m2
    pure msg
  _ -> Nothing

-- | Like 'response', but matches a response for a specific id.
responseForId :: SMethod (m :: Method ClientToServer Request) -> LspId m -> Session (TResponseMessage m)
responseForId m lid = named (T.pack $ "Response for id: " ++ show lid) $ do
  satisfyMaybe $ \msg -> do
    case msg of
      FromServerMess _ _ -> Nothing
      FromServerRsp m' rspMsg@(TResponseMessage _ lid' _) -> do
        HRefl <- runEq mEqClient m m'
        guard (Just lid == lid')
        pure rspMsg

-- | Matches any type of message.
anyMessage :: Session FromServerMessage
anyMessage = satisfy (const True)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: Session FromServerMessage
loggingNotification = named "Logging notification" $ satisfy shouldSkip
 where
  shouldSkip (FromServerMess SMethod_WindowLogMessage _) = True
  shouldSkip (FromServerMess SMethod_WindowShowMessage _) = True
  shouldSkip (FromServerMess SMethod_WindowShowMessageRequest _) = True
  shouldSkip (FromServerMess SMethod_WindowShowDocument _) = True
  shouldSkip _ = False

-- | Matches if the message is a configuration request from the server.
configurationRequest :: Session FromServerMessage
configurationRequest = named "Configuration request" $ satisfy shouldSkip
 where
  shouldSkip (FromServerMess SMethod_WorkspaceConfiguration _) = True
  shouldSkip _ = False

loggingOrConfiguration :: Session FromServerMessage
loggingOrConfiguration = loggingNotification <|> configurationRequest

{- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
 (textDocument/publishDiagnostics) notification.
-}
publishDiagnosticsNotification :: Session (TMessage Method_TextDocumentPublishDiagnostics)
publishDiagnosticsNotification = named "Publish diagnostics notification" $
  satisfyMaybe $ \msg -> case msg of
    FromServerMess SMethod_TextDocumentPublishDiagnostics diags -> Just diags
    _ -> Nothing
