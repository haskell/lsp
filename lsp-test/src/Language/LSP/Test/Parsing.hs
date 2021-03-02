{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Test.Parsing
  ( -- $receiving
    satisfy
  , satisfyMaybe
  , message
  , response
  , responseForId
  , customRequest
  , customNotification
  , anyRequest
  , anyResponse
  , anyNotification
  , anyMessage
  , loggingNotification
  , publishDiagnosticsNotification
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.Conduit.Parser hiding (named)
import qualified Data.Conduit.Parser (named)
import qualified Data.Text as T
import Data.Typeable
import Language.LSP.Types
import Language.LSP.Test.Session

-- $receiving
-- To receive a message, specify the method of the message to expect:
--
-- @
-- msg1 <- message SWorkspaceApplyEdit
-- msg2 <- message STextDocumentHover
-- @
--
-- 'Language.LSP.Test.Session' is actually just a parser
-- that operates on messages under the hood. This means that you
-- can create and combine parsers to match speicifc sequences of
-- messages that you expect.
--
-- For example, if you wanted to match either a definition or
-- references request:
--
-- > defOrImpl = message STextDocumentDefinition
-- >          <|> message STextDocumentReferences
--
-- If you wanted to match any number of telemetry
-- notifications immediately followed by a response:
--
-- @
-- logThenDiags =
--  skipManyTill (message STelemetryEvent)
--               anyResponse
-- @

-- | Consumes and returns the next message, if it satisfies the specified predicate.
--
-- @since 0.5.2.0
satisfy :: (FromServerMessage -> Bool) -> Session FromServerMessage
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

-- | Consumes and returns the result of the specified predicate if it returns `Just`.
--
-- @since 0.6.1.0
satisfyMaybe :: (FromServerMessage -> Maybe a) -> Session a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (FromServerMessage -> Session (Maybe a)) -> Session a
satisfyMaybeM pred = do 
  
  skipTimeout <- overridingTimeout <$> get
  timeoutId <- getCurTimeoutId
  mtid <-
    if skipTimeout
    then pure Nothing
    else Just <$> do
      chan <- asks messageChan
      timeout <- asks (messageTimeout . config)
      liftIO $ forkIO $ do
        threadDelay (timeout * 1000000)
        writeChan chan (TimeoutMessage timeoutId)

  x <- Session await

  forM_ mtid $ \tid -> do
    bumpTimeoutId timeoutId
    liftIO $ killThread tid

  modify $ \s -> s { lastReceivedMessage = Just x }

  res <- pred x

  case res of
    Just a -> do
      logMsg LogServer x
      return a
    Nothing -> empty

named :: T.Text -> Session a -> Session a
named s (Session x) = Session (Data.Conduit.Parser.named s x)


-- | Matches a request or a notification coming from the server.
-- Doesn't match Custom Messages
message :: SServerMethod m -> Session (ServerMessage m)
message (SCustomMethod _) = error "message can't be used with CustomMethod, use customRequest or customNotification instead"
message m1 = named (T.pack $ "Request for: " <> show m1) $ satisfyMaybe $ \case
  FromServerMess m2 msg -> do
    res <- mEqServer m1 m2
    case res of
      Right HRefl -> pure msg
      Left _f -> Nothing
  _ -> Nothing

customRequest :: T.Text -> Session (ServerMessage (CustomMethod :: Method FromServer Request))
customRequest m = named m $ satisfyMaybe $ \case
  FromServerMess m1 msg -> case splitServerMethod m1 of
    IsServerEither -> case msg of
      ReqMess _ | m1 == SCustomMethod m -> Just msg
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

customNotification :: T.Text -> Session (ServerMessage (CustomMethod :: Method FromServer Notification))
customNotification m = named m $ satisfyMaybe $ \case
  FromServerMess m1 msg -> case splitServerMethod m1 of
    IsServerEither -> case msg of
      NotMess _ | m1 == SCustomMethod m -> Just msg
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
response :: SMethod (m :: Method FromClient Request) -> Session (ResponseMessage m)
response m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  FromServerRsp m2 msg -> do
    HRefl <- runEq mEqClient m1 m2
    pure msg
  _ -> Nothing

-- | Like 'response', but matches a response for a specific id.
responseForId :: SMethod (m :: Method FromClient Request) -> LspId m -> Session (ResponseMessage m)
responseForId m lid = named (T.pack $ "Response for id: " ++ show lid) $ do
  satisfyMaybe $ \msg -> do
    case msg of
      FromServerMess _ _ -> Nothing
      FromServerRsp m' rspMsg@(ResponseMessage _ lid' _) -> do
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
    shouldSkip (FromServerMess SWindowLogMessage _) = True
    shouldSkip (FromServerMess SWindowShowMessage _) = True
    shouldSkip (FromServerMess SWindowShowMessageRequest _) = True
    shouldSkip _ = False

-- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
-- (textDocument/publishDiagnostics) notification.
publishDiagnosticsNotification :: Session (Message TextDocumentPublishDiagnostics)
publishDiagnosticsNotification = named "Publish diagnostics notification" $
  satisfyMaybe $ \msg -> case msg of
    FromServerMess STextDocumentPublishDiagnostics diags -> Just diags
    _ -> Nothing
