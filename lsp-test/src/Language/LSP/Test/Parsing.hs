{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.LSP.Test.Parsing (
  -- $receiving
  satisfy,
  satisfyMaybe,
  message,
  request,
  notification,
  response,
  responseForId,
  --customRequest,
  --customNotification,
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
import Language.LSP.Protocol.Message
import Language.LSP.Test.Session
import JSONRPC.Typed.Method (Role (..))
import qualified Language.LSP.Protocol.Message as LSP
import JSONRPC.Typed.Message hiding (message)
import JSONRPC.Id

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
satisfy :: (SomeMessage Server LSP.Method -> Bool) -> Session (SomeMessage Server LSP.Method)
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

{- | Consumes and returns the result of the specified predicate if it returns `Just`.

 @since 0.6.1.0
-}
satisfyMaybe :: (SomeMessage Server LSP.Method -> Maybe a) -> Session a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (SomeMessage Server LSP.Method -> Session (Maybe a)) -> Session a
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
message :: forall (m :: LSP.Method) . SMethod m -> Session (Message Server LSP.Method m)
message m1 = named (T.pack $ "Request for: " <> show m1) $ satisfyMaybe $ \case
  SomeMessage msg@(Req m _) | Just Refl <- geq m m1 -> Just msg
  SomeMessage msg@(Not m _) | Just Refl <- geq m m1 -> Just msg
  SomeMessage msg@(Rsp m _) | Just Refl <- geq m m1 -> Just msg
  _ -> Nothing

-- TODO: resurrect?

{-
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
-}

-- | Matches if the message is a notification.
anyNotification :: Session (SomeMessage Server LSP.Method)
anyNotification = named "Any notification" $ satisfy $ \case
  SomeMessage (Not _ _) -> True
  _ -> False

-- | Matches if the message is a request.
anyRequest :: Session (SomeMessage Server LSP.Method)
anyRequest = named "Any request" $ satisfy $ \case
  SomeMessage (Req _ _) -> True
  _ -> False

-- | Matches if the message is a response.
anyResponse :: Session (SomeMessage Server LSP.Method)
anyResponse = named "Any response" $ satisfy $ \case
  SomeMessage (Rsp _ _) -> True
  _ -> False

-- | Matches a request coming from the server.
request :: SMethod m -> Session (RequestMessage m)
request m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  SomeMessage (Req m msg) | Just Refl <- geq m m1 -> Just msg
  _ -> Nothing

-- | Matches a notification coming from the server.
notification :: SMethod m -> Session (NotificationMessage m)
notification m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  SomeMessage (Not m msg) | Just Refl <- geq m m1 -> Just msg
  _ -> Nothing

-- | Matches a response coming from the server.
response :: SMethod m -> Session (ResponseMessage m)
response m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  SomeMessage (Rsp m msg) | Just Refl <- geq m m1 -> Just msg
  _ -> Nothing


-- | Like 'response', but matches a response for a specific id.
responseForId :: SMethod m -> Id -> Session (ResponseMessage m)
responseForId m1 lid = named (T.pack $ "Response for id: " ++ show lid) $ satisfyMaybe $ \case
  SomeMessage (Rsp m msg) | Just Refl <- geq m m1, msg.id == lid -> Just msg
  _ -> Nothing

-- | Matches any type of message.
anyMessage :: Session (SomeMessage Server LSP.Method)
anyMessage = satisfy (const True)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: Session (SomeMessage Server LSP.Method)
loggingNotification = named "Logging notification" $ satisfy shouldSkip
 where
   shouldSkip :: SomeMessage Server LSP.Method -> Bool
   shouldSkip (SomeMessage (Not SMethod_WindowLogMessage _)) = True
   shouldSkip (SomeMessage (Not SMethod_WindowShowMessage _)) = True
   shouldSkip (SomeMessage (Req SMethod_WindowShowMessageRequest _)) = True
   shouldSkip (SomeMessage (Req SMethod_WindowShowDocument _)) = True
   shouldSkip _ = False


-- | Matches if the message is a configuration request from the server.
configurationRequest :: Session (SomeMessage Server LSP.Method)
configurationRequest = named "Configuration request" $ satisfy shouldSkip
 where
   shouldSkip :: SomeMessage Server LSP.Method -> Bool
   shouldSkip (SomeMessage (Req SMethod_WorkspaceConfiguration _)) = True
   shouldSkip _ = False

loggingOrConfiguration :: Session (SomeMessage Server LSP.Method)
loggingOrConfiguration = loggingNotification <|> configurationRequest

{- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
 (textDocument/publishDiagnostics) notification.
-}
publishDiagnosticsNotification :: Session (NotificationMessage Method_TextDocumentPublishDiagnostics)
publishDiagnosticsNotification = named "Publish diagnostics notification" $
  satisfyMaybe $ \case
    SomeMessage (Not SMethod_TextDocumentPublishDiagnostics diags) -> Just diags
    _ -> Nothing
