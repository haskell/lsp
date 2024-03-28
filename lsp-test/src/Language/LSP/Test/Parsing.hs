{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Language.LSP.Protocol.Message qualified as LSP
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
satisfy :: (LSP.FromServerMessage -> Bool) -> Session LSP.FromServerMessage
satisfy pred = satisfyMaybe (\msg -> if pred msg then Just msg else Nothing)

{- | Consumes and returns the result of the specified predicate if it returns `Just`.

 @since 0.6.1.0
-}
satisfyMaybe :: (LSP.FromServerMessage -> Maybe a) -> Session a
satisfyMaybe pred = satisfyMaybeM (pure . pred)

satisfyMaybeM :: (LSP.FromServerMessage -> Session (Maybe a)) -> Session a
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
message :: LSP.SServerMethod m -> Session (LSP.TMessage m)
message (LSP.SMethod_CustomMethod _) = error "message can't be used with CustomMethod, use customRequest or customNotification instead"
message m1 = named (T.pack $ "Request for: " <> show m1) $ satisfyMaybe $ \case
  LSP.FromServerMess m2 msg -> do
    res <- LSP.mEqServer m1 m2
    case res of
      Right HRefl -> pure msg
      Left _f -> Nothing
  _ -> Nothing

customRequest :: KnownSymbol s => Proxy s -> Session (LSP.TMessage (LSP.Method_CustomMethod s :: LSP.Method LSP.ServerToClient LSP.Request))
customRequest p =
  let m = T.pack $ symbolVal p
   in named m $ satisfyMaybe $ \case
        LSP.FromServerMess m1 msg -> case LSP.splitServerMethod m1 of
          LSP.IsServerEither -> case msg of
            LSP.ReqMess _ -> case m1 `geq` LSP.SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

customNotification :: KnownSymbol s => Proxy s -> Session (LSP.TMessage (LSP.Method_CustomMethod s :: LSP.Method LSP.ServerToClient LSP.Notification))
customNotification p =
  let m = T.pack $ symbolVal p
   in named m $ satisfyMaybe $ \case
        LSP.FromServerMess m1 msg -> case LSP.splitServerMethod m1 of
          LSP.IsServerEither -> case msg of
            LSP.NotMess _ -> case m1 `geq` LSP.SMethod_CustomMethod p of
              Just Refl -> Just msg
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

-- | Matches if the message is a notification.
anyNotification :: Session LSP.FromServerMessage
anyNotification = named "Any notification" $ satisfy $ \case
  LSP.FromServerMess m msg -> case LSP.splitServerMethod m of
    LSP.IsServerNot -> True
    LSP.IsServerEither -> case msg of
      LSP.NotMess _ -> True
      _ -> False
    _ -> False
  LSP.FromServerRsp _ _ -> False

-- | Matches if the message is a request.
anyRequest :: Session LSP.FromServerMessage
anyRequest = named "Any request" $ satisfy $ \case
  LSP.FromServerMess m _ -> case LSP.splitServerMethod m of
    LSP.IsServerReq -> True
    _ -> False
  LSP.FromServerRsp _ _ -> False

-- | Matches if the message is a response.
anyResponse :: Session LSP.FromServerMessage
anyResponse = named "Any response" $ satisfy $ \case
  LSP.FromServerMess _ _ -> False
  LSP.FromServerRsp _ _ -> True

-- | Matches a response coming from the server.
response :: LSP.SMethod (m :: LSP.Method LSP.ClientToServer LSP.Request) -> Session (LSP.TResponseMessage m)
response m1 = named (T.pack $ "Response for: " <> show m1) $ satisfyMaybe $ \case
  LSP.FromServerRsp m2 msg -> do
    HRefl <- LSP.runEq LSP.mEqClient m1 m2
    pure msg
  _ -> Nothing

-- | Like 'response', but matches a response for a specific id.
responseForId :: LSP.SMethod (m :: LSP.Method LSP.ClientToServer LSP.Request) -> LSP.LspId m -> Session (LSP.TResponseMessage m)
responseForId m lid = named (T.pack $ "Response for id: " ++ show lid) $ do
  satisfyMaybe $ \msg -> do
    case msg of
      LSP.FromServerMess _ _ -> Nothing
      LSP.FromServerRsp m' rspMsg@(LSP.TResponseMessage _ lid' _) -> do
        HRefl <- LSP.runEq LSP.mEqClient m m'
        guard (Just lid == lid')
        pure rspMsg

-- | Matches any type of message.
anyMessage :: Session LSP.FromServerMessage
anyMessage = satisfy (const True)

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: Session LSP.FromServerMessage
loggingNotification = named "Logging notification" $ satisfy shouldSkip
 where
  shouldSkip (LSP.FromServerMess LSP.SMethod_WindowLogMessage _) = True
  shouldSkip (LSP.FromServerMess LSP.SMethod_WindowShowMessage _) = True
  shouldSkip (LSP.FromServerMess LSP.SMethod_WindowShowMessageRequest _) = True
  shouldSkip (LSP.FromServerMess LSP.SMethod_WindowShowDocument _) = True
  shouldSkip _ = False

-- | Matches if the message is a configuration request from the server.
configurationRequest :: Session LSP.FromServerMessage
configurationRequest = named "Configuration request" $ satisfy shouldSkip
 where
  shouldSkip (LSP.FromServerMess LSP.SMethod_WorkspaceConfiguration _) = True
  shouldSkip _ = False

loggingOrConfiguration :: Session LSP.FromServerMessage
loggingOrConfiguration = loggingNotification <|> configurationRequest

{- | Matches a 'Language.LSP.Types.TextDocumentPublishDiagnostics'
 (textDocument/publishDiagnostics) notification.
-}
publishDiagnosticsNotification :: Session (LSP.TMessage LSP.Method_TextDocumentPublishDiagnostics)
publishDiagnosticsNotification = named "Publish diagnostics notification" $
  satisfyMaybe $ \msg -> case msg of
    LSP.FromServerMess LSP.SMethod_TextDocumentPublishDiagnostics diags -> Just diags
    _ -> Nothing
