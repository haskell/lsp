{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.LSP.Test.Parsing
  ( -- $receiving
    satisfy
  , message
  , anyRequest
  , anyResponse
  , anyNotification
  , anyMessage
  , loggingNotification
  , publishDiagnosticsNotification
  , responseForId
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit.Parser hiding (named)
import qualified Data.Conduit.Parser (named)
import qualified Data.Text as T
import Data.Typeable
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as LSP
import Language.Haskell.LSP.Test.Messages
import Language.Haskell.LSP.Test.Session

-- $receiving
-- To receive a message, just specify the type that expect:
--
-- @
-- msg1 <- message :: Session ApplyWorkspaceEditRequest
-- msg2 <- message :: Session HoverResponse
-- @
--
-- 'Language.Haskell.LSP.Test.Session' is actually just a parser
-- that operates on messages under the hood. This means that you
-- can create and combine parsers to match speicifc sequences of
-- messages that you expect.
--
-- For example, if you wanted to match either a definition or
-- references request:
--
-- > defOrImpl = (message :: Session DefinitionRequest)
-- >          <|> (message :: Session ReferencesRequest)
--
-- If you wanted to match any number of telemetry
-- notifications immediately followed by a response:
--
-- @
-- logThenDiags =
--  skipManyTill (message :: Session TelemetryNotification)
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
satisfyMaybe pred = do

  skipTimeout <- overridingTimeout <$> get
  timeoutId <- curTimeoutId <$> get
  unless skipTimeout $ do
    chan <- asks messageChan
    timeout <- asks (messageTimeout . config)
    void $ liftIO $ forkIO $ do
      threadDelay (timeout * 1000000)
      writeChan chan (TimeoutMessage timeoutId)

  x <- Session await

  unless skipTimeout $
    modify $ \s -> s { curTimeoutId = timeoutId + 1 }

  modify $ \s -> s { lastReceivedMessage = Just x }

  case pred x of
    Just a -> do
      logMsg LogServer x
      return a
    Nothing -> empty

named :: T.Text -> Session a -> Session a
named s (Session x) = Session (Data.Conduit.Parser.named s x)

-- | Matches a message of type @a@.
message :: forall a. (Typeable a, FromJSON a) => Session a
message =
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe a
  in named (T.pack $ show $ head $ snd $ splitTyConApp $ last $ typeRepArgs $ typeOf parser) $
     satisfyMaybe parser

-- | Matches if the message is a notification.
anyNotification :: Session FromServerMessage
anyNotification = named "Any notification" $ satisfy isServerNotification

-- | Matches if the message is a request.
anyRequest :: Session FromServerMessage
anyRequest = named "Any request" $ satisfy isServerRequest

-- | Matches if the message is a response.
anyResponse :: Session FromServerMessage
anyResponse = named "Any response" $ satisfy isServerResponse

-- | Matches a response for a specific id.
responseForId :: forall a. FromJSON a => LspId -> Session (ResponseMessage a)
responseForId lid = named (T.pack $ "Response for id: " ++ show lid) $ do
  let parser = decode . encodeMsg :: FromServerMessage -> Maybe (ResponseMessage a)
  satisfyMaybe $ \msg -> do
    z <- parser msg
    guard (z ^. LSP.id == responseId lid)
    pure z

-- | Matches any type of message.
anyMessage :: Session FromServerMessage
anyMessage = satisfy (const True)

-- | A version of encode that encodes FromServerMessages as if they
-- weren't wrapped.
encodeMsg :: FromServerMessage -> B.ByteString
encodeMsg = encode . genericToJSON (defaultOptions { sumEncoding = UntaggedValue })

-- | Matches if the message is a log message notification or a show message notification/request.
loggingNotification :: Session FromServerMessage
loggingNotification = named "Logging notification" $ satisfy shouldSkip
  where
    shouldSkip (NotLogMessage _) = True
    shouldSkip (NotShowMessage _) = True
    shouldSkip (ReqShowMessage _) = True
    shouldSkip _ = False

-- | Matches a 'Language.Haskell.LSP.Test.PublishDiagnosticsNotification'
-- (textDocument/publishDiagnostics) notification.
publishDiagnosticsNotification :: Session PublishDiagnosticsNotification
publishDiagnosticsNotification = named "Publish diagnostics notification" $
  satisfyMaybe $ \msg -> case msg of
    NotPublishDiagnostics diags -> Just diags
    _ -> Nothing
