{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Server.IO (serverOut, serverIn, LspIoLog) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson qualified as J
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.Attoparsec.ByteString.Char8
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Prettyprint.Doc

data LspIoLog
  = HeaderParseFail [String] String
  | BodyParseFail String
  | RecvMsg BS.ByteString
  | SendMsg BS.ByteString
  | EOF
  deriving (Show)

instance Pretty LspIoLog where
  pretty (HeaderParseFail ctxs err) =
    vsep
      [ "Failed to parse message header:"
      , pretty (intercalate " > " ctxs) <> ": " <+> pretty err
      ]
  pretty (BodyParseFail err) =
    vsep
      [ "Failed to parse message body:"
      , pretty err
      ]
  pretty (RecvMsg msg) = "---> " <> pretty (T.decodeUtf8 msg)
  pretty (SendMsg msg) = "<--- " <> pretty (T.decodeUtf8 msg)
  pretty EOF = "Got EOF"

-- | Process which receives messages and sends them. Output queue of messages ensures they are serialised.
serverIn ::
  LogAction IO (WithSeverity LspIoLog) ->
  -- | Channel to send out messages on.
  (J.Value -> IO ()) ->
  -- | Action to pull in new messages (e.g. from a handle).
  IO BS.ByteString ->
  IO ()
serverIn logger msgOut clientIn = do
  bs <- clientIn
  loop (parse parser bs)
 where
  loop :: Result BS.ByteString -> IO ()
  loop (Fail _ ctxs err) = do
    logger <& HeaderParseFail ctxs err `WithSeverity` Error
    pure ()
  loop (Partial c) = do
    bs <- clientIn
    if BS.null bs
      then do
        logger <& EOF `WithSeverity` Error
        pure ()
      else loop (c bs)
  loop (Done remainder parsed) = do
    logger <& RecvMsg parsed `WithSeverity` Debug
    case J.eitherDecode (BSL.fromStrict parsed) of
      -- Note: this is recoverable, because we can just discard the
      -- message and keep going, whereas a header parse failure is
      -- not recoverable
      Left err -> logger <& BodyParseFail err `WithSeverity` Error
      Right msg -> msgOut msg
    loop (parse parser remainder)

  parser = do
    try contentType <|> (return ())
    len <- contentLength
    try contentType <|> (return ())
    _ <- string _ONE_CRLF
    Attoparsec.take len

  contentLength = do
    _ <- string "Content-Length: "
    len <- decimal
    _ <- string _ONE_CRLF
    return len

  contentType = do
    _ <- string "Content-Type: "
    skipWhile (/= '\r')
    _ <- string _ONE_CRLF
    return ()

-- | Process which receives messages and sends them. Input queue of messages ensures they are serialised.
serverOut ::
  LogAction IO (WithSeverity LspIoLog) ->
  -- | Channel to receive messages on.
  IO J.Value ->
  -- | Action to send messages out on (e.g. via a handle).
  (BS.ByteString -> IO ()) ->
  IO ()
serverOut logger msgIn clientOut = forever $ do
  msg <- msgIn

  -- We need to make sure we only send over the content of the message,
  -- and no other tags/wrapper stuff
  let str = J.encode msg

  let out =
        BS.concat
          [ T.encodeUtf8 $ T.pack $ "Content-Length: " ++ show (BSL.length str)
          , _TWO_CRLF
          , BSL.toStrict str
          ]

  clientOut out
  logger <& SendMsg out `WithSeverity` Debug

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"
