module JSONRPC.IO where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Control.Applicative ((<|>))
import Control.Exception (bracket_)
import Control.Monad
import Data.Aeson qualified as J
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.Attoparsec.ByteString.Char8
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import JSONRPC.Message
import Language.LSP.Protocol.Types (Null (..), type (|?) (..))
import Prettyprinter

data IoLog
  = HeaderParseFail [String] String
  | BodyParseFail String
  | RecvMsg BS.ByteString
  | SendMsg BS.ByteString
  | StartingSend
  | StoppingSend
  | StartingRecv
  | StoppingRecv
  | EOF
  deriving stock (Show)

instance Pretty IoLog where
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
  pretty (RecvMsg msg) = vsep ["---> ", pretty (T.decodeUtf8 msg)]
  pretty (SendMsg msg) = vsep ["<--- ", pretty (T.decodeUtf8 msg)]
  pretty StartingSend = "Starting send thread"
  pretty StoppingSend = "Stopping send thread"
  pretty StartingRecv = "Starting receive thread"
  pretty StoppingRecv = "Stopping receive thread"
  pretty EOF = "Got EOF"

-- | Thread which receives JSONRPC messages, parses them, and sends them on.
recvThread ::
  LogAction IO (WithSeverity IoLog) ->
  -- | Action to pull in new messages (e.g. from a handle).
  IO BS.ByteString ->
  -- | Action to send out messages on.
  (Message -> IO ()) ->
  -- | Action to send messages to the client (for errors).
  (Message -> IO ()) ->
  IO ()
recvThread logger clientIn msgOut clientOut =
  bracket_
    (logger <& StartingRecv `WithSeverity` Debug)
    (logger <& StoppingRecv `WithSeverity` Debug)
    $ do
      bs <- clientIn
      loop (parse parser bs)
 where
  loop :: Result BS.ByteString -> IO ()
  loop (Fail _ ctxs err) = do
    logger <& HeaderParseFail ctxs err `WithSeverity` Error
    -- exit
    pure ()
  loop (Partial c) = do
    bs <- clientIn
    if BS.null bs
      then do
        logger <& EOF `WithSeverity` Error
        -- exit
        pure ()
      else loop (c bs)
  loop (Done remainder parsed) = do
    let pmsg = J.eitherDecode $ BSL.fromStrict parsed
    -- TODO: figure out how to re-enable
    -- logger <& RecvMsg parsed `WithSeverity` Debug
    case pmsg of
      -- Note: this is recoverable, because we can just discard the
      -- message and keep going, whereas a header parse failure is
      -- not recoverable
      Left err -> do
        logger <& BodyParseFail err `WithSeverity` Error
        let rerr = ResponseError (-32700) (T.pack err) Nothing
        clientOut $ Rsp $ ResponseMessage (InR Null) Nothing (Just rerr)
      Right msg -> msgOut msg
    loop (parse parser remainder)

  parser = do
    try contentType <|> return ()
    len <- contentLength
    try contentType <|> return ()
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

-- | Process which receives protocol messages and sends them as JSONRPC messages.
sendThread ::
  LogAction IO (WithSeverity IoLog) ->
  -- | Action to receive messages.
  IO Message ->
  -- | Action to send serialised messages.
  (BS.ByteString -> IO ()) ->
  IO ()
sendThread logger msgIn clientOut =
  bracket_
    (logger <& StartingSend `WithSeverity` Debug)
    (logger <& StoppingSend `WithSeverity` Debug)
    $ forever
    $ do
      msg <- msgIn

      -- We need to make sure we only send over the content of the message,
      -- and no other tags/wrapper stuff
      let str = BSL.toStrict $ J.encode msg

      let out =
            BS.concat
              [ T.encodeUtf8 $ T.pack $ "Content-Length: " ++ show (BS.length str)
              , _TWO_CRLF
              , str
              ]

      -- TODO: figure out how to re-enable
      -- logger <& SendMsg out `WithSeverity` Debug
      clientOut out

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"
