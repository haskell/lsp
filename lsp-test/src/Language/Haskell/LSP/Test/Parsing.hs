{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Parsing where

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Test.Messages
import Text.Parsec hiding (satisfy)

data MessageParserState = MessageParserState

type MessageParser = Parsec [FromServerMessage] MessageParserState

notification :: MessageParser FromServerMessage
notification = satisfy isServerNotification

request :: MessageParser FromServerMessage
request = satisfy isServerRequest

response :: MessageParser FromServerMessage
response = satisfy isServerResponse

satisfy :: (Stream s m a, Eq a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfy pred = tokenPrim show nextPos test
  where nextPos x _ _ = x
        test x = if pred x then Just x else Nothing

testLog = NotLogMessage (NotificationMessage "2.0" WindowLogMessage (LogMessageParams MtLog "Hello world"))

testSymbols = RspDocumentSymbols (ResponseMessage "2.0" (IdRspInt 0) (Just (List [])) Nothing)

parseMessages :: MessageParser a -> [FromServerMessage] -> Either ParseError a
parseMessages parser = runP parser MessageParserState ""