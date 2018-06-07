{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.LSP.Test.Parsing where

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Test.Messages
import Control.Concurrent
import Text.Parsec hiding (satisfy)
import Control.Monad

data MessageParserState = MessageParserState

type MessageParser = ParsecT (Chan FromServerMessage) MessageParserState IO

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

instance Stream (Chan a) IO a where
  uncons c = do
    x <- readChan c
    return $ Just (x, c)

test :: IO ()
test = do
  chan <- newChan
  let parser = do
        n <- count 2 notification
        rsp <- response
        return (n, rsp)
  forkIO $ forM_ [testLog, testLog, testSymbols] $ \x -> do
    writeChan chan x
    threadDelay 1000000
  x <- runParserT parser MessageParserState "" chan
  print x