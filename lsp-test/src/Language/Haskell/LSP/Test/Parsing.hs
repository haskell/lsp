{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.LSP.Test.Parsing where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as B
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Test.Messages
import Language.Haskell.LSP.Test.Decoding
import System.IO
import Control.Concurrent
import Text.Parsec hiding (satisfy)

data MessageParserState = MessageParserState

data SessionContext = SessionContext
  {
    serverIn :: Handle,
    rootDir :: FilePath,
    messageChan :: Chan FromServerMessage,
    requestMap :: MVar RequestMap
  }

newtype SessionState = SessionState
  {
    curReqId :: LspId
  }

type Session = ParsecT (Chan FromServerMessage) SessionState (ReaderT SessionContext IO)

notification :: Session FromServerMessage
notification = satisfy isServerNotification

request :: Session FromServerMessage
request = satisfy isServerRequest

response :: Session FromServerMessage
response = satisfy isServerResponse

loggingNotification :: Session FromServerMessage
loggingNotification = satisfy shouldSkip
  where
    shouldSkip (NotLogMessage _) = True
    shouldSkip (NotShowMessage _) = True
    shouldSkip (ReqShowMessage _) = True
    shouldSkip _ = False

satisfy :: (Stream s m a, Eq a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfy pred = tokenPrim show nextPos test
  where nextPos x _ _ = x
        test x = if pred x then Just x else Nothing

testLog = NotLogMessage (NotificationMessage "2.0" WindowLogMessage (LogMessageParams MtLog "Hello world"))

testSymbols = RspDocumentSymbols (ResponseMessage "2.0" (IdRspInt 0) (Just (List [])) Nothing)

instance (MonadIO m) => Stream (Chan a) m a where
  uncons c = do
    x <- liftIO $ readChan c
    return $ Just (x, c)
