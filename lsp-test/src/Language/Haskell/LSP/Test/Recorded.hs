{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | A testing tool for replaying recorded client logs back to a server,
-- and validating that the server output matches up with another log.
module Language.Haskell.LSP.Test.Recorded
  ( replay,
    sendNextRequest
  )
where

import           Control.Concurrent
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8    as B
import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types    as LSP
import           Data.Aeson
import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           System.IO
import           System.Directory
import           System.FilePath
import           System.Process
import           Language.Haskell.LSP.Test.Files
import           Language.Haskell.LSP.Test.Parsing

data SessionContext = SessionContext
  {
    reqSema :: MVar FromServerMessage,
    rspSema :: MVar LSP.LspId,
    serverIn :: Handle
  }
type Session = StateT [FromClientMessage] (ReaderT SessionContext IO)

-- | Replays a recorded client output and 
-- makes sure it matches up with an expected response.
replay :: FilePath -- ^ The recorded session directory.
       -> Session a
       -> IO ()
replay sessionDir session = do

  let sessionFp = sessionDir </> "session.log"

  (Just serverIn, Just serverOut, _, serverProc) <- createProcess
    (proc "hie" ["--lsp", "-d", "-l", "/tmp/test-hie.log"]) { std_in  = CreatePipe
                                                 , std_out = CreatePipe
                                                 }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  -- whether to send the next request
  reqSema <- newEmptyMVar
  -- whether to send the next response
  rspSema <- newEmptyMVar

  entries <- B.lines <$> B.readFile sessionFp

  -- decode session
  let unswappedEvents = map (fromJust . decode) entries

  events <- swapFiles sessionDir unswappedEvents

  let clientEvents = map (\(FromClient _ msg) -> msg) $ filter isClientMsg events
      requestMap = getRequestMap clientEvents
      context = (SessionContext rspSema reqSema serverIn)

  -- listen to server
  forkIO $ listenServer serverOut requestMap context

  runReaderT (runStateT session clientEvents) context

  terminateProcess serverProc
  
  where
    isClientMsg (FromClient _ _) = True
    isClientMsg _                = False

    isServerMsg (FromServer _ _) = True
    isServerMsg _                = False

sendNextRequest :: Session FromServerMessage
sendNextRequest = do
  (nextMsg:remainingMsgs) <- get
  put remainingMsgs
  case nextMsg of
    ReqInitialize               m -> request m
    ReqShutdown                 m -> request m
    ReqHover                    m -> request m
    ReqCompletion               m -> request m
    ReqCompletionItemResolve    m -> request m
    ReqSignatureHelp            m -> request m
    ReqDefinition               m -> request m
    ReqFindReferences           m -> request m
    ReqDocumentHighlights       m -> request m
    ReqDocumentSymbols          m -> request m
    ReqWorkspaceSymbols         m -> request m
    ReqCodeAction               m -> request m
    ReqCodeLens                 m -> request m
    ReqCodeLensResolve          m -> request m
    ReqDocumentFormatting       m -> request m
    ReqDocumentRangeFormatting  m -> request m
    ReqDocumentOnTypeFormatting m -> request m
    ReqRename                   m -> request m
    ReqExecuteCommand           m -> request m
    ReqDocumentLink             m -> request m
    ReqDocumentLinkResolve      m -> request m
    ReqWillSaveWaitUntil        m -> request m
    RspApplyWorkspaceEdit       m -> response m
    RspFromClient               m -> response m
    NotInitialized              m -> notification m
    NotExit                     m -> notification m
    NotCancelRequestFromClient  m -> notification m
    NotDidChangeConfiguration   m -> notification m
    NotDidOpenTextDocument      m -> notification m
    NotDidChangeTextDocument    m -> notification m
    NotDidCloseTextDocument     m -> notification m
    NotWillSaveTextDocument     m -> notification m
    NotDidSaveTextDocument      m -> notification m
    NotDidChangeWatchedFiles    m -> notification m
    UnknownFromClientMessage m ->
      error $ "Unknown message was recorded from the client" ++ show m
 where
  -- TODO: May need to prevent premature exit notification being sent
  notification msg@(LSP.NotificationMessage _ LSP.Exit _) = do
    context <- lift ask

    liftIO $ do
      putStrLn "Will send exit notification soon"
      threadDelay 10000000
      B.hPut (serverIn context) $ addHeader (encode msg)
    
    error "Done"

  notification msg@(LSP.NotificationMessage _ m _) = do
    context <- lift ask

    liftIO $ B.hPut (serverIn context) $ addHeader (encode msg)

    liftIO $ putStrLn $ "Sent a notification " ++ show m
    
    sendNextRequest

  request msg@(LSP.RequestMessage _ id m _) = do
    context <- lift ask

    liftIO $ do

      print $ addHeader $ encode msg

      B.hPut (serverIn context) $ addHeader (encode msg)
      putStrLn $  "Sent a request id " ++ show id ++ ": " ++ show m ++ "\nWaiting for a response"

      rsp <- takeMVar (reqSema context)
      -- when (LSP.responseId id /= rsp ^. LSP.id) $ 
      --   error $ "Expected id " ++ show id ++ ", got " ++ show (rsp ^. LSP.id)
      
      return rsp

  response msg@(LSP.ResponseMessage _ id _ _) = do
    context <- lift ask

    liftIO $ do
      putStrLn $ "Waiting for request id " ++ show id ++ " from the server"
      reqId <- takeMVar (rspSema context)
      if LSP.responseId reqId /= id
        then error $ "Expected id " ++ show reqId ++ ", got " ++ show reqId
        else do
          B.hPut (serverIn context) $ addHeader (encode msg)
          putStrLn $ "Sent response to request id " ++ show id

    sendNextRequest


-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: Handle -> RequestMap -> SessionContext -> IO ()
listenServer h reqMap context = do

  msgBytes <- getNextMessage h

  let msg = decodeFromServerMsg reqMap msgBytes

  print msg

  case msg of
    ReqRegisterCapability       m -> request m
    ReqApplyWorkspaceEdit       m -> request m
    ReqShowMessage              m -> request m
    ReqUnregisterCapability     m -> request m
    RspInitialize               m -> response m msg
    RspShutdown                 m -> response m msg
    RspHover                    m -> response m msg
    RspCompletion               m -> response m msg
    RspCompletionItemResolve    m -> response m msg
    RspSignatureHelp            m -> response m msg
    RspDefinition               m -> response m msg
    RspFindReferences           m -> response m msg
    RspDocumentHighlights       m -> response m msg
    RspDocumentSymbols          m -> response m msg
    RspWorkspaceSymbols         m -> response m msg
    RspCodeAction               m -> response m msg
    RspCodeLens                 m -> response m msg
    RspCodeLensResolve          m -> response m msg
    RspDocumentFormatting       m -> response m msg
    RspDocumentRangeFormatting  m -> response m msg
    RspDocumentOnTypeFormatting m -> response m msg
    RspRename                   m -> response m msg
    RspExecuteCommand           m -> response m msg
    RspError                    m -> response m msg
    RspDocumentLink             m -> response m msg
    RspDocumentLinkResolve      m -> response m msg
    RspWillSaveWaitUntil        m -> response m msg
    NotPublishDiagnostics       m -> notification m
    NotLogMessage               m -> notification m
    NotShowMessage              m -> notification m
    NotTelemetry                m -> notification m
    NotCancelRequestFromServer  m -> notification m

  listenServer h reqMap context

  where
  response :: Show a => LSP.ResponseMessage a -> FromServerMessage -> IO ()
  response res wrappedMsg = do
    putStrLn $ "Got response for id " ++ show (res ^. LSP.id)

    putMVar (reqSema context) wrappedMsg -- send back the response for the request we're waiting on

  request :: Show a => LSP.RequestMessage LSP.ServerMethod a b -> IO ()
  request req = do
    putStrLn
      $  "Got request for id "
      ++ show (req ^. LSP.id)
      ++ " "
      ++ show (req ^. LSP.method)

    putMVar (rspSema context) (req ^. LSP.id) -- unblock the handler waiting for a response

  notification :: Show a => LSP.NotificationMessage LSP.ServerMethod a -> IO ()
  notification n = putStrLn $ "Got notification " ++ show (n ^. LSP.method)

  --   lift
  --     $  putStrLn
  --     $  show (length (filter isNotification expectedMsgs) - 1)
  --     ++ " notifications remaining"

  -- checkOrder msg = unless (inRightOrder msg expectedMsgs) $ failSession
  --   (  "Out of order\nExpected\n"
  --   ++ show firstExpected
  --   ++ "\nGot\n"
  --   ++ show msg
  --   ++ "\n"
  --   )

  -- markReceived :: FromServerMessage -> Session [FromServerMessage]
  -- markReceived msg =
  --   let new = delete msg expectedMsgs
  --   in  if new == expectedMsgs
  --         then failSession ("Unexpected message: " ++ show msg) >> return new
  --         else return new

  -- firstExpected = head $ filter (not . isNotification) expectedMsgs

isNotification :: FromServerMessage -> Bool
isNotification (NotPublishDiagnostics      _) = True
isNotification (NotLogMessage              _) = True
isNotification (NotShowMessage             _) = True
isNotification (NotCancelRequestFromServer _) = True
isNotification _                              = False

-- TODO: QuickCheck tests?
-- | Checks wether or not the message appears in the right order
-- @ N1 N2 N3 REQ1 N4 N5 REQ2 RES1 @
-- given N2, notification order doesn't matter.
-- @ N1 N3 REQ1 N4 N5 REQ2 RES1 @
-- given REQ1
-- @ N1 N3 N4 N5 REQ2 RES1 @
-- given RES1
-- @ N1 N3 N4 N5 XXXX RES1 @ False!
-- Order of requests and responses matter
inRightOrder :: FromServerMessage -> [FromServerMessage] -> Bool

inRightOrder _ [] = error "Why is this empty"
-- inRightOrder (LSP.NotificationMessage _ _ _) _ = True

inRightOrder received (expected : msgs)
  | received == expected    = True
  | isNotification expected = inRightOrder received msgs
  | otherwise               = False

-- | The internal monad for tests that can fail or pass,
-- ending execution early.
-- type Session = ReaderT (MVar Bool) IO

-- -- TODO: Make return type polymoprhic more like error
-- failSession :: String -> Session ()
-- failSession reason = do
--   lift $ putStrLn reason
--   passVar <- ask
--   lift $ putMVar passVar False

-- passSession :: Session ()
-- passSession = do
--   passVar <- ask
--   lift $ putMVar passVar True