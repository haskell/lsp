{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | A testing tool for replaying recorded client logs back to a server,
-- and validating that the server output matches up with another log.
module Language.Haskell.LSP.Test.Recorded
  ( replaySession
  )
where

import           Prelude hiding (id)
import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8    as B
import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types hiding (error)
import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           System.IO
import           System.FilePath
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Files
import           Language.Haskell.LSP.Test.Parsing


-- | Replays a recorded client output and 
-- makes sure it matches up with an expected response.
replaySession :: FilePath -- ^ The recorded session directory.
              -> IO Bool
replaySession sessionDir = do

  entries <- B.lines <$> B.readFile (sessionDir </> "session.log")

  -- decode session
  let unswappedEvents = map (fromJust . decode) entries

  events <- swapFiles sessionDir unswappedEvents

  let clientEvents = filter isClientMsg events
      serverEvents = filter isServerMsg events
      clientMsgs = map (\(FromClient _ msg) -> msg) clientEvents
      serverMsgs = filter (not . shouldSkip) $ map (\(FromServer _ msg) -> msg) serverEvents
      requestMap = getRequestMap clientMsgs


  reqSema <- newEmptyMVar
  rspSema <- newEmptyMVar
  passVar <- newEmptyMVar :: IO (MVar Bool)

  forkIO $ runSessionWithHandler (listenServer serverMsgs requestMap reqSema rspSema passVar) sessionDir $
    sendMessages clientMsgs reqSema rspSema

  takeMVar passVar

  where
    isClientMsg (FromClient _ _) = True
    isClientMsg _                = False

    isServerMsg (FromServer _ _) = True
    isServerMsg _                = False

sendMessages :: [FromClientMessage] -> MVar LspId -> MVar LspIdRsp -> Session ()
sendMessages [] _ _ = return ()
sendMessages (nextMsg:remainingMsgs) reqSema rspSema =
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
    UnknownFromClientMessage m -> liftIO $ error $ "Unknown message was recorded from the client" ++ show m
 where
  -- TODO: May need to prevent premature exit notification being sent
  notification msg@(NotificationMessage _ Exit _) = do
    liftIO $ putStrLn "Will send exit notification soon"
    liftIO $ threadDelay 10000000
    sendNotification' msg

    liftIO $ error "Done"

  notification msg@(NotificationMessage _ m _) = do
    sendNotification' msg

    liftIO $ putStrLn $ "Sent a notification " ++ show m

    sendMessages remainingMsgs reqSema rspSema

  request msg@(RequestMessage _ id m _) = do
    sendRequest' msg
    liftIO $ putStrLn $  "Sent a request id " ++ show id ++ ": " ++ show m ++ "\nWaiting for a response"

    rsp <- liftIO $ takeMVar rspSema
    when (responseId id /= rsp) $
      error $ "Expected id " ++ show id ++ ", got " ++ show rsp

    sendMessages remainingMsgs reqSema rspSema

  response msg@(ResponseMessage _ id _ _) = do
    liftIO $ putStrLn $ "Waiting for request id " ++ show id ++ " from the server"
    reqId <- liftIO $ takeMVar reqSema
    if responseId reqId /= id
      then error $ "Expected id " ++ show reqId ++ ", got " ++ show reqId
      else do
        sendResponse' msg
        liftIO $ putStrLn $ "Sent response to request id " ++ show id

    sendMessages remainingMsgs reqSema rspSema


isNotification :: FromServerMessage -> Bool
isNotification (NotPublishDiagnostics      _) = True
isNotification (NotLogMessage              _) = True
isNotification (NotShowMessage             _) = True
isNotification (NotCancelRequestFromServer _) = True
isNotification _                              = False

listenServer :: [FromServerMessage] -> RequestMap -> MVar LspId -> MVar LspIdRsp -> MVar Bool -> Handle -> Session ()
listenServer [] _ _ _ passVar _ = liftIO $ putMVar passVar True
listenServer expectedMsgs reqMap reqSema rspSema passVar serverOut  = do
  msgBytes <- liftIO $ getNextMessage serverOut
  let msg = decodeFromServerMsg reqMap msgBytes

  case msg of
    ReqRegisterCapability       m -> request m
    ReqApplyWorkspaceEdit       m -> request m
    ReqShowMessage              m -> request m
    ReqUnregisterCapability     m -> request m
    RspInitialize               m -> response m
    RspShutdown                 m -> response m
    RspHover                    m -> response m
    RspCompletion               m -> response m
    RspCompletionItemResolve    m -> response m
    RspSignatureHelp            m -> response m
    RspDefinition               m -> response m
    RspFindReferences           m -> response m
    RspDocumentHighlights       m -> response m
    RspDocumentSymbols          m -> response m
    RspWorkspaceSymbols         m -> response m
    RspCodeAction               m -> response m
    RspCodeLens                 m -> response m
    RspCodeLensResolve          m -> response m
    RspDocumentFormatting       m -> response m
    RspDocumentRangeFormatting  m -> response m
    RspDocumentOnTypeFormatting m -> response m
    RspRename                   m -> response m
    RspExecuteCommand           m -> response m
    RspError                    m -> response m
    RspDocumentLink             m -> response m
    RspDocumentLinkResolve      m -> response m
    RspWillSaveWaitUntil        m -> response m
    NotPublishDiagnostics       m -> notification m
    NotLogMessage               m -> notification m
    NotShowMessage              m -> notification m
    NotTelemetry                m -> notification m
    NotCancelRequestFromServer  m -> notification m

  if shouldSkip msg
    then listenServer expectedMsgs reqMap reqSema rspSema passVar serverOut
    else if inRightOrder msg expectedMsgs
      then listenServer (delete msg expectedMsgs) reqMap reqSema rspSema passVar serverOut
      else liftIO $ do
        putStrLn "Out of order"
        putStrLn "Got:"
        print msg
        putStrLn "Expected one of:"
        mapM_ print $ takeWhile (not . isNotification) expectedMsgs
        print $ head $ dropWhile (not . isNotification) expectedMsgs
        putMVar passVar False

  where
  response :: Show a => ResponseMessage a -> Session ()
  response res = do
    liftIO $ putStrLn $ "Got response for id " ++ show (res ^. id)

    liftIO $ putMVar rspSema (res ^. id) -- unblock the handler waiting to send a request

  request :: (Show a, Show b) => RequestMessage ServerMethod a b -> Session ()
  request req = do
    liftIO
      $  putStrLn
      $  "Got request for id "
      ++ show (req ^. id)
      ++ " "
      ++ show (req ^. method)

    liftIO $ putMVar reqSema (req ^. id) -- unblock the handler waiting for a response

  notification :: Show a => NotificationMessage ServerMethod a -> Session ()
  notification n = liftIO $ putStrLn $ "Got notification " ++ show (n ^. method)



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

inRightOrder received (expected : msgs)
  | received == expected    = True
  | isNotification expected = inRightOrder received msgs
  | otherwise               = False

-- | Ignore logging notifications since they vary from session to session
shouldSkip :: FromServerMessage -> Bool
shouldSkip (NotLogMessage  _) = True
shouldSkip (NotShowMessage _) = True
shouldSkip (ReqShowMessage _) = True
shouldSkip _                  = False
