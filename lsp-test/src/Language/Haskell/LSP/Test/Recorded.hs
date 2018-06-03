{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | A testing tool for replaying recorded client logs back to a server,
-- and validating that the server output matches up with another log.
module Language.Haskell.LSP.Test.Recorded
  ( replay
  )
where

import           Control.Concurrent
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.List
import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Core
import qualified Language.Haskell.LSP.Types    as LSP
import           Data.Aeson
import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           System.IO
import           System.Directory
import           System.Process
import           Language.Haskell.LSP.Test.Files
import           Language.Haskell.LSP.Test.Parsing

-- | Replays a recorded client output and 
-- makes sure it matches up with an expected response.
replay
  :: FilePath -- ^ The recorded session file.
  -> FilePath -- ^ The root directory of the project
  -> IO Bool
replay sessionFp curRootDir = do

  -- need to keep hold of current directory since haskell-lsp changes it
  prevDir <- getCurrentDirectory

  (Just serverIn, Just serverOut, _, serverProc) <- createProcess
    (proc "hie" ["--lsp", "-l", "/tmp/hie.log"]) { std_in  = CreatePipe , std_out = CreatePipe }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  -- whether to send the next request
  reqSema <- newEmptyMVar :: IO (MVar LSP.LspIdRsp)
  -- whether to send the next response
  rspSema <- newEmptyMVar :: IO (MVar LSP.LspId)
  let semas = (reqSema, rspSema)

  didPass      <- newEmptyMVar

  entries <- B.lines <$> B.readFile sessionFp

  -- decode session
  let unswappedEvents = map (fromJust . decode) entries
  
  events <- swapFiles curRootDir unswappedEvents

  let clientEvents = map (\(FromClient _ msg) -> msg) $ filter isClientMsg events
      serverEvents = map (\(FromServer _ msg) -> msg) $ filter isServerMsg events

  -- listen to server
  forkIO $ runReaderT (listenServer serverEvents serverOut semas) didPass

  forM_ clientEvents (processClient serverIn)

  print events

  result <- takeMVar didPass
  terminateProcess serverProc

  -- restore directory
  setCurrentDirectory prevDir

  return result
  where
    isClientMsg (FromClient _ _) = True
    isClientMsg _ = False

    isServerMsg (FromServer _ _) = True
    isServerMsg _ = False

processEvent :: Handle -> MVar LSP.LspId -> MVar LSP.LspIdRsp -> Event -> IO ()
processEvent serverH rspSema reqSema (FromClient _ msg) = processClient serverH rspSema reqSema msg
processEvent _ _ _ (FromServer _ msg) = processServer msg

processClient
  :: Handle -> MVar LSP.LspId -> MVar LSP.LspIdRsp -> FromClientMessage -> IO ()
processClient serverH rspSema reqSema msg = case msg of
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
 where
  -- TODO: May need to prevent premature exit notification being sent
  notification msg@(LSP.NotificationMessage _ LSP.Exit _) = do
    putStrLn "Will send exit notification soon"
    threadDelay 10000000
    B.hPut serverH $ addHeader (encode msg)
  notification msg@(LSP.NotificationMessage _ m _) = do
    B.hPut serverH $ addHeader (encode msg)

    putStrLn $ "Sent a notification " ++ show m

  request msg@(LSP.RequestMessage _ id m _) = do

    when (m == LSP.TextDocumentDocumentSymbol) $ threadDelay 5000000

    B.hPut serverH $ addHeader (encode msg)
    putStrLn
      $  "Sent a request id "
      ++ show id
      ++ ": "
      ++ show m
      ++ "\nWaiting for a response"

    rspId <- takeMVar reqSema
    when (LSP.responseId id /= rspId)
      $  error
      $  "Expected id "
      ++ show id
      ++ ", got "
      ++ show rspId

  response msg@(LSP.ResponseMessage _ id _ _) = do
    putStrLn $ "Waiting for request id " ++ show id ++ " from the server"
    reqId <- takeMVar rspSema
    if LSP.responseId reqId /= id
      then error $ "Expected id " ++ show reqId ++ ", got " ++ show reqId
      else do
        B.hPut serverH $ addHeader (encode msg)
        putStrLn $ "Sent response to request id " ++ show id

-- | The internal monad for tests that can fail or pass,
-- ending execution early.
type Session = ReaderT (MVar Bool) IO

-- TODO: Make return type polymoprhic more like error
failSession :: String -> Session ()
failSession reason = do
  lift $ putStrLn reason
  passVar <- ask
  lift $ putMVar passVar False

passSession :: Session ()
passSession = do
  passVar <- ask
  lift $ putMVar passVar True

-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: [FromServerMessage] -> Handle -> (MVar LSP.LspIdRsp, MVar LSP.LspId) -> Session ()
listenServer [] _ _ = passSession
listenServer expectedMsgs h semas@(reqSema, rspSema) = do
  msg <- lift $ getNextMessage h

  newExpectedMsgs <- case decode msg of
    Just m -> request m
    Nothing -> case decode msg of
      Just m -> notification m
      Nothing -> case decode msg of
        Just m -> response m
        Nothing -> failSession "Malformed message" >> return expectedMsgs

  listenServer newExpectedMsgs h semas

  where response :: LSP.ResponseMessage a -> Session [FromServerMessage]
        response res = do
          lift $ putStrLn $ "Got response for id " ++ show (res ^. LSP.id)

          lift $ print res

          checkOrder res

          lift $ putMVar reqSema (res ^. LSP.id) -- unblock the handler waiting to send a request

          markReceived res

        request :: LSP.RequestMessage LSP.ServerMethod a b -> Session [FromServerMessage]
        request req = do
          lift $ putStrLn $ "Got request for id " ++ show (req ^. LSP.id) ++ " " ++ show (req ^. LSP.method)

          lift $ print req

          checkOrder req

          lift $ putMVar rspSema (req ^. LSP.id) -- unblock the handler waiting for a response

          markReceived req

        notification :: LSP.NotificationMessage LSP.ServerMethod a -> Session [FromServerMessage]
        notification n = do
          lift $ putStrLn $ "Got notification " ++ show (n ^. LSP.method)
          lift $ print n

          lift $ putStrLn $ show (length (filter isNotification expectedMsgs) - 1) ++ " notifications remaining"

          if n ^. LSP.method == LSP.WindowLogMessage
            then return expectedMsgs
            else markReceived n

        checkOrder msg = unless (inRightOrder msg expectedMsgs) $ do
          let (Just expected) = decode firstExpected
              _ = expected == msg -- make expected type same as res
          failSession ("Out of order\nExpected\n" ++ show expected ++ "\nGot\n" ++ show msg ++ "\n")

        markReceived :: Eq a => a -> [FromServerMessage] -> Session [FromServerMessage]
        markReceived msg = 
          -- TODO: Find some way of equating FromServerMessage and LSP.ResponseMessage etc.
          let new = delete msg expectedMsgs
           in if new == expectedMsgs
              then failSession ("Unexpected message: " ++ show msg) >> return new
              else return new

        firstExpected = head $ filter (not . isNotification) expectedMsgs

isNotification :: FromServerMessage -> Bool
isNotification (NotPublishDiagnostics _) = True
isNotification (NotLogMessage _) = True
isNotification (NotShowMessage _) = True
isNotification (NotCancelRequestFromServer _) = True
isNotification _ = False

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
inRightOrder :: (FromJSON a, Eq a) => a -> [B.ByteString] -> Bool

inRightOrder _ [] = error "Why is this empty"
-- inRightOrder (LSP.NotificationMessage _ _ _) _ = True

inRightOrder received (expected:msgs)
  | Just received == decode expected = True
  | isNotification expected = inRightOrder received msgs
  | otherwise =  False