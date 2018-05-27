{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Language.Haskell.LSP.Control  as Control
import qualified Data.ByteString.Lazy.Char8    as B
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
  :: FilePath -- ^ The client output to replay to the server.
  -> FilePath -- ^ The expected response from the server.
  -> IO Bool
replay cfp sfp = do

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

  -- the recorded client input to the server
  clientRecIn  <- openFile cfp ReadMode
  serverRecIn  <- openFile sfp ReadMode
  null         <- openFile "/dev/null" WriteMode


  (clientMsgs, fileMap) <- swapFiles emptyFileMap clientRecIn

  tmpDir <- getTemporaryDirectory
  (_, mappedClientRecIn) <- openTempFile tmpDir "clientRecInMapped"
  mapM_ (B.hPut mappedClientRecIn . addHeader) clientMsgs
  hSeek mappedClientRecIn AbsoluteSeek 0

  (expectedMsgs, _) <- swapFiles fileMap serverRecIn

  -- listen to server
  forkIO $ runReaderT (listenServer expectedMsgs serverOut semas) didPass

  -- start client replay
  forkIO $ do
    Control.runWithHandles mappedClientRecIn
                           null
                           (const $ Right (), const $ return Nothing)
                           (handlers serverIn semas)
                           def
                           Nothing
                           Nothing

    -- todo: we shouldn't do this, we should check all notifications were delivered first
    putMVar didPass True

  result <- takeMVar didPass
  terminateProcess serverProc

  -- restore directory
  setCurrentDirectory prevDir

  return result

-- | The internal monad for tests that can fail or pass,
-- ending execution early.
type Session = ReaderT (MVar Bool) IO

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
listenServer :: [B.ByteString] -> Handle -> (MVar LSP.LspIdRsp, MVar LSP.LspId) -> Session ()
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


  where jsonEqual :: (FromJSON a, Eq a) => a -> B.ByteString -> Bool
        jsonEqual x y = Just x == decode y

        deleteFirstJson _ [] = []
        deleteFirstJson msg (x:xs)
          | jsonEqual msg x = xs
          | otherwise = x:deleteFirstJson msg xs

        -- firstExpected :: Show a => a
        firstExpected = head $ filter (not . isNotification) expectedMsgs

        response :: LSP.ResponseMessage Value -> Session [B.ByteString]
        response res = do
          lift $ putStrLn $ "Got response for id " ++ show (res ^. LSP.id)

          lift $ print res

          checkOrder res

          lift $ putMVar reqSema (res ^. LSP.id) -- unblock the handler waiting to send a request

          return $ deleteFirstJson res expectedMsgs

        request :: LSP.RequestMessage LSP.ServerMethod Value Value -> Session [B.ByteString]
        request req = do
          lift $ putStrLn $ "Got request for id " ++ show (req ^. LSP.id) ++ " " ++ show (req ^. LSP.method)

          lift $ print req

          checkOrder req

          lift $ putMVar rspSema (req ^. LSP.id) -- unblock the handler waiting for a response

          return $ deleteFirstJson req expectedMsgs

        notification :: LSP.NotificationMessage LSP.ServerMethod Value -> Session [B.ByteString]
        notification n = do
          lift $ putStrLn $ "Got notification " ++ show (n ^. LSP.method)
          lift $ print n
          return $ deleteFirstJson n expectedMsgs
        
        checkOrder msg = unless (inRightOrder msg expectedMsgs) $ do
          let expected = decode firstExpected
              _ = expected == Just msg -- make expected type same as res
          failSession ("Out of order\nExpected\n" ++ show expected ++ "\nGot\n" ++ show msg ++ "\n")


isNotification :: B.ByteString -> Bool
isNotification msg =
  isJust (decode msg :: Maybe (LSP.NotificationMessage Value Value))

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


handlers :: Handle -> (MVar LSP.LspIdRsp, MVar LSP.LspId) -> Handlers
handlers serverH (reqSema, rspSema) = def
  {
    -- Requests
    hoverHandler                             = Just request
  , completionHandler                        = Just request
  , completionResolveHandler                 = Just request
  , signatureHelpHandler                     = Just request
  , definitionHandler                        = Just request
  , referencesHandler                        = Just request
  , documentHighlightHandler                 = Just request
  , documentSymbolHandler                    = Just request
  , workspaceSymbolHandler                   = Just request
  , codeActionHandler                        = Just request
  , codeLensHandler                          = Just request
  , codeLensResolveHandler                   = Just request
  , documentFormattingHandler                = Just request
  , documentRangeFormattingHandler           = Just request
  , documentTypeFormattingHandler            = Just request
  , renameHandler                            = Just request
  , documentLinkHandler                      = Just request
  , documentLinkResolveHandler               = Just request
  , executeCommandHandler                    = Just request
  , initializeRequestHandler                 = Just request
    -- Notifications
  , didChangeConfigurationParamsHandler      = Just notification
  , didOpenTextDocumentNotificationHandler   = Just notification
  , didChangeTextDocumentNotificationHandler = Just notification
  , didCloseTextDocumentNotificationHandler  = Just notification
  , didSaveTextDocumentNotificationHandler   = Just notification
  , didChangeWatchedFilesNotificationHandler = Just notification
  , initializedHandler                       = Just notification
  , willSaveTextDocumentNotificationHandler  = Just notification
  , cancelNotificationHandler                = Just notification
  , exitNotificationHandler                  = Just notification
    -- Responses
  , responseHandler                          = Just response
  }
 where

  -- TODO: May need to prevent premature exit notification being sent
  -- notification msg@(LSP.NotificationMessage _ LSP.Exit _) = do
  --   putStrLn "Will send exit notification soon"
  --   threadDelay 10000000
  --   B.hPut serverH $ addHeader (encode msg)
  notification msg@(LSP.NotificationMessage _ m _) = do
    B.hPut serverH $ addHeader (encode msg)

    putStrLn $ "Sent a notification " ++ show m

  request msg@(LSP.RequestMessage _ id m _) = do

    when (m == LSP.TextDocumentDocumentSymbol) $ threadDelay 5000000

    B.hPut serverH $ addHeader (encode msg)
    putStrLn $  "Sent a request id " ++ show id ++ ": " ++ show m ++ "\nWaiting for a response"

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
