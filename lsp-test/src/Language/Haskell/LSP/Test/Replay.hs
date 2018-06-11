-- | A testing tool for replaying captured client logs back to a server,
-- and validating that the server output matches up with another log.
module Language.Haskell.LSP.Test.Replay
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
import           Language.Haskell.LSP.Test.Decoding
import           Language.Haskell.LSP.Test.Messages


-- | Replays a captured client output and 
-- makes sure it matches up with an expected response.
-- The session directory should have a captured session file in it
-- named "session.log".
replaySession :: String -- ^ The command to run the server.
              -> FilePath -- ^ The recorded session directory.
              -> IO Bool
replaySession serverExe sessionDir = do

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

  threadId <- forkIO $
    runSessionWithHandler (listenServer serverMsgs requestMap reqSema rspSema passVar)
                          serverExe
                          sessionDir
                          (sendMessages clientMsgs reqSema rspSema)

  result <- takeMVar passVar
  killThread threadId
  return result

  where
    isClientMsg (FromClient _ _) = True
    isClientMsg _                = False

    isServerMsg (FromServer _ _) = True
    isServerMsg _                = False

sendMessages :: [FromClientMessage] -> MVar LspId -> MVar LspIdRsp -> Session ()
sendMessages [] _ _ = return ()
sendMessages (nextMsg:remainingMsgs) reqSema rspSema =
  handleClientMessage request response notification nextMsg
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
        sendResponse msg
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
  
  handleServerMessage request response notification msg

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
        print $ head $ dropWhile isNotification expectedMsgs
        putMVar passVar False

  where
  response :: ResponseMessage a -> Session ()
  response res = do
    liftIO $ putStrLn $ "Got response for id " ++ show (res ^. id)

    liftIO $ putMVar rspSema (res ^. id) -- unblock the handler waiting to send a request

  request :: RequestMessage ServerMethod a b -> Session ()
  request req = do
    liftIO
      $  putStrLn
      $  "Got request for id "
      ++ show (req ^. id)
      ++ " "
      ++ show (req ^. method)

    liftIO $ putMVar reqSema (req ^. id) -- unblock the handler waiting for a response

  notification :: NotificationMessage ServerMethod a -> Session ()
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
