{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Recorded
  ( replay
  )
where

import           Control.Concurrent
import           Data.Default
import           Language.Haskell.LSP.Control  as Control
import qualified Data.ByteString.Lazy.Char8    as B
import           Language.Haskell.LSP.Core
import qualified Language.Haskell.LSP.Types    as LSP
import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           System.IO
import           System.Directory
import           System.Process

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
    (proc "hie" ["--lsp", "-l", "/tmp/hie.log", "-d"]) { std_in  = CreatePipe
                                                       , std_out = CreatePipe
                                                       }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  -- todo: use qsem
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


  expectedMsgs <- getAllMessages serverRecIn

  -- listen to server
  forkIO $ listenServer expectedMsgs serverOut semas didPass

  -- start client replay
  forkIO $ do
    Control.runWithHandles clientRecIn
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

-- todo: Maybe make a reader monad and a fail function for it?
listenServer
  :: [B.ByteString]
  -> Handle
  -> (MVar LSP.LspIdRsp, MVar LSP.LspId)
  -> MVar Bool
  -> IO ()
listenServer [] _ _ passVar = putMVar passVar True
listenServer expectedMsgs h semas@(reqSema, rspSema) passVar = do
  msg <- getNextMessage h
  putStrLn $ "Remaining messages " ++ show (length expectedMsgs)
  if inRightOrder msg expectedMsgs
    then do

      whenResponse msg $ \res -> do
        putStrLn $ "Got response for id " ++ show (res ^. LSP.id)
        putMVar reqSema (res ^. LSP.id) -- unblock the handler waiting to send a request

      whenRequest msg $ \req -> do
        putStrLn $ "Got request for id " ++ show (req ^. LSP.id) ++ " " ++ show (req ^. LSP.method)
        putMVar rspSema (req ^. LSP.id) -- unblock the handler waiting for a response

      whenNotification msg $ \n -> putStrLn $ "Got notification " ++ (show (n ^. LSP.method))

      when (not (msg `elem` expectedMsgs)) $ do
        putStrLn "Got an unexpected message"
        putMVar passVar False

      listenServer (delete msg expectedMsgs) h semas passVar
    else do
      putStrLn $ "Got: " ++ show msg ++ "\n Expected: " ++ show
        (head (filter (not . isNotification) expectedMsgs))
      putMVar passVar False

isNotification :: B.ByteString -> Bool
isNotification msg =
  isJust (decode msg :: Maybe (LSP.NotificationMessage Value Value))

whenResponse :: B.ByteString -> (LSP.ResponseMessage Value -> IO ()) -> IO ()
whenResponse msg f = case decode msg :: Maybe (LSP.ResponseMessage Value) of
  Just msg' -> when (isJust (msg' ^. LSP.result)) (f msg')
  _         -> return ()

whenRequest
  :: B.ByteString -> (LSP.RequestMessage Value Value Value -> IO ()) -> IO ()
whenRequest msg =
  forM_ (decode msg :: (Maybe (LSP.RequestMessage Value Value Value)))

whenNotification :: B.ByteString -> (LSP.NotificationMessage Value Value -> IO ()) -> IO ()
whenNotification msg = forM_ (decode msg :: (Maybe (LSP.NotificationMessage Value Value)))

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
inRightOrder :: B.ByteString -> [B.ByteString] -> Bool
inRightOrder _        []   = error "why is this empty"
inRightOrder received msgs = received `elem` valid
 where
  valid   = takeWhile canSkip msgs ++ firstNonSkip
  -- we don't care about the order of notifications
  canSkip = isNotification
  nonSkip = dropWhile canSkip msgs
  firstNonSkip | null nonSkip = []
               | otherwise    = [head nonSkip]

getAllMessages :: Handle -> IO [B.ByteString]
getAllMessages h = do
  done <- hIsEOF h
  if done
    then return []
    else do
      msg <- getNextMessage h
      (msg :) <$> getAllMessages h

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing   -> error "Couldn't read Content-Length header"
    Just size -> B.hGet h size


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
  , initializeRequestHandler             = Just request
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
  notification msg @(LSP.NotificationMessage _ m _) = do
    B.hPut serverH $ addHeader (encode msg)
    
    putStrLn $ "Sent a notification " ++ show m

  request msg@(LSP.RequestMessage _ id m _) = do
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

addHeader :: B.ByteString -> B.ByteString
addHeader content = B.concat
  [ "Content-Length: "
  , B.pack $ show $ B.length content
  , "\r\n"
  , "\r\n"
  , content
  ]

getHeaders :: Handle -> IO [(String, String)]
getHeaders h = do
  l <- hGetLine h
  let (name, val) = span (/= ':') l
  if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h
