{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Recorded
  ( replay
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Default
import           Language.Haskell.LSP.Control  as Control
import qualified Data.ByteString.Lazy.Char8    as B
import           Language.Haskell.LSP.Core
import qualified Language.Haskell.LSP.Types    as LSP
import           Data.Aeson
import           System.IO
import           System.Process

-- | Replays a recorded client output and 
-- makes sure it matches up with an expected response.
replay :: FilePath -- ^ The client output to replay to the server.
       -> FilePath -- ^ The expected response from the server.
       -> IO Int
replay cfp sfp = do

  (Just serverIn, Just serverOut, _, _) <- createProcess
    (proc "hie" ["--lsp", "-l", "/tmp/hie.log", "-d"]) { std_in  = CreatePipe
                                                       , std_out = CreatePipe
                                                       }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  -- whether to send the next request
  semaphore <- newEmptyMVar

  -- the recorded client input to the server
  clientRecIn <- openFile cfp ReadMode
  serverRecIn <- openFile sfp ReadMode
  null        <- openFile "/dev/null" WriteMode

  -- listen to server
  forkIO $ forever $ do
    msg <- getNextMessage serverOut
    expectedMsg <- getNextMessage serverRecIn
    putStrLn $ "received: " ++ (show msg)
    putStrLn $ "next expected: " ++ (show expectedMsg)
    case decode msg :: Maybe (LSP.RequestMessage Value Value Value) of
      Just _ -> putStrLn "ignoring request" >> return ()
      Nothing -> when (msg /= expectedMsg) $ error ("Expected " ++ show expectedMsg ++ " but got " ++ show msg) 
    case decode msg :: Maybe (LSP.ResponseMessage Value) of
      Just _  -> putMVar semaphore ()
      Nothing -> return () -- might be a notification or something, that's ok

  -- send inialize request ourselves since haskell-lsp consumes it
  -- rest are handled via `handlers`
  sendInitialize clientRecIn serverIn

  Control.runWithHandles clientRecIn
                         null
                         (const $ Right (), const $ return Nothing)
                         (handlers serverIn semaphore)
                         def
                         Nothing
                         Nothing
 where
  sendInitialize recH serverH = do
    message <- getNextMessage recH
    B.hPut serverH (addHeader message)
    -- bring the file back to the start for haskell-lsp
    hSeek recH AbsoluteSeek 0

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing   -> error "Couldn't read Content-Length header"
    Just size -> B.hGet h size


handlers :: Handle -> MVar () -> Handlers
handlers serverH flag = def
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
  , responseHandler                          = Just notification
  }
 where
  notification m = do
    B.hPut serverH $ addHeader (encode m)
    putStrLn "sent a notification"
  request m = do
    B.hPut serverH $ addHeader (encode m)
    putStrLn "sent a request, waiting for a response"
    takeMVar flag
    putStrLn "got a response"

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
