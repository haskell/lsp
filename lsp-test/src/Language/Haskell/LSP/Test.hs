{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Language.Haskell.LSP.Test
-- Description : A functional testing framework for LSP servers.
-- Maintainer  : luke_lau@icloud.com
-- Stability   : experimental
--
-- A framework for testing <https://github.com/Microsoft/language-server-protocol Language Server Protocol servers> at the JSON level.

module Language.Haskell.LSP.Test
  (
  -- * Sessions
    runSession
  , runSessionWithHandler
  , Session
  -- * Sending
  , sendRequest
  , sendNotification
  , sendRequest'
  , sendNotification'
  , sendResponse
  -- * Receving
  , anyRequest
  , request
  , anyResponse
  , response
  , anyNotification
  , notification
  , loggingNotification
  , publishDiagnosticsNotification
  -- * Combinators
  , choice
  , option
  , optional
  , between
  , some
  , many
  , sepBy
  , sepBy1
  , sepEndBy1
  , sepEndBy
  , endBy1
  , endBy
  , count
  , manyTill
  , skipMany
  , skipSome
  , skipManyTill
  , skipSomeTill
  , (<|>)
  , satisfy
  -- * Utilities
  , openDoc
  , getDocItem
  , getDocUri
  ) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Lens hiding ((.=))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import System.Process
import Language.Haskell.LSP.Types
import qualified  Language.Haskell.LSP.Types as LSP (error, id)
import Language.Haskell.LSP.Test.Compat
import System.IO
import System.Directory
import System.FilePath
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Parsing

-- | Starts a new session.
runSession :: String -- ^ The command to run the server.
           -> FilePath -- ^ The filepath to the root directory for the session.
           -> Session a -- ^ The session to run.
           -> IO ()
runSession serverExe rootDir session = do
  pid <- getProcessID
  absRootDir <- canonicalizePath rootDir

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          def
                                          (Just TraceOff)

  runSessionWithHandler listenServer serverExe rootDir $ do

    -- Wrap the session around initialize and shutdown calls
    sendRequest Initialize initializeParams
    initRsp <- response :: Session InitializeResponse
    liftIO $ maybe (return ()) (putStrLn . ("Error while initializing: " ++) . show ) (initRsp ^. LSP.error)

    sendNotification Initialized InitializedParams

    -- Run the actual test
    session

    sendNotification Exit ExitParams

-- | An internal version of 'runSession' that allows for a custom handler to listen to the server.
-- It also does not automatically send initialize and exit messages.
runSessionWithHandler :: (Handle -> Session ())
                      -> String
                      -> FilePath
                      -> Session a
                      -> IO a
runSessionWithHandler serverHandler serverExe rootDir session = do
  absRootDir <- canonicalizePath rootDir

  let createProc = (shell serverExe) { std_in = CreatePipe, std_out = CreatePipe }
  (Just serverIn, Just serverOut, Nothing, serverProc) <- createProcess createProc

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  reqMap <- newMVar newRequestMap
  messageChan <- newChan
  meaninglessChan <- newChan

  let context = SessionContext serverIn absRootDir messageChan reqMap
      initState = SessionState (IdInt 9)

  threadId <- forkIO $ void $ runSession' meaninglessChan context initState (serverHandler serverOut)
  (result, _) <- runSession' messageChan context initState session

  terminateProcess serverProc
  killThread threadId

  return result

-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: Handle -> Session ()
listenServer serverOut = do
  msgBytes <- liftIO $ getNextMessage serverOut

  context <- ask
  reqMap <- liftIO $ readMVar $ requestMap context

  liftIO $ writeChan (messageChan context) $ decodeFromServerMsg reqMap msgBytes

  listenServer serverOut

-- | Sends a request to the server.
--
-- @
-- sendRequest (Proxy :: Proxy DocumentSymbolRequest)
--             TextDocumentDocumentSymbol
--             (DocumentSymbolParams docId)
-- @
sendRequest
  :: (ToJSON params)
  => --Proxy (RequestMessage ClientMethod params resp) -- ^ A proxy to provide more type information about the request.
  ClientMethod -- ^ The request method.
  -> params -- ^ The request parameters.
  -> Session LspId -- ^ The id of the request that was sent.
sendRequest method params = do
  id <- curReqId <$> get
  modify $ \c -> c { curReqId = nextId id }

  let req = RequestMessage' "2.0" id method params

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r id method

  sendMessage req

  return id

  where nextId (IdInt i) = IdInt (i + 1)
        nextId (IdString s) = IdString $ T.pack $ show $ read (T.unpack s) + 1

-- | A custom type for request message that doesn't
-- need a response type, allows us to infer the request
-- message type without using proxies.
data RequestMessage' a = RequestMessage' T.Text LspId ClientMethod a

instance ToJSON a => ToJSON (RequestMessage' a) where
  toJSON (RequestMessage' rpc id method params) =
    object ["jsonrpc" .= rpc, "id" .= id, "method" .= method, "params" .= params]


sendRequest' :: (ToJSON a, ToJSON b) => RequestMessage ClientMethod a b -> Session ()
sendRequest' req = do
  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r (req ^. LSP.id) (req ^. method)

  sendMessage req

-- | Sends a notification to the server.
sendNotification :: ToJSON a
                 => ClientMethod -- ^ The notification method.
                 -> a -- ^ The notification parameters.
                 -> Session ()
sendNotification method params =
  let notif = NotificationMessage "2.0" method params
    in sendNotification' notif

sendNotification' :: (ToJSON a, ToJSON b) => NotificationMessage a b -> Session ()
sendNotification' = sendMessage

sendResponse :: ToJSON a => ResponseMessage a -> Session ()
sendResponse = sendMessage

sendMessage :: ToJSON a => a -> Session ()
sendMessage msg = do
  h <- serverIn <$> ask
  liftIO $ B.hPut h $ addHeader (encode msg)

-- | Opens a text document and sends a notification to the client.
openDoc :: FilePath -> String -> Session TextDocumentIdentifier
openDoc file languageId = do
  item <- getDocItem file languageId
  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams item)
  TextDocumentIdentifier <$> getDocUri file

-- | Reads in a text document as the first version.
getDocItem :: FilePath -- ^ The path to the text document to read in.
           -> String -- ^ The language ID, e.g "haskell" for .hs files.
           -> Session TextDocumentItem
getDocItem file languageId = do
  context <- ask
  let fp = rootDir context </> file
  contents <- liftIO $ T.readFile fp
  return $ TextDocumentItem (filePathToUri fp) (T.pack languageId) 0 contents

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
  context <- ask
  let fp = rootDir context </> file
  return $ filePathToUri fp
