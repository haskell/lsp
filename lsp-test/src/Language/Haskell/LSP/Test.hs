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
  , sendResponse'
  -- * Receving
  , getMessage
  -- * Utilities
  , getDocItem
  , getDocUri
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import Data.Maybe
import Data.Proxy
import System.Process
import Language.Haskell.LSP.Types hiding (error, id)
import Language.Haskell.LSP.Test.Compat
import System.IO
import System.Directory
import System.FilePath
import Language.Haskell.LSP.Test.Parsing

data SessionContext = SessionContext
  {
    messageSema :: MVar B.ByteString,
    serverIn :: Handle,
    rootDir :: FilePath
  }

newtype SessionState = SessionState
  {
    curReqId :: LspId
  }

-- | A session representing one instance of launching and connecting to a server.
-- 
-- You can send and receive messages to the server within 'Session' via 'getMessage',
-- 'sendRequest' and 'sendNotification'.
--
-- @
-- runSession \"path\/to\/root\/dir\" $ do
--   docItem <- getDocItem "Desktop/simple.hs" "haskell"
--   sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams docItem)
--   diagnostics <- getMessage :: Session PublishDiagnosticsNotification
-- @
-- 
type Session = StateT SessionState (ReaderT SessionContext IO)

-- | Starts a new session.
runSession :: FilePath -- ^ The filepath to the root directory for the session.
           -> Session a -- ^ The session to run.
           -> IO ()
runSession rootDir session = do
  pid <- getProcessID
  absRootDir <- canonicalizePath rootDir

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          def
                                          (Just TraceOff)

  runSessionWithHandler listenServer rootDir $ do

    -- Wrap the session around initialize and shutdown calls
    sendRequest (Proxy :: Proxy InitializeRequest) Initialize initializeParams
    (ResponseMessage _ _ (Just (InitializeResponseCapabilities _)) e) <- getMessage
    liftIO $ maybe (return ()) (putStrLn . ("Error when initializing: " ++) . show ) e

    sendNotification Initialized InitializedParams

    -- Run the actual test
    session

    sendNotification Exit ExitParams

runSessionWithHandler :: (Handle -> Session ())
                      -> FilePath
                      -> Session a
                      -> IO a
runSessionWithHandler serverHandler rootDir session = do
  absRootDir <- canonicalizePath rootDir

  (Just serverIn, Just serverOut, Nothing, serverProc) <- createProcess
    (proc "hie" ["--lsp", "-d", "-l", "/tmp/hie-test.log"])
    { std_in = CreatePipe, std_out = CreatePipe }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  messageSema <- newEmptyMVar

  let context = SessionContext messageSema serverIn absRootDir
      initState = SessionState (IdInt 9)

  forkIO $ void $ runReaderT (runStateT (serverHandler serverOut) initState) context
  (result, _) <- runReaderT (runStateT session initState) context

  terminateProcess serverProc

  return result

-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: Handle -> Session ()
listenServer serverOut = do
  context <- lift ask
  msgBytes <- liftIO $ getNextMessage serverOut

  liftIO $ case decode msgBytes :: Maybe LogMessageNotification of
    -- Just print log and show messages
    Just (NotificationMessage _ WindowLogMessage (LogMessageParams _ msg)) -> T.putStrLn msg
    _ -> case decode msgBytes :: Maybe ShowMessageNotification of
      Just (NotificationMessage _ WindowShowMessage (ShowMessageParams _ msg)) -> T.putStrLn msg
    -- Give everything else for getMessage to handle
      _ -> putMVar (messageSema context) msgBytes

  listenServer serverOut

-- | Sends a request to the server.
--
-- @
-- sendRequest (Proxy :: Proxy DocumentSymbolRequest)
--             TextDocumentDocumentSymbol
--             (DocumentSymbolParams docId)
-- @
sendRequest
  :: forall params resp. (ToJSON params, ToJSON resp, FromJSON resp)
  => Proxy (RequestMessage ClientMethod params resp) -- ^ A proxy to provide more type information about the request.
  -> ClientMethod -- ^ The request method.
  -> params -- ^ The request parameters.
  -> Session LspId -- ^ The id of the request that was sent.
sendRequest _ method params = do
  id <- curReqId <$> get
  get >>= \c -> put c { curReqId = nextId id }

  let req = RequestMessage "2.0" id method params :: RequestMessage ClientMethod params resp

  sendRequest' req

  return id

  where nextId (IdInt i) = IdInt (i + 1)
        nextId (IdString s) = IdString $ T.pack $ show $ read (T.unpack s) + 1

sendRequest' :: (ToJSON a, ToJSON b, ToJSON c) => RequestMessage a b c -> Session ()
sendRequest' = sendMessage

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

sendResponse' :: ToJSON a => ResponseMessage a -> Session ()
sendResponse' = sendMessage

sendMessage :: ToJSON a => a -> Session ()
sendMessage msg = do
  h <- serverIn <$> lift ask
  liftIO $ B.hPut h $ addHeader (encode msg)

-- | Reads in a message from the server.
getMessage :: FromJSON a => Session a
getMessage = do
  sema <- messageSema <$> lift ask
  bytes <- liftIO $ takeMVar sema
  return $ fromMaybe (error $ "Wrong type! Got: " ++ show bytes) (decode bytes)

-- | Reads in a text document as the first version.
getDocItem :: FilePath -- ^ The path to the text document to read in.
           -> String -- ^ The language ID, e.g "haskell" for .hs files.
           -> Session TextDocumentItem
getDocItem file languageId = do
  context <- lift ask
  let fp = rootDir context </> file
  contents <- liftIO $ T.readFile fp
  return $ TextDocumentItem (filePathToUri fp) (T.pack languageId) 0 contents

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
  context <- lift ask
  let fp = rootDir context </> file
  return $ filePathToUri fp