{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Haskell.LSP.Test
  (
  -- * Sessions
    runSession
  , Session
  -- * Sending
  , sendRequest
  , sendNotification
  -- * Receving
  , getMessage
  -- * Utilities
  , getDocItem
  , getDocUri
  ) where

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
import Compat
import System.IO
import System.Directory
import System.FilePath
import Language.Haskell.LSP.Test.Parsing

data SessionContext = SessionContext
  {
    messageSema :: MVar B.ByteString,
    serverIn :: Handle,
    serverOut :: Handle,
    rootDir :: FilePath
  }

newtype SessionState = SessionState
  {
    curReqId :: LspId
  }
type Session = StateT SessionState (ReaderT SessionContext IO)

runSession :: FilePath -> Session a -> IO ()
runSession rootDir session = do

  absRootDir <- canonicalizePath rootDir

  (Just serverIn, Just serverOut, Nothing, serverProc) <- createProcess
    (proc "hie" ["--lsp", "-d", "-l", "/tmp/hie-test.log"])
    { std_in = CreatePipe, std_out = CreatePipe }

  hSetBuffering serverIn  NoBuffering
  hSetBuffering serverOut NoBuffering

  pid <- getProcessID
  messageSema <- newEmptyMVar

  let initializeParams :: InitializeParams
      initializeParams = InitializeParams (Just pid)
                                              (Just $ T.pack absRootDir)
                                              (Just $ filePathToUri absRootDir)
                                              Nothing
                                              def
                                              (Just TraceOff)
      context = SessionContext messageSema serverIn serverOut absRootDir
      initState = SessionState (IdInt 9)

      -- | The session wrapped around initialize and shutdown calls
      fullSession = do
        sendRequest (Proxy :: Proxy InitializeRequest) Initialize initializeParams
        (ResponseMessage _ _ (Just (InitializeResponseCapabilities _)) e) <- getMessage
        liftIO $ maybe (return ()) (putStrLn . ("Error when initializing: " ++) . show ) e

        sendNotification Initialized InitializedParams

        -- Run the actual thing
        session

        sendNotification Exit ExitParams

  forkIO $ listenServer context
  _ <- runReaderT (runStateT fullSession initState) context

  terminateProcess serverProc

  return ()

-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: SessionContext -> IO ()
listenServer context = do
  msgBytes <- getNextMessage (serverOut context)

  case decode msgBytes :: Maybe LogMessageNotification of
    -- Just print log and show messages
    Just (NotificationMessage _ WindowLogMessage (LogMessageParams _ msg)) -> T.putStrLn msg
    _ -> case decode msgBytes :: Maybe ShowMessageNotification of
      Just (NotificationMessage _ WindowShowMessage (ShowMessageParams _ msg)) -> T.putStrLn msg
    -- Give everything else for getMessage to handle
      _ -> putMVar (messageSema context) msgBytes
    
  listenServer context

-- | Sends a request to the server.
sendRequest
  :: forall params resp. (ToJSON params, ToJSON resp, FromJSON resp)
  => Proxy (RequestMessage ClientMethod params resp)
  -> ClientMethod
  -> params
  -> Session LspId
sendRequest _ method params = do
  h <- serverIn <$> lift ask

  id <- curReqId <$> get
  get >>= \c -> put c { curReqId = nextId id }

  let msg = RequestMessage "2.0" id method params :: RequestMessage ClientMethod params resp

  liftIO $ B.hPut h $ addHeader (encode msg)

  return id

  where nextId (IdInt i) = IdInt (i + 1)
        nextId (IdString s) = IdString $ T.pack $ show $ read (T.unpack s) + 1

-- | Sends a notification to the server.
sendNotification :: ToJSON a => ClientMethod -> a -> Session ()
sendNotification method params = do
  h <- serverIn <$> lift ask

  let msg = NotificationMessage "2.0" method params
  liftIO $ B.hPut h $ addHeader (encode msg)

-- | Reads in a message from the server.
getMessage :: FromJSON a => Session a
getMessage = do
  sema <- messageSema <$> lift ask
  bytes <- liftIO $ takeMVar sema
  return $ fromMaybe (error $ "Wrong type! Got: " ++ show bytes) (decode bytes)

-- | Reads in a text document as the first version.
getDocItem :: FilePath
           -- ^ The path to the text document to read in.
           -> String
           -- ^ The language ID, e.g "haskell" for .hs files.
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