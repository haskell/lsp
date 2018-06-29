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
  , runSessionWithHandles
  , runSessionWithConfig
  , Session
  , SessionConfig(..)
  , MonadSessionConfig(..)
  , SessionException(..)
  , anySessionException
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
  , initializeResponse
  , openDoc
  , documentContents
  , getDocumentEdit
  , getDocUri
  , noDiagnostics
  , getDocumentSymbols
  , getDiagnostics
  ) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Lens hiding ((.=), List)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Language.Haskell.LSP.Types hiding (id, capabilities)
import qualified Language.Haskell.LSP.Types as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import Language.Haskell.LSP.Test.Parsing
import Language.Haskell.LSP.Test.Session
import Language.Haskell.LSP.Test.Server
import System.Console.ANSI
import System.IO
import System.Directory
import System.FilePath
import qualified Yi.Rope as Rope

-- | Starts a new session.
runSession :: String -- ^ The command to run the server.
           -> FilePath -- ^ The filepath to the root directory for the session.
           -> Session a -- ^ The session to run.
           -> IO a
runSession = runSessionWithConfig def

-- | Starts a new sesion with a client with the specified capabilities.
runSessionWithConfig :: SessionConfig -- ^ The capabilities the client should have.
                     -> String -- ^ The command to run the server.
                     -> FilePath -- ^ The filepath to the root directory for the session.
                     -> Session a -- ^ The session to run.
                     -> IO a
runSessionWithConfig config serverExe rootDir session = do
  pid <- getCurrentProcessID
  absRootDir <- canonicalizePath rootDir

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          (capabilities config)
                                          (Just TraceOff)
  withServer serverExe (logStdErr config) $ \serverIn serverOut _ ->
    runSessionWithHandles serverIn serverOut listenServer config rootDir $ do

      -- Wrap the session around initialize and shutdown calls
      sendRequest Initialize initializeParams
      initRspMsg <- response :: Session InitializeResponse

      liftIO $ maybe (return ()) (putStrLn . ("Error while initializing: " ++) . show ) (initRspMsg ^. LSP.error)

      initRspVar <- initRsp <$> ask
      liftIO $ putMVar initRspVar initRspMsg

      sendNotification Initialized InitializedParams

      -- Run the actual test
      result <- session

      sendNotification Exit ExitParams

      return result

-- | Listens to the server output, makes sure it matches the record and
-- signals any semaphores
listenServer :: Handle -> Session ()
listenServer serverOut = do
  msgBytes <- liftIO $ getNextMessage serverOut

  context <- ask
  reqMap <- liftIO $ readMVar $ requestMap context

  let msg = decodeFromServerMsg reqMap msgBytes
  liftIO $ writeChan (messageChan context) msg

  listenServer serverOut

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session T.Text
documentContents doc = do
  vfs <- vfs <$> get
  let file = vfs Map.! (doc ^. uri)
  return $ Rope.toText $ Language.Haskell.LSP.VFS._text file

-- | Parses an ApplyEditRequest, checks that it is for the passed document
-- and returns the new content
getDocumentEdit :: TextDocumentIdentifier -> Session T.Text
getDocumentEdit doc = do
  req <- request :: Session ApplyWorkspaceEditRequest

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $ throw (IncorrectApplyEditRequestException (show req))

  documentContents doc
  where
    checkDocumentChanges :: ApplyWorkspaceEditRequest -> Bool
    checkDocumentChanges req =
      let changes = req ^. params . edit . documentChanges
          maybeDocs = fmap (fmap (^. textDocument . uri)) changes
      in case maybeDocs of
        Just docs -> (doc ^. uri) `elem` docs
        Nothing -> False
    checkChanges :: ApplyWorkspaceEditRequest -> Bool
    checkChanges req =
      let mMap = req ^. params . edit . changes
        in maybe False (HashMap.member (doc ^. uri)) mMap

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

-- | Open a virtual file if we send a did open text document notification
sendNotification TextDocumentDidOpen params = do
  let params' = fromJust $ decode $ encode params
      n :: DidOpenTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidOpen params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ openVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

-- | Close a virtual file if we send a close text document notification
sendNotification TextDocumentDidClose params = do
  let params' = fromJust $ decode $ encode params
      n :: DidCloseTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidClose params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ closeVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

sendNotification method params = sendNotification' (NotificationMessage "2.0" method params)

sendNotification' :: (ToJSON a, ToJSON b) => NotificationMessage a b -> Session ()
sendNotification' = sendMessage

sendResponse :: ToJSON a => ResponseMessage a -> Session ()
sendResponse = sendMessage

sendMessage :: ToJSON a => a -> Session ()
sendMessage msg = do
  h <- serverIn <$> ask
  let encoded = encode msg
  liftIO $ do

    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn $ "--> " ++ B.unpack encoded
    setSGR [Reset]

    B.hPut h (addHeader encoded)



-- | Returns the initialize response that was received from the server.
-- The initialize requests and responses are not included the session,
-- so if you need to test it use this.
initializeResponse :: Session InitializeResponse
initializeResponse = initRsp <$> ask >>= (liftIO . readMVar)

-- | Opens a text document and sends a notification to the client.
openDoc :: FilePath -> String -> Session TextDocumentIdentifier
openDoc file languageId = do
  item <- getDocItem file languageId
  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams item)
  TextDocumentIdentifier <$> getDocUri file
  where
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

getDiagnostics :: Session [Diagnostic]
getDiagnostics = do
  diagsNot <- notification :: Session PublishDiagnosticsNotification
  let (List diags) = diagsNot ^. params . LSP.diagnostics
  return diags

-- | Expects a 'PublishDiagnosticsNotification' and throws an
-- 'UnexpectedDiagnosticsException' if there are any diagnostics
-- returned.
noDiagnostics :: Session ()
noDiagnostics = do
  diagsNot <- notification :: Session PublishDiagnosticsNotification
  when (diagsNot ^. params . LSP.diagnostics /= List []) $ liftIO $ throw UnexpectedDiagnosticsException

-- | Returns the symbols in a document.
getDocumentSymbols :: TextDocumentIdentifier -> Session DocumentSymbolsResponse
getDocumentSymbols doc = do
  sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
  response