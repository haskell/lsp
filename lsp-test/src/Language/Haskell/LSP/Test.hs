{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test
  (
  -- * Sessions
    manualSession
  -- * Documents
  , openDocument
  , documentSymbols
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Proxy
import System.Process
import qualified Language.Haskell.LSP.Client as Client
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import Language.Haskell.LSP.Test.Recorded
import Capabilities
import Compat

type Session = ReaderT Client.Client IO

manualSession :: Session a -> IO ()
manualSession f = do
  (Just hin, Just hout, _, serverProc) <- createProcess (proc "hie" ["--lsp", "-l", "/tmp/hie.log"])
    { std_in = CreatePipe, std_out = CreatePipe }
  client <- Client.start $ Client.Config hin hout notificationHandler requestHandler

  pid <- getProcessID

  let initializeParams :: LSP.InitializeParams
      initializeParams = LSP.InitializeParams (Just pid)
                                              Nothing
                                              Nothing
                                              Nothing
                                              capabilities
                                              Nothing

  Client.sendClientRequest client
                           (Proxy :: Proxy LSP.InitializeRequest)
                           LSP.Initialize initializeParams
  Client.sendClientNotification client
                                LSP.Initialized
                                (Just LSP.InitializedParams)

  putStrLn "Session started"

  runReaderT f client

  Client.sendClientRequest client
                           (Proxy :: Proxy LSP.ShutdownRequest)
                           LSP.Shutdown Nothing
  Client.sendClientNotification client
                                LSP.Exit
                                (Just LSP.ExitParams)

  Client.stop client

  -- todo: this interrupts the test server process as well?
  -- interruptProcessGroupOf serverProc
  -- waitForProcess serverProc
  terminateProcess serverProc

  putStrLn "Session ended"

openDocument :: FilePath -> Session ()
openDocument path = do
  text <- liftIO $ T.readFile path

  let uri = LSP.filePathToUri path

  client <- ask
  liftIO $ Client.sendClientNotification client LSP.TextDocumentDidOpen (Just (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem uri "haskell" 1 text)))

documentSymbols :: FilePath -> Session (LSP.List LSP.SymbolInformation)
documentSymbols path = do
  let uri = LSP.filePathToUri path

  client <- ask

  liftIO $ do
    res <- Client.sendClientRequest client
                                    (Proxy :: Proxy LSP.DocumentSymbolRequest)
                                    LSP.TextDocumentDocumentSymbol (LSP.DocumentSymbolParams (LSP.TextDocumentIdentifier uri))
    return $ case res of
      Just (Right syms) -> syms
      _ -> error "Failed to get document symbols"

notificationHandler :: Client.NotificationMessageHandler
notificationHandler = Client.NotificationMessageHandler
  (\(LSP.NotificationMessage _ _ (LSP.ShowMessageParams _ msg)) -> print msg)
  (\(LSP.NotificationMessage _ _ (LSP.LogMessageParams _ msg)) -> print msg)
  (\(LSP.NotificationMessage _ _ json) -> putStrLn $ "Telemetry: " ++ show json)
  (\(LSP.NotificationMessage _ _ (LSP.PublishDiagnosticsParams uri diags)) ->
    putStrLn $ "Diagnostics at " ++ showUri uri ++ ": " ++ showDiags diags)

  where showDiags :: LSP.List LSP.Diagnostic -> String
        showDiags (LSP.List diags) = unlines $ map (T.unpack . (^. LSP.message)) diags
        showUri :: LSP.Uri -> String
        showUri = fromMaybe "unknown path" . LSP.uriToFilePath



requestHandler :: Client.RequestMessageHandler
requestHandler = Client.RequestMessageHandler
  (\m -> emptyRsp m <$ print m)
  (\m -> emptyRsp m <$ print m)
  (\m -> emptyRsp m <$ print m)
  (\m -> emptyRsp m <$ print m)
  where emptyRsp :: LSP.RequestMessage m req rsp -> LSP.ResponseMessage a
        emptyRsp m = LSP.ResponseMessage (m ^. LSP.jsonrpc)
                                         (lspIdToRspId $ m ^. LSP.id)
                                         Nothing
                                         Nothing

        lspIdToRspId (LSP.IdInt i) = LSP.IdRspInt i
        lspIdToRspId (LSP.IdString i) = LSP.IdRspString i
