{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import qualified Language.Haskell.LSP.TH.ClientCapabilities as LSP
import qualified LSP.Client as Client
import Data.Proxy
import qualified Data.Text.IO as T
import Control.Concurrent
import System.Process
import Control.Lens
import System.IO
import System.Exit
import System.Environment
import System.Directory
import Control.Monad

import qualified Compat

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  when (length args /= 1) $ do
    hPutStrLn stderr ("This program expects one argument: " ++ progName ++ " FILEPATH")
    exitFailure

  let [path] = args

  exists <- doesFileExist path
  unless exists $ do
    hPutStrLn stderr ("File does not exist: " ++ path)
    exitFailure

  file <- canonicalizePath path

  pid <- Compat.getPID

  let caps = LSP.ClientCapabilities (Just workspaceCaps) (Just textDocumentCaps) Nothing
      workspaceCaps = LSP.WorkspaceClientCapabilities
        (Just False)
        (Just (LSP.WorkspaceEditClientCapabilities (Just False)))
        (Just (LSP.DidChangeConfigurationClientCapabilities (Just False)))
        (Just (LSP.DidChangeWatchedFilesClientCapabilities (Just False)))
        (Just (LSP.SymbolClientCapabilities (Just False)))
        (Just (LSP.ExecuteClientCapabilities (Just False)))
      textDocumentCaps = LSP.TextDocumentClientCapabilities
        (Just (LSP.SynchronizationTextDocumentClientCapabilities
                 (Just False)
                 (Just False)
                 (Just False)
                 (Just False)))
        (Just (LSP.CompletionClientCapabilities
                 (Just False)
                 (Just (LSP.CompletionItemClientCapabilities (Just False)))))
        (Just (LSP.HoverClientCapabilities (Just False)))
        (Just (LSP.SignatureHelpClientCapabilities (Just False)))
        (Just (LSP.ReferencesClientCapabilities (Just False)))
        (Just (LSP.DocumentHighlightClientCapabilities (Just False)))
        (Just (LSP.DocumentSymbolClientCapabilities (Just False)))
        (Just (LSP.FormattingClientCapabilities (Just False)))
        (Just (LSP.RangeFormattingClientCapabilities (Just False)))
        (Just (LSP.OnTypeFormattingClientCapabilities (Just False)))
        (Just (LSP.DefinitionClientCapabilities (Just False)))
        (Just (LSP.CodeActionClientCapabilities (Just False)))
        (Just (LSP.CodeLensClientCapabilities (Just False)))
        (Just (LSP.DocumentLinkClientCapabilities (Just False)))
        (Just (LSP.RenameClientCapabilities (Just False)))

      initializeParams :: LSP.InitializeParams
      initializeParams = LSP.InitializeParams (Just pid) Nothing Nothing Nothing caps Nothing


  (Just inp, Just out, _, _) <- createProcess (proc "hie" ["--lsp", "-l", "/tmp/hie.log", "--debug"])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

  client <- Client.start (Client.Config inp out testNotificationMessageHandler testRequestMessageHandler)

  Client.sendClientRequest client (Proxy :: Proxy LSP.InitializeRequest) LSP.Initialize initializeParams

  Client.sendClientNotification client LSP.Initialized (Just LSP.InitializedParams)

  txt <- T.readFile file

  let uri = LSP.filePathToUri file

  Client.sendClientNotification client LSP.TextDocumentDidOpen (Just (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem uri "haskell" 1 txt)))

  Client.sendClientRequest
    client
    (Proxy :: Proxy LSP.DefinitionRequest)
    LSP.TextDocumentDefinition
    (LSP.TextDocumentPositionParams (LSP.TextDocumentIdentifier uri) (LSP.Position 88 36)) >>= \case
      Just (Right pos) -> print pos
      _ -> putStrLn "Server couldn't give us defnition position"

  Client.sendClientRequest client (Proxy :: Proxy LSP.DocumentSymbolRequest) LSP.TextDocumentDocumentSymbol (LSP.DocumentSymbolParams (LSP.TextDocumentIdentifier uri))
    >>= \case
      Just (Right as) -> mapM_ T.putStrLn (as ^.. traverse . LSP.name)
      _ -> putStrLn "Server couldn't give us document symbol information"

  Client.sendClientRequest client (Proxy :: Proxy LSP.ShutdownRequest) LSP.Shutdown Nothing
  Client.sendClientNotification client LSP.Exit (Just LSP.ExitParams)

  Client.stop client

testRequestMessageHandler :: Client.RequestMessageHandler
testRequestMessageHandler = Client.RequestMessageHandler
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  where
    toRspId (LSP.IdInt i) = LSP.IdRspInt i
    toRspId (LSP.IdString t) = LSP.IdRspString t

    emptyResponse :: LSP.RequestMessage m req resp -> LSP.ResponseMessage a
    emptyResponse m = LSP.ResponseMessage (m ^. LSP.jsonrpc) (toRspId (m ^. LSP.id)) Nothing Nothing

testNotificationMessageHandler :: Client.NotificationMessageHandler
testNotificationMessageHandler = Client.NotificationMessageHandler
  (T.putStrLn . view (LSP.params . LSP.message))
  (T.putStrLn . view (LSP.params . LSP.message))
  (print . view LSP.params)
  (mapM_ T.putStrLn . (^.. LSP.params . LSP.diagnostics . traverse . LSP.message))
