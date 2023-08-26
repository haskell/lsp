{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent
import Control.Exception qualified as E
import Control.Lens hiding (Iso)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson qualified as J
import Data.Int (Int32)
import Data.Text qualified as T
import GHC.Generics (Generic)
import JSONRPC.Typed.Method
import JSONRPC.Typed.Server hiding (ServerDefinition)
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.Server.DynamicRegistration qualified as Registration
import Language.LSP.Server.Config qualified as Config
import Language.LSP.Server.Progress qualified as Progress
import Language.LSP.Diagnostics qualified as Diagnostics
import Language.LSP.VFS
import Prettyprinter
import System.IO
import JSONRPC.Typed.RPC qualified as RPC

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

main :: IO ()
main = flip E.catches handlers $ do
  let
    -- Three loggers:
    -- 1. To stderr
    -- 2. To the client (filtered by severity)
    -- 3. To both
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap show L.logStringStderr
    -- clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    -- clientLogger = defaultClientLogger
    dualLogger :: LogAction IO (WithSeverity T.Text)
    dualLogger = stderrLogger -- <> clientLogger
    serverDefinition =
      ServerDefinition
        { defaultConfig = Config{fooTheBar = False, wibbleFactor = 0}
        , parseConfig = \_old v -> do
            case J.fromJSON v of
              J.Error e -> Left (T.pack e)
              J.Success cfg -> Right cfg
        , onConfigChange = const $ pure ()
        , configSection = "demo"
        , doInitialize = \h _ -> pure $ Right $ lspHandlers h dualLogger
        , options = lspOptions
        }

  let
    logToText = T.pack . show . pretty
  runLspServerWithHandles
    (L.cmap (fmap logToText) dualLogger)
    stdin
    stdout
    serverDefinition
 where
  handlers = [E.Handler ioExcept, E.Handler someExcept]
  ioExcept (e :: E.IOException) = print e >> return ()
  someExcept (e :: E.SomeException) = print e >> return ()

-- ---------------------------------------------------------------------

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    , optExecuteCommandCommands = Just ["lsp-hello-command"]
    }

-- ---------------------------------------------------------------------

{- | Analyze the file and send any diagnostics to the client in a
 "textDocument/publishDiagnostics" notification
-}
sendDiagnostics :: LanguageServerHandle Config -> LSP.NormalizedUri -> Maybe Int32 -> IO ()
sendDiagnostics h fileUri version = do
  let
    diags =
      [ LSP.Diagnostic
          (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
          (Just LSP.DiagnosticSeverity_Warning) -- severity
          Nothing -- code
          Nothing
          (Just "lsp-hello") -- source
          "Example diagnostic message"
          Nothing -- tags
          (Just [])
          Nothing
      ]
  Diagnostics.publishDiagnostics h.diagnosticsHandle 100 fileUri version (partitionBySource diags)

-- | Where the actual logic resides for handling requests and notifications.
lspHandlers :: LanguageServerHandle Config -> L.LogAction IO (WithSeverity T.Text) -> LspServerHandlers
lspHandlers h logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ mkNotificationHandler $ \_msg -> do
        logger <& "Processing the Initialized notification" `WithSeverity` Info

        -- We're initialized! Lets send a showMessageRequest now
        let params =
              LSP.ShowMessageRequestParams
                LSP.MessageType_Warning
                "What's your favourite language extension?"
                (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

        res <- requestAwaitResponse h.serverHandle LSP.SMethod_WindowShowMessageRequest params
        case res of
          Left e -> logger <& ("Got an error: " <> T.pack (show e)) `WithSeverity` Error
          Right _ -> do
            RPC.sendNotification h.serverHandle.rpcHandle LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Excellent choice")

            -- We can dynamically register a capability once the user accepts it
            RPC.sendNotification h.serverHandle.rpcHandle LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Turning on code lenses dynamically")

            let regOpts = LSP.CodeLensRegistrationOptions (LSP.InR LSP.Null) Nothing (Just False)

            void
              $ Registration.registerCapability
                h.registrationHandle
                LSP.SMethod_TextDocumentCodeLens
                regOpts
              $ requestHandler LSP.SMethod_TextDocumentCodeLens $ mkRequestHandler $ \_req -> do
                logger <& "Processing a textDocument/codeLens request" `WithSeverity` Info
                let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                    rsp = [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
                pure (Right $ LSP.InL rsp)
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ mkNotificationHandler $ \p -> do
        let doc = p ^. LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics h (LSP.toNormalizedUri doc) (Just 0)
    , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ mkNotificationHandler $ \msg -> do
        cfg <- Config.getConfig h.configHandle
        logger L.<& ("Configuration changed: " <> T.pack (show (msg, cfg))) `WithSeverity` Info
        RPC.sendNotification h.serverHandle.rpcHandle LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ mkNotificationHandler $ \p -> do
        let doc =
              p
                ^. LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        logger <& ("Processing DidChangeTextDocument for: " <> T.pack (show doc)) `WithSeverity` Info
        mdoc <- atomically $ getVirtualFile h.vfsHandle doc
        case mdoc of
          Just (VirtualFile _version str _) -> do
            logger <& ("Found the virtual file: " <> T.pack (show str)) `WithSeverity` Info
          Nothing -> do
            logger <& ("Didn't find anything in the VFS for: " <> T.pack (show doc)) `WithSeverity` Info
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ mkNotificationHandler $ \p -> do
        let doc = p ^. LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics h (LSP.toNormalizedUri doc) Nothing
    , requestHandler LSP.SMethod_TextDocumentRename $ mkRequestHandler $ \params -> do
        logger <& "Processing a textDocument/rename request" `WithSeverity` Info
        let LSP.Position l c = params ^. LSP.position
            newName = params ^. LSP.newName
        vdoc <- atomically $ getVersionedTextDoc h.vfsHandle (params ^. LSP.textDocument)
        -- Replace some text at the position with what the user entered
        let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + fromIntegral (T.length newName))) newName
            tde = LSP.TextDocumentEdit (LSP._versionedTextDocumentIdentifier # vdoc) [edit]
            -- "documentChanges" field is preferred over "changes"
            rsp = LSP.WorkspaceEdit Nothing (Just [LSP.InL tde]) Nothing
        pure (Right $ LSP.InL rsp)
    , requestHandler LSP.SMethod_TextDocumentHover $ mkRequestHandler $ \params -> do
        logger <& "Processing a textDocument/hover request" `WithSeverity` Info
        let LSP.HoverParams _doc pos _workDone = params
            LSP.Position _l _c' = pos
            rsp = LSP.Hover ms (Just range)
            ms = LSP.InL $ LSP.mkMarkdown "Your type info here!"
            range = LSP.Range pos pos
        pure (Right $ LSP.InL rsp)
    , requestHandler LSP.SMethod_TextDocumentDocumentSymbol $ mkRequestHandler $ \params -> do
        logger <& "Processing a textDocument/documentSymbol request" `WithSeverity` Info
        let LSP.DocumentSymbolParams _ _ doc = params
            loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
            rsp = [LSP.SymbolInformation "lsp-hello" LSP.SymbolKind_Function Nothing Nothing Nothing loc]
        pure (Right $ LSP.InL rsp)
    , requestHandler LSP.SMethod_TextDocumentCodeAction $ mkRequestHandler $ \params -> do
        logger <& "Processing a textDocument/codeAction request" `WithSeverity` Info
        let doc = params ^. LSP.textDocument
            diags = params ^. LSP.context . LSP.diagnostics
            -- makeCommand only generates commands for diagnostics whose source is us
            makeCommand d
              | (LSP.Range s _) <- d ^. LSP.range
              , (Just "lsp-hello") <- d ^. LSP.source =
                  let
                    title = "Apply LSP hello command:" <> head (T.lines $ d ^. LSP.message)
                    -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                    cmd = "lsp-hello-command"
                    -- need 'file' and 'start_pos'
                    args =
                      [ J.object [("file", J.object [("textDocument", J.toJSON doc)])]
                      , J.object [("start_pos", J.object [("position", J.toJSON s)])]
                      ]
                    cmdparams = Just args
                   in
                    [LSP.Command title cmd cmdparams]
            makeCommand _ = []
            rsp = map LSP.InL $ concatMap makeCommand diags
        pure (Right $ LSP.InL rsp)
    , requestHandler LSP.SMethod_WorkspaceExecuteCommand $ mkRequestHandler $ \params -> do
        logger <& "Processing a workspace/executeCommand request" `WithSeverity` Info
        let margs = params ^. LSP.arguments

        logger <& ("The arguments are: " <> T.pack (show margs)) `WithSeverity` Debug
        void $ Progress.withProgress h.progressHandle "Executing some long running command" (params ^. LSP.workDoneToken) Progress.Cancellable $ \update ->
          forM [(0 :: LSP.UInt) .. 10] $ \i -> do
            update (Progress.ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
            liftIO $ threadDelay (1 * 1000000)
        pure (Right $ LSP.InL (J.Object mempty)) -- respond to the request
    ]
