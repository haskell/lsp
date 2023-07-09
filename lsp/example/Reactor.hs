{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations #-}

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

import           Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import qualified Colog.Core as L
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                     as E
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson                            as J
import           Data.Int (Int32)
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           GHC.Generics (Generic)
import           Language.LSP.Server
import           System.IO
import           Language.LSP.Diagnostics
import           Language.LSP.Logging (defaultClientLogger)
import qualified Language.LSP.Protocol.Types            as LSP
import qualified Language.LSP.Protocol.Lens       as LSP
import qualified Language.LSP.Protocol.Message          as LSP
import           Language.LSP.VFS
import           System.Exit
import           Control.Concurrent


-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
--

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data Config = Config { fooTheBar :: Bool, wibbleFactor :: Int }
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    -- Three loggers:
    -- 1. To stderr
    -- 2. To the client (filtered by severity)
    -- 3. To both
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap show L.logStringStderr
    clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    clientLogger = defaultClientLogger
    dualLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

    serverDefinition = ServerDefinition
      { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0 }
      , onConfigurationChange = \_old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right cfg
      , doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env)
      -- Handlers log to both the client and stderr
      , staticHandlers = \_caps -> lspHandlers dualLogger rin
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

  let
    logToText = T.pack . show . pretty
  runServerWithHandles
      -- Log to both the client and stderr when we can, stderr beforehand
    (L.cmap (fmap logToText) stderrLogger)
    (L.cmap (fmap logToText) dualLogger)
    stdin
    stdout
    serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions = LSP.TextDocumentSyncOptions
  { LSP._openClose         = Just True
  , LSP._change            = Just LSP.TextDocumentSyncKind_Incremental
  , LSP._willSave          = Just False
  , LSP._willSaveWaitUntil = Just False
  , LSP._save              = Just $ LSP.InR $ LSP.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { optTextDocumentSync = Just syncOptions
  , optExecuteCommandCommands = Just ["lsp-hello-command"]
  }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> LspM Config ()
sendDiagnostics fileUri version = do
  let
    diags = [LSP.Diagnostic
              (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
              (Just LSP.DiagnosticSeverity_Warning)  -- severity
              Nothing  -- code
              Nothing
              (Just "lsp-hello") -- source
              "Example diagnostic message"
              Nothing -- tags
              (Just [])
              Nothing
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: L.LogAction IO (WithSeverity T.Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> TChan ReactorInput -> Handlers m
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
  where
    goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). Handler (LspM Config) a -> Handler (LspM Config) a
    goReq f = \msg k -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger = mconcat
  [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
      logger <& "Processing the Initialized notification" `WithSeverity` Info
      
      -- We're initialized! Lets send a showMessageRequest now
      let params = LSP.ShowMessageRequestParams
                         LSP.MessageType_Warning
                         "What's your favourite language extension?"
                         (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

      void $ sendRequest LSP.SMethod_WindowShowMessageRequest params $ \res ->
        case res of
          Left e -> logger <& ("Got an error: " <> T.pack (show e)) `WithSeverity` Error
          Right _ -> do
            sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Excellent choice")

            -- We can dynamically register a capability once the user accepts it
            sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Turning on code lenses dynamically")
            
            let regOpts = LSP.CodeLensRegistrationOptions (LSP.InR LSP.Null) Nothing (Just False)
            
            void $ registerCapability LSP.SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              logger <& "Processing a textDocument/codeLens request" `WithSeverity` Info
              let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right $ LSP.InL rsp)

  , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. LSP.params . LSP.textDocument . LSP.uri
        fileName =  LSP.uriToFilePath doc
    logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
    sendDiagnostics (LSP.toNormalizedUri doc) (Just 0)

  , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
      cfg <- getConfig
      logger L.<& ("Configuration changed: " <> T.pack (show (msg,cfg))) `WithSeverity` Info
      sendNotification LSP.SMethod_WindowShowMessage $
        LSP.ShowMessageParams LSP.MessageType_Info $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))

  , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
    let doc  = msg ^. LSP.params
                    . LSP.textDocument
                    . LSP.uri
                    . to LSP.toNormalizedUri
    logger <& ("Processing DidChangeTextDocument for: " <> T.pack (show doc)) `WithSeverity` Info
    mdoc <- getVirtualFile doc
    case mdoc of
      Just (VirtualFile _version str _) -> do
        logger <& ("Found the virtual file: " <> T.pack (show str)) `WithSeverity` Info
      Nothing -> do
        logger <& ("Didn't find anything in the VFS for: " <> T.pack (show doc)) `WithSeverity` Info

  , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do
      let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
          fileName = LSP.uriToFilePath doc
      logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show fileName)) `WithSeverity` Info
      sendDiagnostics (LSP.toNormalizedUri doc) Nothing

  , requestHandler LSP.SMethod_TextDocumentRename $ \req responder -> do
      logger <& "Processing a textDocument/rename request" `WithSeverity` Info
      let params = req ^. LSP.params
          LSP.Position l c = params ^. LSP.position
          newName = params ^. LSP.newName
      vdoc <- getVersionedTextDoc (params ^. LSP.textDocument)
      -- Replace some text at the position with what the user entered
      let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + fromIntegral (T.length newName))) newName
          tde = LSP.TextDocumentEdit (LSP._versionedTextDocumentIdentifier # vdoc) [edit]
          -- "documentChanges" field is preferred over "changes"
          rsp = LSP.WorkspaceEdit Nothing (Just [LSP.InL tde]) Nothing
      responder (Right $ LSP.InL rsp)

  , requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
      logger <& "Processing a textDocument/hover request" `WithSeverity` Info
      let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
          LSP.Position _l _c' = pos
          rsp = LSP.Hover ms (Just range)
          ms = LSP.InL $ LSP.mkMarkdown "Your type info here!"
          range = LSP.Range pos pos
      responder (Right $ LSP.InL rsp)

  , requestHandler LSP.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
      logger <& "Processing a textDocument/documentSymbol request" `WithSeverity` Info
      let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
          loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
          rsp = [LSP.SymbolInformation "lsp-hello" LSP.SymbolKind_Function Nothing Nothing Nothing loc]
      responder (Right $ LSP.InL rsp)

  , requestHandler LSP.SMethod_TextDocumentCodeAction $ \req responder -> do
      logger <& "Processing a textDocument/codeAction request" `WithSeverity` Info
      let params = req ^. LSP.params
          doc = params ^. LSP.textDocument
          diags = params ^. LSP.context . LSP.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand d | (LSP.Range s _) <- d ^. LSP.range, (Just "lsp-hello") <- d ^. LSP.source =
            let
              title = "Apply LSP hello command:" <> head (T.lines $ d ^. LSP.message)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = [ J.object [("file",     J.object [("textDocument",J.toJSON doc)])]
                     , J.object [("start_pos",J.object [("position",    J.toJSON s)])]
                     ]
              cmdparams = Just args
            in [LSP.Command title cmd cmdparams]
          makeCommand _ = []
          rsp = map LSP.InL $ concatMap makeCommand diags
      responder (Right $ LSP.InL rsp)

  , requestHandler LSP.SMethod_WorkspaceExecuteCommand $ \req responder -> do
      logger <& "Processing a workspace/executeCommand request" `WithSeverity` Info
      let params = req ^. LSP.params
          margs = params ^. LSP.arguments

      logger <& ("The arguments are: " <> T.pack (show margs)) `WithSeverity` Debug
      responder (Right $ LSP.InL (J.Object mempty)) -- respond to the request

      void $ withProgress "Executing some long running command" Cancellable $ \update ->
        forM [(0 :: LSP.UInt)..10] $ \i -> do
          update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
          liftIO $ threadDelay (1 * 1000000)
  ]

-- ---------------------------------------------------------------------
