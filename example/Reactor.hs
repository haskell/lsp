{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `haskell-lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                     as E
import           Control.Lens hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson                            as J
import           Data.Default
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           GHC.Generics (Generic)
import qualified Language.Haskell.LSP.Control          as CTRL
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import           Language.Haskell.LSP.VFS
import           System.Exit
import           System.Log.Logger
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
  deriving (Generic, J.ToJSON, J.FromJSON)

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    callbacks = InitializeCallbacks
      { onConfigurationChange = \v -> case J.fromJSON v of
          J.Error e -> pure $ Left (T.pack e)
          J.Success cfg -> do
            sendNotification J.SWindowShowMessage $
              J.ShowMessageParams J.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))
            pure $ Right cfg
      , doInitialize = const $ forkIO (reactor rin) >> pure (Right ())
      , staticHandlers = lspHandlers rin
      , interpretHandler = const $ \env -> Iso (runLspT env) liftIO
      }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor"] DEBUG
    CTRL.run callbacks lspOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = def { textDocumentSync = Just syncOptions
                 , executeCommandCommands = Just ["lsp-hello-command"]
                 }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> Maybe Int -> LspM Config ()
sendDiagnostics fileUri version = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              "Example diagnostic message"
              Nothing -- tags
              (Just (J.List []))
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Handlers (LspM Config)
lspHandlers rin = mapHandlers goReq goNot handle
  where
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM Config) a -> Handler (LspM Config) a
    goReq f = \msg k -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: Handlers (LspM Config)
handle = mconcat
  [ notificationHandler J.SInitialized $ \_msg -> do
      liftIO $ debugM "reactor.handle" "Processing the Initialized notification"
      
      -- We're initialized! Lets send a showMessageRequest now
      let params = J.ShowMessageRequestParams
                         J.MtWarning
                         "What's your favourite language extension?"
                         (Just [J.MessageActionItem "Rank2Types", J.MessageActionItem "NPlusKPatterns"])

      void $ sendRequest J.SWindowShowMessageRequest params $ \res ->
        case res of
          Left e -> liftIO $ errorM "reactor.handle" $ "Got an error: " ++ show e
          Right _ -> do
            sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Excellent choice")

            -- We can dynamically register a capability once the user accepts it
            sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Turning on code lenses dynamically")
            
            let regOpts = J.CodeLensRegistrationOptions Nothing Nothing (Just False)
            
            void $ registerCapability J.STextDocumentCodeLens regOpts $ \_req responder -> do
              liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
              let cmd = J.Command "Say hello" "lsp-hello-command" Nothing
                  rsp = J.List [J.CodeLens (J.mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)

  , notificationHandler J.STextDocumentDidOpen $ \msg -> do
    let doc  = msg ^. J.params . J.textDocument . J.uri
        fileName =  J.uriToFilePath doc
    liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
    sendDiagnostics (J.toNormalizedUri doc) (Just 0)

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
    let doc  = msg ^. J.params
                    . J.textDocument
                    . J.uri
                    . to J.toNormalizedUri
    liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
    mdoc <- getVirtualFile doc
    case mdoc of
      Just (VirtualFile _version str _) -> do
        liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show str
      Nothing -> do
        liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc

  , notificationHandler J.STextDocumentDidSave $ \msg -> do
      let doc = msg ^. J.params . J.textDocument . J.uri
          fileName = J.uriToFilePath doc
      liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
      sendDiagnostics (J.toNormalizedUri doc) Nothing

  , requestHandler J.STextDocumentRename $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
      let params = req ^. J.params
          J.Position l c = params ^. J.position
          newName = params ^. J.newName
      vdoc <- getVersionedTextDoc (params ^. J.textDocument)
      -- Replace some text at the position with what the user entered
      let edit = J.TextEdit (J.mkRange l c l (c + T.length newName)) newName
          tde = J.TextDocumentEdit vdoc (J.List [edit])
          -- "documentChanges" field is preferred over "changes"
          rsp = J.WorkspaceEdit Nothing (Just (J.List [tde]))
      responder (Right rsp)

  , requestHandler J.STextDocumentHover $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
      let J.HoverParams _doc pos _workDone = req ^. J.params
          J.Position _l _c' = pos
          rsp = J.Hover ms (Just range)
          ms = J.HoverContents $ J.markedUpContent "lsp-hello" "Your type info here!"
          range = J.Range pos pos
      responder (Right $ Just rsp)

  , requestHandler J.STextDocumentCodeAction $ \req responder -> do
      liftIO $ debugM "reactor.handle" $ "Processing a textDocument/codeAction request"
      let params = req ^. J.params
          doc = params ^. J.textDocument
          (J.List diags) = params ^. J.context . J.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _t _l) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = J.List
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m _t _l) = []
          rsp = J.List $ map J.InL $ concatMap makeCommand diags
      responder (Right rsp)

  , requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
      let params = req ^. J.params
          margs = params ^. J.arguments

      liftIO $ debugM "reactor.handle" $ "The arguments are: " ++ show margs
      responder (Right (J.Object mempty)) -- respond to the request

      void $ withProgress "Executing some long running command" Cancellable $ \update ->
        forM [(0 :: Double)..10] $ \i -> do
          update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
          liftIO $ threadDelay (1 * 1000000)
  ]

-- ---------------------------------------------------------------------
