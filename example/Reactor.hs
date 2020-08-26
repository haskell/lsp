{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

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
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                     as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Control
import qualified Data.Aeson                            as J
import           Data.Default
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import qualified Language.Haskell.LSP.Control          as CTRL
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import           Language.Haskell.LSP.VFS
import           System.Exit
import           System.Log.Logger


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

run :: IO Int
run = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    callbacks = InitializeCallbacks
      { onInitialConfiguration = const $ Right ()
      , onConfigurationChange = const $ Right ()
      , onStartup = do
          _reactorThreadId <- (liftBaseDiscard forkIO) (reactor rin)
          return Nothing
      }

  flip E.finally finalProc $ do
    setupLogger Nothing [] DEBUG
    CTRL.run callbacks (lspHandlers rin) lspOptions

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
  , J._save              = Just $ J.R $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = def { textDocumentSync = Just syncOptions
                 , executeCommandCommands = Just ["lsp-hello-command"]
                 }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorInput
  = forall (m :: J.Method 'J.FromClient 'J.Request).
    ReactorInputReq (J.SMethod m) (J.RequestMessage m) (Either J.ResponseError (J.ResponseParams m) -> LspM () ())
  | forall (m :: J.Method 'J.FromClient 'J.Notification).
    ReactorInputNot (J.SMethod m) (J.NotificationMessage m)

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> Maybe Int -> LspM () ()
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
  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> LspM () ()
reactor inp = do
  liftIO $ debugM "reactor" "entered"
  forever $ do
    reactorInput <- liftIO $ atomically $ readTChan inp
    case reactorInput of
      ReactorInputReq method msg responder ->
        case handle method of
          Just f -> f msg responder
          Nothing -> pure ()
      ReactorInputNot method msg ->
        case handle method of
          Just f -> f msg
          Nothing -> pure ()

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Handlers ()
lspHandlers rin method =
  case handle method of
    Just _ -> case J.splitClientMethod method of
      J.IsClientReq -> Just $ \clientMsg responder ->
        liftIO $ atomically $ writeTChan rin (ReactorInputReq method clientMsg responder)
      J.IsClientNot -> Just $ \clientMsg ->
        liftIO $ atomically $ writeTChan rin (ReactorInputNot method clientMsg)
      J.IsClientEither -> error "TODO???"
    Nothing -> Nothing

-- | Where the actual logic resides for handling requests and notifications.
handle :: J.SMethod m -> Maybe (Handler m ())
handle J.SInitialized = Just $ \_msg -> do
    liftIO $ debugM "handle" "Processing the Initialized notification"
    
    -- We're initialized! Lets send a showMessageRequest now
    let params = J.ShowMessageRequestParams
                       J.MtWarning
                       "What's your favourite language extension?"
                       (Just [J.MessageActionItem "Rank2Types", J.MessageActionItem "NPlusKPatterns"])

    void $ sendRequest J.SWindowShowMessageRequest params $ \res ->
      case res of
        Left e -> liftIO $ errorM "handle" $ "Got an error: " ++ show e
        Right _ -> do
          sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Excellent choice")

          -- We can dynamically register a capability once the user accepts it
          sendNotification J.SWindowShowMessage (J.ShowMessageParams J.MtInfo "Turning on code lenses dynamically")
          
          let regOpts = J.CodeLensRegistrationOptions Nothing Nothing (Just False)
          
          void $ registerCapability J.STextDocumentCodeLens regOpts $ \_req responder -> do
            liftIO $ debugM "handle" "Processing a textDocument/codeLens request"
            let cmd = J.Command "Say hello" "lsp-hello-command" Nothing
                rsp = J.List [J.CodeLens (J.mkRange 0 0 0 100) (Just cmd) Nothing]
            responder (Right rsp)

handle J.STextDocumentDidOpen = Just $ \msg -> do
  let doc  = msg ^. J.params . J.textDocument . J.uri
      fileName =  J.uriToFilePath doc
  liftIO $ debugM "handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
  sendDiagnostics (J.toNormalizedUri doc) (Just 0)

handle J.STextDocumentDidChange = Just $ \msg -> do
  let doc  = msg ^. J.params
                  . J.textDocument
                  . J.uri
                  . to J.toNormalizedUri
  liftIO $ debugM "handle" $ "Processing DidChangeTextDocument for: " ++ show doc
  mdoc <- getVirtualFile doc
  case mdoc of
    Just (VirtualFile _version str _) -> do
      liftIO $ debugM "handle" $ "Found the virtual file: " ++ show str
    Nothing -> do
      liftIO $ debugM "handle" $ "Didn't find anything in the VFS for: " ++ show doc

handle J.STextDocumentDidSave = Just $ \msg -> do
  let doc = msg ^. J.params . J.textDocument . J.uri
      fileName = J.uriToFilePath doc
  liftIO $ debugM "handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  sendDiagnostics (J.toNormalizedUri doc) Nothing

handle J.STextDocumentRename = Just $ \req responder -> do
  liftIO $ debugM "handle" "Processing a textDocument/rename request"
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

handle J.STextDocumentHover = Just $ \req responder -> do
  liftIO $ debugM "handle" "Processing a textDocument/hover request"
  let J.HoverParams _doc pos _workDone = req ^. J.params
      J.Position _l _c' = pos
      rsp = J.Hover ms (Just range)
      ms = J.HoverContents $ J.markedUpContent "lsp-hello" "Your type info here!"
      range = J.Range pos pos
  responder (Right rsp)

handle J.STextDocumentCodeAction = Just $ \req responder -> do
  liftIO $ debugM "handle" $ "Processing a textDocument/codeAction request"
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
      rsp = J.List $ map J.L $ concatMap makeCommand diags
  responder (Right rsp)

handle J.SWorkspaceExecuteCommand = Just $ \req responder -> do
  liftIO $ debugM "handle" "Processing a workspace/executeCommand request"
  let params = req ^. J.params
      margs = params ^. J.arguments

  liftIO $ debugM "handle" $ "The arguments are: " ++ show margs
  responder (Right (J.Object mempty)) -- respond to the request

  void $ withProgress "Executing some long running command" Cancellable $ \update ->
    forM [(0 :: Double)..10] $ \i -> do
      update (Progress (Just (i * 10)) (Just "Doing stuff"))
      liftIO $ threadDelay (1 * 1000000)


handle _ = Nothing

-- ---------------------------------------------------------------------
