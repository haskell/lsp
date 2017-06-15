{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Aeson as J
import           Data.Default
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import           Language.Haskell.LSP.VFS
import           System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi
import Control.Lens


-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
--

main :: IO ()
main = do
  run (return ()) >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

run :: IO () -> IO Int
run dispatcherProc = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    dp lf = do
      liftIO $ U.logs $ "main.run:dp entered"
      _rpid  <- forkIO $ reactor lf def rin
      liftIO $ U.logs $ "main.run:dp tchan"
      dispatcherProc
      liftIO $ U.logs $ "main.run:dp after dispatcherProc"
      return Nothing

  flip E.finally finalProc $ do
    Core.setupLogger "/tmp/lsp-hello.log" L.DEBUG
    CTRL.run dp (lspHandlers rin) lspOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorInput
  = HandlerRequest Core.OutMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

data ReactorState =
  ReactorState
    { lspReqId           :: !J.LspId -- ^ unique ids for requests to the client
    }

instance Default ReactorState where
  def = ReactorState (J.IdInt 0)

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = ReaderT Core.LspFuncs (StateT ReactorState IO) a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => a -> R ()
reactorSend msg = do
  lf <- ask
  liftIO $ Core.sendFunc lf msg

-- ---------------------------------------------------------------------

publishDiagnostics :: J.Uri -> Maybe J.TextDocumentVersion -> DiagnosticsBySource -> R ()
publishDiagnostics uri mv diags = do
  lf <- ask
  liftIO $ (Core.publishDiagnosticsFunc lf) uri mv diags

-- ---------------------------------------------------------------------

nextLspReqId :: R J.LspId
nextLspReqId = do
  s <- get
  let i@(J.IdInt r) = lspReqId s
  put s { lspReqId = J.IdInt (r + 1) }
  return i

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: Core.LspFuncs -> ReactorState -> TChan ReactorInput -> IO ()
reactor lf st inp = do
  liftIO $ U.logs $ "reactor:entered"
  flip evalStateT st $ flip runReaderT lf $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of

      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
      HandlerRequest (Core.RspFromClient rm) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest (Core.NotInitialized _notification) -> do
        liftIO $ U.logm $ "****** reactor: processing Initialized Notification"
        -- Server is ready, register any specific capabilities we need

         {-
         Example:
         {
                 "method": "client/registerCapability",
                 "params": {
                         "registrations": [
                                 {
                                         "id": "79eee87c-c409-4664-8102-e03263673f6f",
                                         "method": "textDocument/willSaveWaitUntil",
                                         "registerOptions": {
                                                 "documentSelector": [
                                                         { "language": "javascript" }
                                                 ]
                                         }
                                 }
                         ]
                 }
         }
        -}
        let
          registration = J.Registration "lsp-hello-registered" "workspace/executeCommand" Nothing
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        reactorSend $ fmServerRegisterCapabilityRequest rid registrations

        -- example of showMessageRequest
        let
          params = J.ShowMessageRequestParams J.MtWarning "choose an option for XXX"
                           (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
        rid1 <- nextLspReqId

        reactorSend $ fmServerShowMessageRequest rid1 params

      -- -------------------------------

      HandlerRequest (Core.NotDidOpenTextDocument notification) -> do
        liftIO $ U.logm $ "****** reactor: processing NotDidOpenTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName =  J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc (Just 0)

      -- -------------------------------

      HandlerRequest (Core.NotDidChangeTextDocument notification) -> do
        let doc :: J.Uri
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
        mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
        case mdoc of
          Just (VirtualFile _version str) -> do
            liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: vf got:" ++ (show $ Yi.toString str)
          Nothing -> do
            liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: vf returned Nothing"

        liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++ (show doc)

      -- -------------------------------

      HandlerRequest (Core.NotDidSaveTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc Nothing

      -- -------------------------------

      HandlerRequest (Core.ReqRename req) -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let
            params = req ^. J.params
            doc  = params ^. J.textDocument . J.uri
            J.Position l c = params ^. J.position
            newName  = params ^. J.newName

        let we = J.WorkspaceEdit
                    Nothing -- "changes" field is deprecated
                    (Just (J.List [])) -- populate with actual changes from the rename
        let rspMsg = Core.makeResponseMessage (J.responseId $ req ^. J.id) we
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest (Core.ReqHover req) -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let J.TextDocumentPositionParams doc pos = req ^. J.params
            J.Position l c = pos

        let
          ht = J.Hover ms (Just range)
          ms = J.List [J.MarkedString "lsp-hello" "TYPE INFO" ]
          range = J.Range pos pos
        reactorSend $ Core.makeResponseMessage (J.responseId $ req ^. J.id) ht

      -- -------------------------------

      HandlerRequest (Core.ReqCodeAction req) -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            (J.List diags) = params ^. J.context . J.diagnostics

        let
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m  ) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = J.Array$ V.fromList
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m  ) = []
        let body = concatMap makeCommand diags
        reactorSend $ Core.makeResponseMessage (J.responseId $ req ^. J.id) body

      -- -------------------------------

      HandlerRequest (Core.ReqExecuteCommand req) -> do
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = req ^. J.params
            margs = params ^. J.arguments

        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs

        let
          reply v = reactorSend $ Core.makeResponseMessage (J.responseId $ req ^. J.id) v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
          r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (J.Object mempty)
            lid <- nextLspReqId
            -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            reactorSend $ fmServerApplyWorkspaceEditRequest lid we
          Nothing ->
            reply r

      -- -------------------------------

      HandlerRequest om -> do
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------

toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Uri -> Maybe Int -> R ()
sendDiagnostics fileUri mversion = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              "Example diagnostic message"
            ]
  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  publishDiagnostics fileUri mversion (partitionBySource diags)

-- ---------------------------------------------------------------------

syncMethod :: J.TextDocumentSyncKind
syncMethod = J.TdSyncIncremental

lspOptions :: Core.Options
lspOptions = def { Core.textDocumentSync = Just syncMethod
                 , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
                 }

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin Core.NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin Core.ReqRename
        , Core.hoverHandler                             = Just $ passHandler rin Core.ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin Core.NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin Core.NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin Core.NotCancelRequest
        , Core.responseHandler                          = Just $ responseHandlerCb rin
        , Core.codeActionHandler                        = Just $ passHandler rin Core.ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin Core.ReqExecuteCommand
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> Core.OutMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------
