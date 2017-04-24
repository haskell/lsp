{-# LANGUAGE DuplicateRecordFields #-}
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
import           Control.Monad.Trans.State.Lazy
import qualified Data.Aeson as J
-- import qualified Data.Aeson.Types as J
-- import           Data.Algorithm.DiffOutput
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
-- import           Data.Either
import qualified Data.HashMap.Strict as H
-- import           Data.List
-- import qualified Data.Map as Map
import           Data.Maybe
-- import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
-- import           System.Directory
import           System.Exit
import qualified System.Log.Logger as L


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
  _rpid  <- forkIO $ reactor def rin

  let
    dp capabilities sendFunc = do
      atomically $ writeTChan rin (InitializeCallBack capabilities sendFunc)
      dispatcherProc
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
  = InitializeCallBack C.ClientCapabilities Core.SendFunc
      -- ^ called when the LSP ioLoop receives the `initialize` message from the
      -- client, providing the client capabilities (for LSP 3.0 and later)
  | HandlerRequest (BSL.ByteString -> IO ()) Core.OutMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

data ReactorState =
  ReactorState
    { sender             :: !(Maybe (BSL.ByteString -> IO ()))
        -- ^ function provided in the 'InitializeCallBack' to send reply
        -- messages to the client.
    , clientCapabilities :: !(Maybe C.ClientCapabilities)
        -- ^ client capabilities from the 'InitializeCallBack'
    , lspReqId           :: !J.LspId -- ^ unique ids for requests to the client
    -- , wip                :: !(Map.Map RequestId Core.OutMessage)
    }

instance Default ReactorState where
  def = ReactorState Nothing Nothing (J.IdInt 0)

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = StateT ReactorState IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

setSendFunc :: (BSL.ByteString -> IO ()) -> R ()
setSendFunc sf = modify' (\s -> s {sender = Just sf})

setClientCapabilities :: C.ClientCapabilities -> R ()
setClientCapabilities c = modify' (\s -> s {clientCapabilities = Just c})

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => a -> R ()
reactorSend msg = do
  s <- get
  case sender s of
    Nothing -> error "reactorSend: send function not initialised yet"
    Just sf -> liftIO $ sf (J.encode msg)

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
reactor :: ReactorState -> TChan ReactorInput -> IO ()
reactor st inp = do
  flip evalStateT st $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of
      -- This will be the first message received from the client, and can be
      -- used to tailor processing according to the given client capabilities.
      InitializeCallBack capabilities sf -> do
        liftIO $ U.logs $ "reactor:got Client capabilities:" ++ show capabilities
        setSendFunc sf
        setClientCapabilities capabilities

      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
      HandlerRequest sf (Core.RspFromClient rm) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest sf (Core.NotInitialized _notification) -> do
        setSendFunc sf
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

        -- Current vscode implementation has the wrong name in it:
        -- https://github.com/Microsoft/vscode-languageserver-node/issues/199
        let smr = J.RequestMessage "2.0" rid "client/registerFeature"  (Just registrations)
        -- let smr = J.RequestMessage "2.0" rid "client/registerCapability"  (Just registrations)

        reactorSend smr

      -- -------------------------------

      HandlerRequest sf (Core.NotDidOpenTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "****** reactor: processing NotDidOpenTextDocument"
        let
            params  = fromJust $ J._params (notification :: J.DidOpenTextDocumentNotification)
            textDoc = J._textDocument (params :: J.DidOpenTextDocumentNotificationParams)
            doc     = J._uri (textDoc :: J.TextDocumentItem)
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "********* doc=" ++ show doc
        sendDiagnostics fileName

      -- -------------------------------

      HandlerRequest sf (Core.NotDidSaveTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            params = fromJust $ J._params (notification :: J.NotificationMessage J.DidSaveTextDocumentParams)
            J.TextDocumentIdentifier doc = J._textDocument (params :: J.DidSaveTextDocumentParams)
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "********* doc=" ++ show doc
        sendDiagnostics fileName

      HandlerRequest sf (Core.NotDidChangeTextDocument _notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: NOT processing NotDidChangeTextDocument"

      -- -------------------------------
      HandlerRequest sf (Core.ReqRename req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let params = fromJust $ J._params (req :: J.RenameRequest)
            J.TextDocumentIdentifier doc = J._textDocument (params :: J.RenameRequestParams)
            fileName = drop (length ("file://"::String)) doc
            J.Position l c = J._position (params :: J.RenameRequestParams)
            newName  = J._newName params

        let we = J.WorkspaceEdit
                    Nothing -- "changes" field is deprecated
                    (Just (J.List [])) -- populate with actual changes from the rename
        let rspMsg = Core.makeResponseMessage (J.responseId $ J._id (req :: J.RenameRequest)) we
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest sf r@(Core.ReqHover req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let J.TextDocumentPositionParams doc pos = fromJust $ J._params (req :: J.HoverRequest)
            fileName = drop (length ("file://"::String)) $ J._uri (doc :: J.TextDocumentIdentifier)
            J.Position l c = pos

        let
          ht = J.Hover ms (Just range)
          ms = [J.MarkedString "lsp-hello" "TYPE INFO" ]
          range = J.Range pos pos
        reactorSend $ Core.makeResponseMessage (J.responseId $ J._id (req :: J.HoverRequest) ) ht

      -- -------------------------------

      HandlerRequest sf (Core.ReqCodeAction req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = fromJust $ J._params (req :: J.CodeActionRequest)
            doc = J._textDocument (params :: J.CodeActionParams)
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            J.CodeActionContext (J.List diags) = J._context (params :: J.CodeActionParams)

        let
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m  ) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" ++ head (lines _m)
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
        reactorSend $ Core.makeResponseMessage (J.responseId $ J._id (req :: J.CodeActionRequest)) body

      -- -------------------------------

      HandlerRequest sf (Core.ReqExecuteCommand req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = fromJust $ J._params (req :: J.ExecuteCommandRequest)
            command = J._command (params :: J.ExecuteCommandParams)
            margs = J._arguments (params :: J.ExecuteCommandParams)

        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs

        let
          reply v = reactorSend $ Core.makeResponseMessage (J.responseId $ J._id (req :: J.ExecuteCommandRequest)) v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
          r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (J.Object mempty)
            lid <- nextLspReqId
            reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            -- reply r
          Nothing ->
            reply r

      -- -------------------------------

      HandlerRequest sf om -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------

toWorkspaceEdit :: t -> Maybe J.WorkspaceEdit
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: String -> R ()
sendDiagnostics fileUri = do
  let
    r = J.PublishDiagnosticsParams
          fileUri
          (J.List [J.Diagnostic
                    (J.Range (J.Position 1 1) (J.Position 10 0))
                    Nothing  -- severity
                    Nothing  -- code
                    (Just "lsp-hello") -- source
                    "Example diagnostic message"
                  ])
  reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)

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
passHandler rin c sf notification = do
  atomically $ writeTChan rin (HandlerRequest sf (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin _sf resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------
