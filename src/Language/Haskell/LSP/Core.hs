{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Core (
    handleRequest
  , LanguageContextData(..)
  , Handler
  , InitializeCallback
  , LspFuncs(..)
  , SendFunc
  , Handlers(..)
  , Options(..)
  , OutMessage(..)
  , defaultLanguageContextData
  , initializeRequestHandler
  , makeResponseMessage
  , makeResponseError
  , setupLogger
  , sendErrorResponseS
  , sendErrorLogS
  , sendErrorShowS
  ) where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import           Language.Haskell.LSP.Constant
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J
import           Language.Haskell.LSP.Utility
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Diagnostics
import           System.Directory
import           System.Exit
import           System.IO
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import           System.Log.Logger
import qualified System.Log.Logger as L

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resRootPath            :: !(Maybe FilePath)
  , resHandlers            :: !Handlers
  , resOptions             :: !Options
  , resSendResponse        :: !(BSL.ByteString -> IO ())
  , resVFS                 :: !VFS
  , resDiagnostics         :: !DiagnosticStore
  , resLspFuncs            :: LspFuncs -- NOTE: Cannot be strict, lazy initialization
  }

-- ---------------------------------------------------------------------

-- | Language Server Protocol options supported by the given language server.
-- These are automatically turned into capabilities reported to the client
-- during initialization.
data Options =
  Options
    { textDocumentSync                 :: Maybe J.TextDocumentSyncKind
    , completionProvider               :: Maybe J.CompletionOptions
    , signatureHelpProvider            :: Maybe J.SignatureHelpOptions
    , codeLensProvider                 :: Maybe J.CodeLensOptions
    , documentOnTypeFormattingProvider :: Maybe J.DocumentOnTypeFormattingOptions
    , documentLinkProvider             :: Maybe J.DocumentLinkOptions
    , executeCommandProvider           :: Maybe J.ExecuteCommandOptions
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A function to send a message to the client
type SendFunc = (BSL.ByteString -> IO ())

-- | A function to publish diagnostics. It aggregates all diagnostics pertaining
-- to a particular version of a document, by source, and sends a
-- 'textDocument/publishDiagnostics' notification with the total whenever it is
-- updated.
type PublishDiagnosticsFunc = J.Uri -> Maybe J.TextDocumentVersion -> [J.Diagnostic] -> IO ()

-- | Returned to the server on startup, providing ways to interact with the client.
data LspFuncs =
  LspFuncs
    { clientCapabilities     :: !C.ClientCapabilities
    , sendFunc               :: !SendFunc
    , getVirtualFileFunc     :: !(J.Uri -> IO (Maybe VirtualFile))
    , publishDiagnosticsFunc :: !PublishDiagnosticsFunc
    }

-- | The function in the LSP process that is called once the 'initialize'
-- message is received. Message processing will only continue once this returns,
-- so it should create whatever processes are needed.
type InitializeCallback = LspFuncs -> IO (Maybe J.ResponseError)

-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
type Handler b = LspFuncs -> b -> IO ()
-- TODO: change the sendFunc to take a ToJSON type instead

-- | Callbacks from the language server to the language handler
data Handlers =
  Handlers
    {
    -- Capability-advertised handlers
      hoverHandler                   :: !(Maybe (Handler J.HoverRequest))
    , completionHandler              :: !(Maybe (Handler J.CompletionRequest))
    , completionResolveHandler       :: !(Maybe (Handler J.CompletionItemResolveRequest))
    , signatureHelpHandler           :: !(Maybe (Handler J.SignatureHelpRequest))
    , definitionHandler              :: !(Maybe (Handler J.DefinitionRequest))
    , referencesHandler              :: !(Maybe (Handler J.FindReferencesRequest))
    , documentHighlightHandler       :: !(Maybe (Handler J.DocumentHighlightsRequest))
    , documentSymbolHandler          :: !(Maybe (Handler J.DocumentSymbolsRequest))
    , workspaceSymbolHandler         :: !(Maybe (Handler J.WorkspaceSymbolsRequest))
    , codeActionHandler              :: !(Maybe (Handler J.CodeActionRequest))
    , codeLensHandler                :: !(Maybe (Handler J.CodeLensRequest))
    , codeLensResolveHandler         :: !(Maybe (Handler J.CodeLensResolveRequest))
    , documentFormattingHandler      :: !(Maybe (Handler J.DocumentFormattingRequest))
    , documentRangeFormattingHandler :: !(Maybe (Handler J.DocumentRangeFormattingRequest))
    , documentTypeFormattingHandler  :: !(Maybe (Handler J.DocumentOnTypeFormattingRequest))
    , renameHandler                  :: !(Maybe (Handler J.RenameRequest))
    -- new in 3.0
    , documentLinkHandler            :: !(Maybe (Handler J.DocumentLinkRequest))
    , documentLinkResolveHandler     :: !(Maybe (Handler J.DocumentLinkResolveRequest))
    , executeCommandHandler          :: !(Maybe (Handler J.ExecuteCommandRequest))
    -- Next 2 go from server -> client
    -- , registerCapabilityHandler      :: !(Maybe (Handler J.RegisterCapabilityRequest))
    -- , unregisterCapabilityHandler    :: !(Maybe (Handler J.UnregisterCapabilityRequest))
    , willSaveWaitUntilTextDocHandler:: !(Maybe (Handler J.WillSaveWaitUntilTextDocumentResponse))

    -- Notifications from the client
    , didChangeConfigurationParamsHandler      :: !(Maybe (Handler J.DidChangeConfigurationParamsNotification))
    , didOpenTextDocumentNotificationHandler   :: !(Maybe (Handler J.DidOpenTextDocumentNotification))
    , didChangeTextDocumentNotificationHandler :: !(Maybe (Handler J.DidChangeTextDocumentNotification))
    , didCloseTextDocumentNotificationHandler  :: !(Maybe (Handler J.DidCloseTextDocumentNotification))
    , didSaveTextDocumentNotificationHandler   :: !(Maybe (Handler J.DidSaveTextDocumentNotification))
    , didChangeWatchedFilesNotificationHandler :: !(Maybe (Handler J.DidChangeWatchedFilesNotification))
    -- new in 3.0
    , initializedHandler                       :: !(Maybe (Handler J.InitializedNotification))
    , willSaveTextDocumentNotificationHandler  :: !(Maybe (Handler J.WillSaveTextDocumentNotification))

    , cancelNotificationHandler                :: !(Maybe (Handler J.CancelNotification))

    -- Responses to Request messages originated from the server
    , responseHandler                          :: !(Maybe (Handler J.BareResponseMessage))
    }

instance Default Handlers where
  def = Handlers Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing

-- ---------------------------------------------------------------------

handlerMap :: Handlers
           -> Map.Map String (MVar LanguageContextData -> String -> B.ByteString -> IO ())
handlerMap h = Map.fromList
  [ ("textDocument/completion",        hh $ completionHandler h)
  , ("completionItem/resolve",         hh $ completionResolveHandler h)
  , ("textDocument/hover",             hh $ hoverHandler h)
  , ("textDocument/signatureHelp",     hh $ signatureHelpHandler h)
  , ("textDocument/definition",        hh $ definitionHandler h)
  , ("textDocument/references",        hh $ referencesHandler h)
  , ("textDocument/documentHighlight", hh $ documentHighlightHandler h)
  , ("textDocument/documentSymbol",    hh $ documentSymbolHandler h)
  , ("workspace/symbol",               hh $ workspaceSymbolHandler h)
  , ("textDocument/codeAction",        hh $ codeActionHandler h)
  , ("textDocument/codeLens",          hh $ codeLensHandler h)
  , ("codeLens/resolve",               hh $ codeLensResolveHandler h)
  , ("textDocument/formatting",        hh $ documentFormattingHandler h)
  , ("textDocument/rangeFormatting",   hh $ documentRangeFormattingHandler h)
  , ("textDocument/onTypeFormatting",  hh $ documentTypeFormattingHandler h)
  , ("textDocument/rename",            hh $ renameHandler h)
  , ("workspace/executeCommand",       hh $ executeCommandHandler h)

  , ("initialized",                      hh $ initializedHandler h)
  , ("workspace/didChangeConfiguration", hh $ didChangeConfigurationParamsHandler h)
  , ("textDocument/didOpen",             hh $ didOpenTextDocumentNotificationHandler h)
  , ("textDocument/didChange",           hh $ didChangeTextDocumentNotificationHandler h )
  , ("textDocument/didClose",            hh $ didCloseTextDocumentNotificationHandler h )
  , ("textDocument/didSave",             hh $ didSaveTextDocumentNotificationHandler h)
  , ("workspace/didChangeWatchedFiles",  hh $ didChangeWatchedFilesNotificationHandler h)

  , ("$/cancelRequest",                  hh $ cancelNotificationHandler h)

  -- Next is not actually a method value, just a proxy for ResponseMessage
  , ("response",                         hh $ responseHandler h)
  ]

-- ---------------------------------------------------------------------

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: forall b. (J.FromJSON b)
   => Maybe (Handler b) -> MVar LanguageContextData -> String -> B.ByteString -> IO ()
hh Nothing = \mvarDat cmd jsonStr -> do
      let msg = unwords ["haskell-lsp:no handler for.", cmd, lbs2str jsonStr]
      sendErrorLog mvarDat msg
hh (Just h) = \mvarDat cmd jsonStr -> do
      case J.eitherDecode jsonStr of
        Right req -> do
          ctx <- readMVar mvarDat
          vfs' <- getVfs (resVFS ctx) cmd jsonStr
          modifyMVar_ mvarDat (\c -> return c {resVFS = vfs'})
          h  (resLspFuncs ctx) req
        Left  err -> do
          let msg = unwords $ ["haskell-lsp:parse error.", lbs2str jsonStr, show err] ++ _ERR_MSG_URL
          sendErrorLog mvarDat msg

-- ---------------------------------------------------------------------

getVirtualFile :: MVar LanguageContextData -> J.Uri -> IO (Maybe VirtualFile)
getVirtualFile mvarDat uri = do
  ctx <- readMVar mvarDat
  return $ Map.lookup uri (resVFS ctx)

-- ---------------------------------------------------------------------

-- | Wrap all the protocol messages into a single type, for use in the language
-- handler in storing the original message
data OutMessage = ReqHover                    J.HoverRequest
                | ReqCompletion               J.CompletionRequest
                | ReqCompletionItemResolve    J.CompletionItemResolveRequest
                | ReqSignatureHelp            J.SignatureHelpRequest
                | ReqDefinition               J.DefinitionRequest
                | ReqFindReferences           J.FindReferencesRequest
                | ReqDocumentHighlights       J.DocumentHighlightsRequest
                | ReqDocumentSymbols          J.DocumentSymbolsRequest
                | ReqWorkspaceSymbols         J.WorkspaceSymbolsRequest
                | ReqCodeAction               J.CodeActionRequest
                | ReqCodeLens                 J.CodeLensRequest
                | ReqCodeLensResolve          J.CodeLensResolveRequest
                | ReqDocumentFormatting       J.DocumentFormattingRequest
                | ReqDocumentRangeFormatting  J.DocumentRangeFormattingRequest
                | ReqDocumentOnTypeFormatting J.DocumentOnTypeFormattingRequest
                | ReqRename                   J.RenameRequest
                | ReqExecuteCommand           J.ExecuteCommandRequest
                -- responses
                | RspHover                    J.HoverResponse
                | RspCompletion               J.CompletionResponse
                | RspCompletionItemResolve    J.CompletionItemResolveResponse
                | RspSignatureHelp            J.SignatureHelpResponse
                | RspDefinition               J.DefinitionResponse
                | RspFindReferences           J.FindReferencesResponse
                | RspDocumentHighlights       J.DocumentHighlightsResponse
                | RspDocumentSymbols          J.DocumentSymbolsResponse
                | RspWorkspaceSymbols         J.WorkspaceSymbolsResponse
                | RspCodeAction               J.CodeActionResponse
                | RspCodeLens                 J.CodeLensResponse
                | RspCodeLensResolve          J.CodeLensResolveResponse
                | RspDocumentFormatting       J.DocumentFormattingResponse
                | RspDocumentRangeFormatting  J.DocumentRangeFormattingResponse
                | RspDocumentOnTypeFormatting J.DocumentOnTypeFormattingResponse
                | RspRename                   J.RenameResponse
                | RspExecuteCommand           J.ExecuteCommandResponse

                -- notifications
                | NotInitialized                  J.InitializedNotification
                | NotDidChangeConfigurationParams J.DidChangeConfigurationParamsNotification
                | NotDidOpenTextDocument          J.DidOpenTextDocumentNotification
                | NotDidChangeTextDocument        J.DidChangeTextDocumentNotification
                | NotDidCloseTextDocument         J.DidCloseTextDocumentNotification
                | NotDidSaveTextDocument          J.DidSaveTextDocumentNotification
                | NotDidChangeWatchedFiles        J.DidChangeWatchedFilesNotification

                | NotCancelRequest                J.CancelNotification

                | RspFromClient                   J.BareResponseMessage
                deriving (Eq,Read,Show)

-- ---------------------------------------------------------------------
-- |
--
--
_INITIAL_RESPONSE_SEQUENCE :: Int
_INITIAL_RESPONSE_SEQUENCE = 0


-- |
--
--
_SEP_WIN :: Char
_SEP_WIN = '\\'

-- |
--
--
_SEP_UNIX :: Char
_SEP_UNIX = '/'

-- |
--
--
_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new haskell-lsp."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=xxxxxxxxxxxxxxx"
               ]


-- |
--
--
defaultLanguageContextData :: Handlers -> Options -> LspFuncs -> LanguageContextData
defaultLanguageContextData h o lf = LanguageContextData _INITIAL_RESPONSE_SEQUENCE Nothing h o BSL.putStr mempty mempty lf

-- ---------------------------------------------------------------------

handleRequest :: InitializeCallback
              -> MVar LanguageContextData -> BSL.ByteString -> BSL.ByteString -> IO ()
handleRequest dispatcherProc mvarDat contLenStr' jsonStr' = do
  {-
  Message Types we must handle are the following

  Request      | jsonrpc | id | method | params?
  Response     | jsonrpc | id |        |         | response? | error?
  Notification | jsonrpc |    | method | params?

  -}

  case J.eitherDecode jsonStr' :: Either String J.Object of
    Left  err -> do
      let msg =  unwords [ "haskell-lsp:incoming message parse error.", lbs2str contLenStr', lbs2str jsonStr', show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"
      sendErrorLog mvarDat msg

    Right o -> do
      case HM.lookup "method" o of
        Just (J.String cmd) -> handle jsonStr' (T.unpack cmd)
        Just oops -> logs $ "haskell-lsp:got strange method param, ignoring:" ++ show oops
        Nothing -> do
          logs $ "haskell-lsp:Got reply message:" ++ show jsonStr'
          handle jsonStr' "response"

  where
    helper :: J.FromJSON b => B.ByteString -> (MVar LanguageContextData -> b -> IO ())
           -> IO ()
    helper jsonStr requestHandler = case J.eitherDecode jsonStr of
      Right req -> do
        requestHandler mvarDat req
      Left  err -> do
        let msg = unwords $ ["haskell-lsp:parse error.", lbs2str contLenStr', lbs2str jsonStr', show err] ++ _ERR_MSG_URL
        sendErrorLog mvarDat msg

    -- ---------------------------------

    -- Server life cycle handlers
    handle jsonStr "initialize" = helper jsonStr (initializeRequestHandler dispatcherProc)
    handle jsonStr "shutdown"   = helper jsonStr shutdownRequestHandler
    handle _jsonStr "exit" = do
      logm $ B.pack "haskell-lsp:Got exit, exiting"
      exitSuccess

    -- ---------------------------------

    -- {"jsonrpc":"2.0","method":"$/setTraceNotification","params":{"value":"off"}}
    handle jsonStr "$/setTraceNotification" = helper jsonStr h
      where
        h :: MVar LanguageContextData -> J.TraceNotification -> IO ()
        h _ _ = do
          logm "haskell-lsp:Got setTraceNotification, ignoring"
          sendErrorLog mvarDat "haskell-lsp:Got setTraceNotification, ignoring"

    -- capability based handlers
    handle jsonStr cmd = do
      ctx <- readMVar mvarDat
      let h = resHandlers ctx
      case Map.lookup cmd (handlerMap h) of
        Just f -> f mvarDat cmd jsonStr
        Nothing -> do
          let msg = unwords ["haskell-lsp:unknown message received:method='" ++ cmd ++ "',", lbs2str contLenStr', lbs2str jsonStr]
          sendErrorLog mvarDat msg

-- ---------------------------------------------------------------------

makeResponseMessage :: J.LspIdRsp -> a -> J.ResponseMessage a
makeResponseMessage origId result = J.ResponseMessage "2.0" origId (Just result) Nothing

makeResponseError :: J.LspIdRsp -> J.ResponseError -> J.ResponseMessage ()
makeResponseError origId err = J.ResponseMessage "2.0" origId Nothing (Just err)

-- ---------------------------------------------------------------------
-- |
--
sendEvent :: MVar LanguageContextData -> BSL.ByteString -> IO ()
sendEvent mvarCtx str = sendResponse mvarCtx str

-- |
--
sendResponse :: MVar LanguageContextData -> BSL.ByteString -> IO ()
sendResponse mvarCtx str = do
  ctx <- readMVar mvarCtx
  resSendResponse ctx str


-- ---------------------------------------------------------------------
-- |
--
--
sendErrorResponse :: MVar LanguageContextData -> J.LspIdRsp -> String -> IO ()
sendErrorResponse mv origId msg = sendErrorResponseS (sendEvent mv) origId J.InternalError msg

sendErrorResponseS :: (B.ByteString -> IO ()) -> J.LspIdRsp -> J.ErrorCode -> String -> IO ()
sendErrorResponseS sf origId err msg = do
  sf $ J.encode (J.ResponseMessage "2.0" origId Nothing
                         (Just $ J.ResponseError err msg Nothing) :: J.ErrorResponse)

sendErrorLog :: MVar LanguageContextData -> String -> IO ()
sendErrorLog mv msg = sendErrorLogS (sendEvent mv) msg

sendErrorLogS :: (B.ByteString -> IO ()) -> String -> IO ()
sendErrorLogS sf msg =
  sf $ J.encode (J.defLogMessage J.MtError msg)

-- sendErrorShow :: String -> IO ()
-- sendErrorShow msg = sendErrorShowS sendEvent msg

sendErrorShowS :: (B.ByteString -> IO ()) -> String -> IO ()
sendErrorShowS sf msg =
  sf $ J.encode (J.defShowMessage J.MtError msg)

-- ---------------------------------------------------------------------

defaultErrorHandlers :: (Show a) => MVar LanguageContextData -> J.LspIdRsp -> a -> [E.Handler ()]
defaultErrorHandlers mvarDat origId req = [ E.Handler someExcept ]
  where
    someExcept (e :: E.SomeException) = do
      let msg = unwords ["request error.", show req, show e]
      sendErrorResponse mvarDat origId msg
      sendErrorLog mvarDat msg

-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler :: InitializeCallback
                         -> MVar LanguageContextData
                         -> J.InitializeRequest -> IO ()
initializeRequestHandler _dispatcherProc _mvarCtx (J.RequestMessage _ _origId _ Nothing) = do
  logs "haskell-lsp:initializeRequestHandler: no params in message, ignoring"
initializeRequestHandler dispatcherProc mvarCtx req@(J.RequestMessage _ origId _ (Just params)) =
  flip E.catches (defaultErrorHandlers mvarCtx (J.responseId origId) req) $ do

    ctx0 <- readMVar mvarCtx

    modifyMVar_ mvarCtx (\c -> return c { resRootPath = J._rootPath params})
    case J._rootPath params of
      Nothing -> return ()
      Just dir -> do
        logs $ "haskell-lsp:initializeRequestHandler: setting current dir to project root:" ++ dir
        unless (null dir) $ setCurrentDirectory dir

    let
      getCapabilities :: J.InitializeParams -> C.ClientCapabilities
      getCapabilities (J.InitializeParams _ _ _ _ c _) = c

    -- Launch the given process once the project root directory has been set
    let lspFuncs = LspFuncs (getCapabilities params)
                            (resSendResponse ctx0)
                            (getVirtualFile mvarCtx)
                            (publishDiagnostics mvarCtx)
    let ctx = ctx0 { resLspFuncs = lspFuncs }
    modifyMVar_ mvarCtx (\_ -> return ctx)

    initializationResult <- dispatcherProc lspFuncs

    case initializationResult of
      Just errResp -> do
        sendResponse mvarCtx $ J.encode $ makeResponseError (J.responseId origId) errResp

      Nothing -> do

        let
          h = resHandlers ctx
          o = resOptions  ctx

          supported (Just _) = Just True
          supported Nothing   = Nothing

          capa =
            J.InitializeResponseCapabilitiesInner
              { J._textDocumentSync                 = textDocumentSync o
              , J._hoverProvider                    = supported (hoverHandler h)
              , J._completionProvider               = completionProvider o
              , J._signatureHelpProvider            = signatureHelpProvider o
              , J._definitionProvider               = supported (definitionHandler h)
              , J._referencesProvider               = supported (referencesHandler h)
              , J._documentHighlightProvider        = supported (documentHighlightHandler h)
              , J._documentSymbolProvider           = supported (documentSymbolHandler h)
              , J._workspaceSymbolProvider          = supported (workspaceSymbolHandler h)
              , J._codeActionProvider               = supported (codeActionHandler h)
              , J._codeLensProvider                 = codeLensProvider o
              , J._documentFormattingProvider       = supported (documentFormattingHandler h)
              , J._documentRangeFormattingProvider  = supported (documentRangeFormattingHandler h)
              , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider o
              , J._renameProvider                   = supported (renameHandler h)
              , J._documentLinkProvider             = documentLinkProvider o
              , J._executeCommandProvider           = executeCommandProvider o
              -- TODO: Add something for experimental
              , J._experimental                     = (Nothing :: Maybe J.Value)
              }

          -- TODO: wrap this up into a fn to create a response message
          res  = J.ResponseMessage "2.0" (J.responseId origId) (Just $ J.InitializeResponseCapabilities capa) Nothing

        sendResponse mvarCtx $ J.encode res

-- |
--
shutdownRequestHandler :: MVar LanguageContextData -> J.ShutdownRequest -> IO ()
shutdownRequestHandler mvarCtx req@(J.RequestMessage _ origId _ _) =
  flip E.catches (defaultErrorHandlers mvarCtx (J.responseId origId) req) $ do
  let res  = makeResponseMessage (J.responseId origId) ("ok"::String)

  sendResponse mvarCtx $ J.encode res

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publist the total to the client.
publishDiagnostics :: MVar LanguageContextData -> PublishDiagnosticsFunc
publishDiagnostics mvarDat uri mversion diags = do
  ctx <- readMVar mvarDat
  let ds = updateDiagnostics (resDiagnostics ctx) uri mversion diags
  modifyMVar_ mvarDat (\c -> return c {resDiagnostics = ds})
  let mdp = getDiagnosticParamsFor ds uri
  case mdp of
    Nothing -> return ()
    Just params -> do
      (resSendResponse ctx) $ J.encode $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just params)

-- |=====================================================================
--
--  utility


-- |
--  Logger
--
setupLogger :: FilePath -> Priority -> IO ()
setupLogger logFile level = do

  logStream <- openFile logFile AppendMode
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel level

