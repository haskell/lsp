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
import           Control.Lens ( (<&>), (^.) )
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Foldable
import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Constant
import           Language.Haskell.LSP.Messages
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

-- | A function to send a message to the client
type SendFunc = forall a. (J.ToJSON a => a -> IO ())

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resRootPath            :: !(Maybe FilePath)
  , resHandlers            :: !Handlers
  , resOptions             :: !Options
  , resSendResponse        :: !SendFunc
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
    , referencesHandler              :: !(Maybe (Handler J.ReferencesRequest))
    , documentHighlightHandler       :: !(Maybe (Handler J.DocumentHighlightRequest))
    , documentSymbolHandler          :: !(Maybe (Handler J.DocumentSymbolRequest))
    , workspaceSymbolHandler         :: !(Maybe (Handler J.WorkspaceSymbolRequest))
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
    , didChangeConfigurationParamsHandler      :: !(Maybe (Handler J.DidChangeConfigurationNotification))
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
nop :: a -> b -> IO a
nop = const . return

helper :: J.FromJSON a
       => (MVar LanguageContextData -> a       -> IO ())
       -> (MVar LanguageContextData -> J.Value -> IO ())
helper requestHandler mvarDat json =
  case J.fromJSON json of
    J.Success req -> requestHandler mvarDat req
    J.Error err -> do
      let msg = T.pack . unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
      sendErrorLog mvarDat msg

handlerMap :: InitializeCallback
           -> Handlers -> J.ClientMethod -> (MVar LanguageContextData -> J.Value -> IO ())
-- General
handlerMap i _ J.Initialize                      = helper (initializeRequestHandler i)
handlerMap _ h J.Initialized                     = hh nop $ initializedHandler h
handlerMap _ _ J.Shutdown                        = helper shutdownRequestHandler
handlerMap _ _ J.Exit                            = \_ _ -> do
  logm $ B.pack "haskell-lsp:Got exit, exiting"
  exitSuccess
handlerMap _ h J.CancelRequest                   = hh nop $ cancelNotificationHandler h
-- Workspace
handlerMap _ h J.WorkspaceDidChangeConfiguration = hh nop $ didChangeConfigurationParamsHandler h
handlerMap _ h J.WorkspaceDidChangeWatchedFiles  = hh nop $ didChangeWatchedFilesNotificationHandler h
handlerMap _ h J.WorkspaceSymbol                 = hh nop $ workspaceSymbolHandler h
handlerMap _ h J.WorkspaceExecuteCommand         = hh nop $ executeCommandHandler h
-- Document
handlerMap _ h J.TextDocumentDidOpen             = hh openVFS $ didOpenTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidChange           = hh changeVFS $ didChangeTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSave            = hh nop $ willSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSaveWaitUntil   = hh nop $ willSaveWaitUntilTextDocHandler h
handlerMap _ h J.TextDocumentDidSave             = hh nop $ didSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidClose            = hh closeVFS $ didCloseTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentCompletion          = hh nop $ completionHandler h
handlerMap _ h J.CompletionItemResolve           = hh nop $ completionResolveHandler h
handlerMap _ h J.TextDocumentHover               = hh nop $ hoverHandler h
handlerMap _ h J.TextDocumentSignatureHelp       = hh nop $ signatureHelpHandler h
handlerMap _ h J.TextDocumentReferences          = hh nop $ referencesHandler h
handlerMap _ h J.TextDocumentDocumentHighlight   = hh nop $ documentHighlightHandler h
handlerMap _ h J.TextDocumentDocumentSymbol      = hh nop $ documentSymbolHandler h
handlerMap _ h J.TextDocumentFormatting          = hh nop $ documentFormattingHandler h
handlerMap _ h J.TextDocumentRangeFormatting     = hh nop $ documentRangeFormattingHandler h
handlerMap _ h J.TextDocumentOnTypeFormatting    = hh nop $ documentTypeFormattingHandler h
handlerMap _ h J.TextDocumentDefinition          = hh nop $ definitionHandler h
handlerMap _ h J.TextDocumentCodeAction          = hh nop $ codeActionHandler h
handlerMap _ h J.TextDocumentCodeLens            = hh nop $ codeLensHandler h
handlerMap _ h J.CodeLensResolve                 = hh nop $ codeLensResolveHandler h
handlerMap _ h J.TextDocumentDocumentLink        = hh nop $ documentLinkHandler h
handlerMap _ h J.DocumentLinkResolve             = hh nop $ documentLinkResolveHandler h
handlerMap _ h J.TextDocumentRename              = hh nop $ renameHandler h
handlerMap _ _ (J.Misc x)   = helper f
  where f ::  MVar LanguageContextData -> J.TraceNotification -> IO ()
        f mvarDat _ = do
          let msg = "haskell-lsp:Got " ++ T.unpack x ++ " ignoring"
          logm (B.pack msg)
          sendErrorLog mvarDat (T.pack msg)

-- ---------------------------------------------------------------------

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: forall b. (J.FromJSON b)
   => (VFS -> b -> IO VFS) -> Maybe (Handler b) -> MVar LanguageContextData -> J.Value -> IO ()
hh _ Nothing = \mvarDat json -> do
      let msg = T.pack $ unwords ["haskell-lsp:no handler for.", show json]
      sendErrorLog mvarDat msg
hh getVfs (Just h) = \mvarDat json -> do
      case J.fromJSON json of
        J.Success req -> do
          ctx <- readMVar mvarDat
          vfs' <- getVfs (resVFS ctx) req
          modifyMVar_ mvarDat (\c -> return c {resVFS = vfs'})
          h  (resLspFuncs ctx) req
        J.Error  err -> do
          let msg = T.pack $ unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
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
                | ReqFindReferences           J.ReferencesRequest
                | ReqDocumentHighlights       J.DocumentHighlightRequest
                | ReqDocumentSymbols          J.DocumentSymbolRequest
                | ReqWorkspaceSymbols         J.WorkspaceSymbolRequest
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
                | RspFindReferences           J.ReferencesResponse
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
                | NotDidChangeConfigurationParams J.DidChangeConfigurationNotification
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
defaultLanguageContextData h o lf = LanguageContextData _INITIAL_RESPONSE_SEQUENCE Nothing h o (BSL.putStr . J.encode) mempty mempty lf

-- ---------------------------------------------------------------------

handleRequest :: InitializeCallback
              -> MVar LanguageContextData -> BSL.ByteString -> BSL.ByteString -> IO ()
handleRequest dispatcherProc mvarDat contLenStr jsonStr = do
  {-
  Message Types we must handle are the following

  Request      | jsonrpc | id | method | params?
  Response     | jsonrpc | id |        |         | response? | error?
  Notification | jsonrpc |    | method | params?

  -}

  case J.eitherDecode jsonStr :: Either String J.Object of
    Left  err -> do
      let msg =  T.pack $ unwords [ "haskell-lsp:incoming message parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"
      sendErrorLog mvarDat msg

    Right o -> do
      case HM.lookup "method" o of
        Just cmd@(J.String s) -> case J.fromJSON cmd of
                                   J.Success m -> handle (J.Object o) m
                                   J.Error _ -> do
                                     let msg = T.pack $ unwords ["haskell-lsp:unknown message received:method='" ++ T.unpack s ++ "',", lbs2str contLenStr, lbs2str jsonStr]
                                     sendErrorLog mvarDat msg
        Just oops -> logs $ "haskell-lsp:got strange method param, ignoring:" ++ show oops
        Nothing -> do
          logs $ "haskell-lsp:Got reply message:" ++ show jsonStr
          handleResponse (J.Object o)

  where
    handleResponse json = do
      ctx <- readMVar mvarDat
      case responseHandler $ resHandlers ctx of
        Nothing -> sendErrorLog mvarDat $ T.pack $ "haskell-lsp: responseHandler is not defined, ignoring response " ++ lbs2str jsonStr
        Just h -> case J.fromJSON json of
          J.Success res -> h (resLspFuncs ctx) res
          J.Error err -> let msg = T.pack $ unwords $ ["haskell-lsp:response parse error.", lbs2str jsonStr, show err] ++ _ERR_MSG_URL
                           in sendErrorLog mvarDat msg
    -- capability based handlers
    handle json cmd = do
      ctx <- readMVar mvarDat
      let h = resHandlers ctx
      handlerMap dispatcherProc h cmd mvarDat json

-- ---------------------------------------------------------------------

makeResponseMessage :: J.LspIdRsp -> a -> J.ResponseMessage a
makeResponseMessage origId result = J.ResponseMessage "2.0" origId (Just result) Nothing

makeResponseError :: J.LspIdRsp -> J.ResponseError -> J.ResponseMessage ()
makeResponseError origId err = J.ResponseMessage "2.0" origId Nothing (Just err)

-- ---------------------------------------------------------------------
-- |
--
sendEvent :: J.ToJSON a => MVar LanguageContextData -> a -> IO ()
sendEvent mvarCtx str = sendResponse mvarCtx str

-- |
--
sendResponse :: J.ToJSON a => MVar LanguageContextData -> a -> IO ()
sendResponse mvarCtx str = do
  ctx <- readMVar mvarCtx
  resSendResponse ctx str


-- ---------------------------------------------------------------------
-- |
--
--
sendErrorResponse :: MVar LanguageContextData -> J.LspIdRsp -> Text -> IO ()
sendErrorResponse mv origId msg = sendErrorResponseS (sendEvent mv) origId J.InternalError msg

sendErrorResponseS ::  SendFunc -> J.LspIdRsp -> J.ErrorCode -> Text -> IO ()
sendErrorResponseS sf origId err msg = do
  sf $ (J.ResponseMessage "2.0" origId Nothing
                (Just $ J.ResponseError err msg Nothing) :: J.ErrorResponse)

sendErrorLog :: MVar LanguageContextData -> Text -> IO ()
sendErrorLog mv msg = sendErrorLogS (sendEvent mv) msg

sendErrorLogS :: SendFunc -> Text -> IO ()
sendErrorLogS sf msg =
  sf $ fmServerLogMessageNotification J.MtError msg

-- sendErrorShow :: String -> IO ()
-- sendErrorShow msg = sendErrorShowS sendEvent msg

sendErrorShowS :: SendFunc -> Text -> IO ()
sendErrorShowS sf msg =
  sf $ fmServerShowMessageNotification J.MtError msg

-- ---------------------------------------------------------------------

defaultErrorHandlers :: (Show a) => MVar LanguageContextData -> J.LspIdRsp -> a -> [E.Handler ()]
defaultErrorHandlers mvarDat origId req = [ E.Handler someExcept ]
  where
    someExcept (e :: E.SomeException) = do
      let msg = T.pack $ unwords ["request error.", show req, show e]
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
initializeRequestHandler dispatcherProc mvarCtx req@(J.RequestMessage _ origId _ params) =
  flip E.catches (defaultErrorHandlers mvarCtx (J.responseId origId) req) $ do

    ctx0 <- readMVar mvarCtx

    let rootDir = getFirst $ foldMap First [ params ^. J.rootUri  >>= J.uriToFilePath
                                           , params ^. J.rootPath <&> T.unpack ]

    modifyMVar_ mvarCtx (\c -> return c { resRootPath = rootDir })
    case rootDir of
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
        sendResponse mvarCtx $ makeResponseError (J.responseId origId) errResp

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

        sendResponse mvarCtx res

-- |
--
shutdownRequestHandler :: MVar LanguageContextData -> J.ShutdownRequest -> IO ()
shutdownRequestHandler mvarCtx req@(J.RequestMessage _ origId _ _) =
  flip E.catches (defaultErrorHandlers mvarCtx (J.responseId origId) req) $ do
  let res  = makeResponseMessage (J.responseId origId) ("ok"::String)

  sendResponse mvarCtx res

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
      (resSendResponse ctx) $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics (Just params)

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

