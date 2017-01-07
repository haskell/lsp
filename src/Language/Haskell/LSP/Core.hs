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
, Handlers(..)
, Options(..)
, OutMessage(..)
, defaultLanguageContextData
, initializeRequestHandler
, makeResponseMessage
, setupLogger
, sendErrorResponseS
, sendErrorLogS
, sendErrorShowS
) where

import Language.Haskell.LSP.Constant
import Language.Haskell.LSP.Utility
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

import Data.Default
import Data.Monoid
import System.IO
import System.Directory
import System.Exit
import System.Log.Logger
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Control.Exception as E
import qualified Data.Map as MAP
import Control.Concurrent
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData a =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resRootPath            :: !(Maybe FilePath)
  , resHandlers            :: !(Handlers a)
  , resOptions             :: !Options
  , resSendResponse        :: !(BSL.ByteString -> IO ())
  , resData                :: !a
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
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing

-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
type Handler a b = a -> (BSL.ByteString -> IO ()) -> b -> IO ()

-- | Callbacks from the language server to the language handler
data Handlers a =
  Handlers
    {
    -- Capability-advertised handlers
      hoverHandler                   :: !(Maybe (Handler a J.HoverRequest))
    , completionHandler              :: !(Maybe (Handler a J.CompletionRequest))
    , completionResolveHandler       :: !(Maybe (Handler a J.CompletionItemResolveRequest))
    , signatureHelpHandler           :: !(Maybe (Handler a J.SignatureHelpRequest))
    , definitionHandler              :: !(Maybe (Handler a J.DefinitionRequest))
    , referencesHandler              :: !(Maybe (Handler a J.FindReferencesRequest))
    , documentHighlightHandler       :: !(Maybe (Handler a J.DocumentHighlightsRequest))
    , documentSymbolHandler          :: !(Maybe (Handler a J.DocumentSymbolsRequest))
    , workspaceSymbolHandler         :: !(Maybe (Handler a J.WorkspaceSymbolsRequest))
    , codeActionHandler              :: !(Maybe (Handler a J.CodeActionRequest))
    , codeLensHandler                :: !(Maybe (Handler a J.CodeLensRequest))
    , codeLensResolveHandler         :: !(Maybe (Handler a J.CodeLensResolveRequest))
    , documentFormattingHandler      :: !(Maybe (Handler a J.DocumentFormattingRequest))
    , documentRangeFormattingHandler :: !(Maybe (Handler a J.DocumentRangeFormattingRequest))
    , documentTypeFormattingHandler  :: !(Maybe (Handler a J.DocumentOnTypeFormattingRequest))
    , renameHandler                  :: !(Maybe (Handler a J.RenameRequest))

    -- Notifications from the client
    , didChangeConfigurationParamsHandler      :: !(Maybe (Handler a J.DidChangeConfigurationParamsNotification))
    , didOpenTextDocumentNotificationHandler   :: !(Maybe (Handler a J.DidOpenTextDocumentNotification))
    , didChangeTextDocumentNotificationHandler :: !(Maybe (Handler a J.DidChangeTextDocumentNotification))
    , didCloseTextDocumentNotificationHandler  :: !(Maybe (Handler a J.DidCloseTextDocumentNotification))
    , didSaveTextDocumentNotificationHandler   :: !(Maybe (Handler a J.DidSaveTextDocumentNotification))
    , didChangeWatchedFilesNotificationHandler :: !(Maybe (Handler a J.DidChangeWatchedFilesNotification))

    , cancelNotificationHandler                :: !(Maybe (Handler a J.CancelNotification))

    -- Responses to Request messages originated from the server
    , responseHandler                          :: !(Maybe (Handler a J.BareResponseMessage))
    }

instance Default (Handlers a) where
  def = Handlers Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------

handlerMap :: Handlers a
           -> MAP.Map String (MVar (LanguageContextData a) -> String -> B.ByteString -> IO ())
handlerMap h = MAP.fromList
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

-- | Adapter from the handlers exposed to the library users and the internal message loop
hh :: forall a b. (J.FromJSON b)
   => Maybe (Handler a b) -> MVar (LanguageContextData a) -> String -> B.ByteString -> IO ()
hh Nothing = \_mvarDat cmd jsonStr -> do
      let msg = unwords ["haskell-lsp:no handler for.", cmd, lbs2str jsonStr]
      sendErrorLog msg
hh (Just h) = \mvarDat _cmd jsonStr -> do
      case J.eitherDecode jsonStr of
        Right req -> do
          ctx <- readMVar mvarDat
          h (resData ctx) (resSendResponse ctx) req
        Left  err -> do
          let msg = unwords $ ["haskell-lsp:parse error.", lbs2str jsonStr, show err] ++ _ERR_MSG_URL
          sendErrorLog msg

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

                -- notifications
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
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"

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
defaultLanguageContextData :: a -> Handlers a -> Options -> LanguageContextData a
defaultLanguageContextData a h o = LanguageContextData _INITIAL_RESPONSE_SEQUENCE Nothing h o BSL.putStr a

-- ---------------------------------------------------------------------

handleRequest :: forall a. IO () -> MVar (LanguageContextData a) -> BSL.ByteString -> BSL.ByteString -> IO ()
handleRequest dispatcherProc mvarDat contLenStr' jsonStr' = do
  {-
  Message Types we must handle are the following

  Request      | jsonrpc | id | method | params?
  Response     | jsonrpc | id |        |         | response? | error?
  Notification | jsonrpc |    | method | params?

  -}

  case J.eitherDecode jsonStr' :: Either String J.Object of
    Left  err -> do
      let msg =  unwords [ "request request parse error.", lbs2str contLenStr', lbs2str jsonStr', show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"
      sendErrorLog msg

    Right o -> do
      case HM.lookup "method" o of
        Just (J.String cmd) -> handle jsonStr' (T.unpack cmd)
        Just oops -> logs $ "got strange method param, ignoring:" ++ show oops
        Nothing -> do
          logs $ "Got reply message:" ++ show jsonStr'
          handle jsonStr' "response"

  where
    helper :: J.FromJSON b => B.ByteString -> (MVar (LanguageContextData a) -> b -> IO ())
           -> IO ()
    helper jsonStr requestHandler = case J.eitherDecode jsonStr of
      Right req -> do
        requestHandler mvarDat req
      Left  err -> do
        let msg = unwords $ ["haskell-lsp:parse error.", lbs2str contLenStr', lbs2str jsonStr', show err] ++ _ERR_MSG_URL
        sendErrorLog msg

    -- ---------------------------------

    -- Server life cycle handlers
    handle jsonStr "initialize" = helper jsonStr (initializeRequestHandler dispatcherProc)
    handle jsonStr "shutdown"   = helper jsonStr shutdownRequestHandler
    handle _jsonStr "exit" = do
      logm $ B.pack "Got exit, exiting"
      exitSuccess

    -- ---------------------------------

    -- {"jsonrpc":"2.0","method":"$/setTraceNotification","params":{"value":"off"}}
    handle jsonStr "$/setTraceNotification" = helper jsonStr h
      where
        h :: MVar (LanguageContextData a) -> J.TraceNotification -> IO ()
        h _ _ = do
          logm "Got setTraceNotification, ignoring"
          sendErrorLog "Got setTraceNotification, ignoring"

    -- capability based handlers
    handle jsonStr cmd = do
      ctx <- readMVar mvarDat
      let h = resHandlers ctx
      case MAP.lookup cmd (handlerMap h) of
        Just f -> f mvarDat cmd jsonStr
        Nothing -> do
          let msg = unwords ["unknown request command.", cmd, lbs2str contLenStr', lbs2str jsonStr]
          sendErrorLog msg

-- ---------------------------------------------------------------------

makeResponseMessage :: Int -> a -> J.ResponseMessage a
makeResponseMessage origId result = J.ResponseMessage "2.0" origId (Just result) Nothing

-- ---------------------------------------------------------------------
-- |
--
sendEvent :: BSL.ByteString -> IO ()
sendEvent str = sendResponseInternal str

-- |
--
sendResponse2 :: MVar (LanguageContextData a) -> BSL.ByteString -> IO ()
sendResponse2 mvarCtx str = do
  ctx <- readMVar mvarCtx
  resSendResponse ctx str

-- |
--
sendResponseInternal :: BSL.ByteString -> IO ()
sendResponseInternal str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout
  logm $ B.pack "<---" <> str

-- |
--
--
sendErrorResponse :: Int -> String -> IO ()
sendErrorResponse origId msg = sendErrorResponseS sendEvent origId J.InternalError msg

sendErrorResponseS :: (B.ByteString -> IO ()) -> Int -> J.ErrorCode -> String -> IO ()
sendErrorResponseS sf origId err msg = do
  sf $ J.encode (J.ResponseMessage "2.0" origId Nothing
                         (Just $ J.ResponseError err msg Nothing) :: J.ErrorResponse)

sendErrorLog :: String -> IO ()
sendErrorLog msg = sendErrorLogS sendEvent msg

sendErrorLogS :: (B.ByteString -> IO ()) -> String -> IO ()
sendErrorLogS sf msg =
  sf $ J.encode (J.defLogMessage J.MtError msg)

-- sendErrorShow :: String -> IO ()
-- sendErrorShow msg = sendErrorShowS sendEvent msg

sendErrorShowS :: (B.ByteString -> IO ()) -> String -> IO ()
sendErrorShowS sf msg =
  sf $ J.encode (J.defShowMessage J.MtError msg)

-- ---------------------------------------------------------------------

defaultErrorHandlers :: (Show a) => Int -> a -> [E.Handler ()]
defaultErrorHandlers origId req = [ E.Handler someExcept ]
  where
    someExcept (e :: E.SomeException) = do
      let msg = unwords ["request error.", show req, show e]
      sendErrorResponse origId msg
      sendErrorLog msg

-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler :: IO () -> MVar (LanguageContextData a) -> J.InitializeRequest -> IO ()
initializeRequestHandler dispatcherProc mvarCtx req@(J.RequestMessage _ origId _ mp) =
  flip E.catches (defaultErrorHandlers origId req) $ do

    ctx <- readMVar mvarCtx

    case mp of
      Nothing -> return ()
      Just params -> do
        modifyMVar_ mvarCtx (\c -> return c { resRootPath = J.rootPathInitializeRequestArguments params})
        case J.rootPathInitializeRequestArguments params of
          Nothing -> return ()
          Just dir -> do
            logs $ "initializeRequestHandler: setting current dir to project root:" ++ dir
            setCurrentDirectory dir

    -- Launch the given process once the project root directory has been set
    logs "initializeRequestHandler: calling dispatcherProc"
    dispatcherProc

    let
      h = resHandlers ctx
      o = resOptions  ctx

      supported (Just _) = Just True
      supported Nothing   = Nothing

      capa =
        J.InitializeResponseCapabilitiesInner
          { J.textDocumentSync                 = textDocumentSync o
          , J.hoverProvider                    = supported (hoverHandler h)
          , J.completionProvider               = completionProvider o
          , J.signatureHelpProvider            = signatureHelpProvider o
          , J.definitionProvider               = supported (definitionHandler h)
          , J.referencesProvider               = supported (referencesHandler h)
          , J.documentHighlightProvider        = supported (documentHighlightHandler h)
          , J.documentSymbolProvider           = supported (documentSymbolHandler h)
          , J.workspaceSymbolProvider          = supported (workspaceSymbolHandler h)
          , J.codeActionProvider               = supported (codeActionHandler h)
          , J.codeLensProvider                 = codeLensProvider o
          , J.documentFormattingProvider       = supported (documentFormattingHandler h)
          , J.documentRangeFormattingProvider  = supported (documentRangeFormattingHandler h)
          , J.documentOnTypeFormattingProvider = documentOnTypeFormattingProvider o
          , J.renameProvider                   = supported (renameHandler h)
          }

      -- TODO: wrap this up into a fn to create a response message
      res  = J.ResponseMessage "2.0" origId (Just $ J.InitializeResponseCapabilities capa) Nothing

    sendResponse2 mvarCtx $ J.encode res

    -- ++AZ++ experimenting
    -- let
    --   ais = Just [J.MessageActionItem "action item 1", J.MessageActionItem "action item 2"]
    --   p   = J.ShowMessageRequestParams J.MtWarning "playing with ShowMessageRequest" ais
    --   smr = J.RequestMessage "2.0" 1 "window/showMessageRequest" (Just p)
    -- sendEvent $ J.encode smr


-- |
--
shutdownRequestHandler :: MVar (LanguageContextData a) -> J.ShutdownRequest -> IO ()
shutdownRequestHandler mvarCtx req@(J.RequestMessage _ origId _ _) =
  flip E.catches (defaultErrorHandlers origId req) $ do
  let res  = makeResponseMessage origId ("ok"::String)

  sendResponse2 mvarCtx $ J.encode res



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

