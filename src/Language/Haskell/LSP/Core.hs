{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Core (
    handleMessage
  , LanguageContextData(..)
  , Handler
  , InitializeCallbacks(..)
  , LspFuncs(..)
  , Progress(..)
  , ProgressCancellable(..)
  , ProgressCancelledException
  , SendFunc
  , Handlers(..)
  , Options(..)
  , defaultLanguageContextData
  , makeResponseMessage
  , makeResponseError
  , setupLogger
  , sendErrorResponseS
  , sendErrorLogS
  , sendErrorShowS
  , reverseSortEdit
  ) where

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens ( (<&>), (^.) )
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Constant
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types.Capabilities    as C
import qualified Language.Haskell.LSP.Types                 as J
import qualified Language.Haskell.LSP.Types.Lens            as J
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
type SendFunc = FromServerMessage -> IO ()

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData config =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resHandlers            :: !Handlers
  , resOptions             :: !Options
  , resSendResponse        :: !SendFunc
  , resVFS                 :: !VFS
  , reverseMap             :: !(Map.Map FilePath FilePath)
  , resDiagnostics         :: !DiagnosticStore
  , resConfig              :: !(Maybe config)
  , resLspId               :: !(TVar Int)
  , resLspFuncs            :: LspFuncs config -- NOTE: Cannot be strict, lazy initialization
  , resCaptureFile         :: !(Maybe FilePath)
  , resWorkspaceFolders    :: ![J.WorkspaceFolder]
  , resProgressData        :: !ProgressData
  }

data ProgressData = ProgressData { progressNextId :: !Int
                                 , progressCancel :: !(Map.Map Text (IO ())) }

-- ---------------------------------------------------------------------

-- | Language Server Protocol options supported by the given language server.
-- These are automatically turned into capabilities reported to the client
-- during initialization.
data Options =
  Options
    { textDocumentSync                 :: Maybe J.TextDocumentSyncOptions
    , completionProvider               :: Maybe J.CompletionOptions
    , signatureHelpProvider            :: Maybe J.SignatureHelpOptions
    , typeDefinitionProvider           :: Maybe J.GotoOptions
    , implementationProvider           :: Maybe J.GotoOptions
    , codeActionProvider               :: Maybe J.CodeActionOptions
    , codeLensProvider                 :: Maybe J.CodeLensOptions
    , documentOnTypeFormattingProvider :: Maybe J.DocumentOnTypeFormattingOptions
    , documentLinkProvider             :: Maybe J.DocumentLinkOptions
    , colorProvider                    :: Maybe J.ColorOptions
    , foldingRangeProvider             :: Maybe J.FoldingRangeOptions
    , executeCommandProvider           :: Maybe J.ExecuteCommandOptions
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing

-- | A function to publish diagnostics. It aggregates all diagnostics pertaining
-- to a particular version of a document, by source, and sends a
-- 'textDocument/publishDiagnostics' notification with the total (limited by the
-- first parameter) whenever it is updated.
type PublishDiagnosticsFunc = Int -- Max number of diagnostics to send
                            -> J.NormalizedUri -> J.TextDocumentVersion -> DiagnosticsBySource -> IO ()

-- | A function to remove all diagnostics from a particular source, and send the updates to the client.
type FlushDiagnosticsBySourceFunc = Int -- Max number of diagnostics to send
                                  -> Maybe J.DiagnosticSource -> IO ()

-- | A package indicating the perecentage of progress complete and a
-- an optional message to go with it during a 'withProgress'
--
-- @since 0.10.0.0
data Progress = Progress (Maybe Double) (Maybe Text)

-- | Thrown if the user cancels a 'Cancellable' 'withProgress'/'withIndefiniteProgress'/ session
--
-- @since 0.11.0.0
data ProgressCancelledException = ProgressCancelledException
  deriving Show
instance E.Exception ProgressCancelledException

-- | Whether or not the user should be able to cancel a 'withProgress'/'withIndefiniteProgress'
-- session
--
-- @since 0.11.0.0
data ProgressCancellable = Cancellable | NotCancellable

-- | Returned to the server on startup, providing ways to interact with the client.
data LspFuncs c =
  LspFuncs
    { clientCapabilities           :: !C.ClientCapabilities
    , config                       :: !(IO (Maybe c))
      -- ^ Derived from the DidChangeConfigurationNotification message via a
      -- server-provided function.
    , sendFunc                     :: !SendFunc
    , getVirtualFileFunc           :: !(J.NormalizedUri -> IO (Maybe VirtualFile))
    , persistVirtualFileFunc       :: !(J.NormalizedUri -> IO FilePath)
    , reverseFileMapFunc           :: !(IO (FilePath -> FilePath))
    , publishDiagnosticsFunc       :: !PublishDiagnosticsFunc
    , flushDiagnosticsBySourceFunc :: !FlushDiagnosticsBySourceFunc
    , getNextReqId                 :: !(IO J.LspId)
    , rootPath                     :: !(Maybe FilePath)
    , getWorkspaceFolders          :: !(IO (Maybe [J.WorkspaceFolder]))
    , withProgress                 :: !(forall a . Text -> ProgressCancellable
                                        -> ((Progress -> IO ()) -> IO a) -> IO a)
      -- ^ Wrapper for reporting progress to the client during a long running
      -- task.
      -- 'withProgress' @title cancellable f@ starts a new progress reporting
      -- session, and finishes it once f is completed.
      -- f is provided with an update function that allows it to report on
      -- the progress during the session.
      -- If @cancellable@ is 'Cancellable', @f@ will be thrown a
      -- 'ProgressCancelledException' if the user cancels the action in
      -- progress.
      --
      -- @since 0.10.0.0
    , withIndefiniteProgress       :: !(forall a . Text -> ProgressCancellable
                                        -> IO a -> IO a)
    -- ^ Same as 'withProgress', but for processes that do not report the
    -- precentage complete.
    --
    -- @since 0.10.0.0
    }

-- | Contains all the callbacks to use for initialized the language server.
-- it is parameterized over a config type variable representing the type for the
-- specific configuration data the language server needs to use.
data InitializeCallbacks config =
  InitializeCallbacks
    { onInitialConfiguration :: J.InitializeRequest -> Either T.Text config
      -- ^ Invoked on the first message from the language client, containg the client configuration
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onConfigurationChange :: J.DidChangeConfigurationNotification-> Either T.Text config
      -- ^ Invoked whenever the clients sends a message with a changed client configuration.
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onStartup :: LspFuncs config -> IO (Maybe J.ResponseError)
      -- ^ Once the initial configuration has been received, this callback will be invoked to offer
      -- the language server implementation the chance to create any processes or start new threads
      -- that may be necesary for the server lifecycle.
    }

-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
type Handler b =  b -> IO ()

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
    , typeDefinitionHandler          :: !(Maybe (Handler J.TypeDefinitionRequest))
    , implementationHandler          :: !(Maybe (Handler J.ImplementationRequest))
    , referencesHandler              :: !(Maybe (Handler J.ReferencesRequest))
    , documentHighlightHandler       :: !(Maybe (Handler J.DocumentHighlightRequest))
    , documentSymbolHandler          :: !(Maybe (Handler J.DocumentSymbolRequest))
    , workspaceSymbolHandler         :: !(Maybe (Handler J.WorkspaceSymbolRequest))
    , codeActionHandler              :: !(Maybe (Handler J.CodeActionRequest))
    , codeLensHandler                :: !(Maybe (Handler J.CodeLensRequest))
    , codeLensResolveHandler         :: !(Maybe (Handler J.CodeLensResolveRequest))
    , documentColorHandler           :: !(Maybe (Handler J.DocumentColorRequest))
    , colorPresentationHandler       :: !(Maybe (Handler J.ColorPresentationRequest))
    , documentFormattingHandler      :: !(Maybe (Handler J.DocumentFormattingRequest))
    , documentRangeFormattingHandler :: !(Maybe (Handler J.DocumentRangeFormattingRequest))
    , documentTypeFormattingHandler  :: !(Maybe (Handler J.DocumentOnTypeFormattingRequest))
    , renameHandler                  :: !(Maybe (Handler J.RenameRequest))
    , foldingRangeHandler            :: !(Maybe (Handler J.FoldingRangeRequest))
    -- new in 3.0
    , documentLinkHandler            :: !(Maybe (Handler J.DocumentLinkRequest))
    , documentLinkResolveHandler     :: !(Maybe (Handler J.DocumentLinkResolveRequest))
    , executeCommandHandler          :: !(Maybe (Handler J.ExecuteCommandRequest))
    -- Next 2 go from server -> client
    -- , registerCapabilityHandler      :: !(Maybe (Handler J.RegisterCapabilityRequest))
    -- , unregisterCapabilityHandler    :: !(Maybe (Handler J.UnregisterCapabilityRequest))
    , willSaveWaitUntilTextDocHandler:: !(Maybe (Handler J.WillSaveWaitUntilTextDocumentRequest))

    -- Notifications from the client
    , didChangeConfigurationParamsHandler      :: !(Maybe (Handler J.DidChangeConfigurationNotification))
    , didOpenTextDocumentNotificationHandler   :: !(Maybe (Handler J.DidOpenTextDocumentNotification))
    , didChangeTextDocumentNotificationHandler :: !(Maybe (Handler J.DidChangeTextDocumentNotification))
    -- ^ Note: If you need to keep track of document changes,
    -- "Language.Haskell.LSP.VFS" will take care of these messages for you!
    , didCloseTextDocumentNotificationHandler  :: !(Maybe (Handler J.DidCloseTextDocumentNotification))
    , didSaveTextDocumentNotificationHandler   :: !(Maybe (Handler J.DidSaveTextDocumentNotification))
    , didChangeWatchedFilesNotificationHandler :: !(Maybe (Handler J.DidChangeWatchedFilesNotification))
    , didChangeWorkspaceFoldersNotificationHandler :: !(Maybe (Handler J.DidChangeWorkspaceFoldersNotification))
    -- new in 3.0
    , initializedHandler                       :: !(Maybe (Handler J.InitializedNotification))
    , willSaveTextDocumentNotificationHandler  :: !(Maybe (Handler J.WillSaveTextDocumentNotification))
    , cancelNotificationHandler                :: !(Maybe (Handler J.CancelNotification))

    -- Responses to Request messages originated from the server
    -- TODO: Properly decode response types and replace them with actual handlers
    , responseHandler                    :: !(Maybe (Handler J.BareResponseMessage))
    -- , registerCapabilityHandler                :: !(Maybe (Handler J.RegisterCapabilityResponse))
    -- , unregisterCapabilityHandler              :: !(Maybe (Handler J.RegisterCapabilityResponse))
    -- , showMessageHandler                       :: !(Maybe (Handler J.ShowMessageResponse))

    -- Initialization request on startup
    , initializeRequestHandler                 :: !(Maybe (Handler J.InitializeRequest))
    -- Will default to terminating `exitMessage` if Nothing
    , exitNotificationHandler                  :: !(Maybe (Handler J.ExitNotification))

    , customRequestHandler                     :: !(Maybe (Handler J.CustomClientRequest))
    , customNotificationHandler                :: !(Maybe (Handler J.CustomClientNotification))

    }

instance Default Handlers where
  -- These already implicitly do stuff to the VFS, so silence warnings about no handler
  def = nothings { didChangeTextDocumentNotificationHandler = Just ignore
                 , didOpenTextDocumentNotificationHandler   = Just ignore
                 , didCloseTextDocumentNotificationHandler  = Just ignore
                 }
    where ignore = const (pure ())
          nothings = Handlers Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------
nop :: a -> b -> IO a
nop = const . return


helper :: J.FromJSON a => (TVar (LanguageContextData config) -> a -> IO ()) -> (TVar (LanguageContextData config) -> J.Value -> IO ())
helper requestHandler tvarDat json =
  case J.fromJSON json of
    J.Success req -> requestHandler tvarDat req
    J.Error err -> do
      let msg = T.pack . unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
          failLog = sendErrorLog tvarDat msg
      case json of
        (J.Object o) -> case HM.lookup "id" o of
          Just olid -> case J.fromJSON olid of
            J.Success lid -> sendErrorResponse tvarDat lid msg
            _ -> failLog
          _ -> failLog
        _ -> failLog

handlerMap :: (Show config) => InitializeCallbacks config
           -> Handlers -> J.ClientMethod -> (TVar (LanguageContextData config) -> J.Value -> IO ())
-- General
handlerMap i h J.Initialize                      = handleInitialConfig i (initializeRequestHandler h)
handlerMap _ h J.Initialized                     = hh nop NotInitialized $ initializedHandler h
handlerMap _ _ J.Shutdown                        = helper shutdownRequestHandler
handlerMap _ h J.Exit                            =
  case exitNotificationHandler h of
    Just _ -> hh nop NotExit $ exitNotificationHandler h
    Nothing -> \ctxVar v -> do
      ctx <- readTVarIO ctxVar
      -- Capture exit notification
      case J.fromJSON v :: J.Result J.ExitNotification of
        J.Success n -> captureFromClient (NotExit n) (resCaptureFile ctx)
        J.Error _ -> return ()
      logm $ B.pack "haskell-lsp:Got exit, exiting"
      exitSuccess
handlerMap _ h J.CancelRequest                   = hh nop NotCancelRequestFromClient $ cancelNotificationHandler h
-- Workspace
handlerMap _ h J.WorkspaceDidChangeWorkspaceFolders = hwf $ didChangeWorkspaceFoldersNotificationHandler h
handlerMap i h J.WorkspaceDidChangeConfiguration = hc i $ didChangeConfigurationParamsHandler h
handlerMap _ h J.WorkspaceDidChangeWatchedFiles  = hh nop NotDidChangeWatchedFiles $ didChangeWatchedFilesNotificationHandler h
handlerMap _ h J.WorkspaceSymbol                 = hh nop ReqWorkspaceSymbols $ workspaceSymbolHandler h
handlerMap _ h J.WorkspaceExecuteCommand         = hh nop ReqExecuteCommand $ executeCommandHandler h
-- Document
handlerMap _ h J.TextDocumentDidOpen             = hh openVFS NotDidOpenTextDocument $ didOpenTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidChange           = hh changeFromClientVFS NotDidChangeTextDocument $ didChangeTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSave            = hh nop NotWillSaveTextDocument $ willSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSaveWaitUntil   = hh nop ReqWillSaveWaitUntil $ willSaveWaitUntilTextDocHandler h
handlerMap _ h J.TextDocumentDidSave             = hh nop NotDidSaveTextDocument $ didSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidClose            = hh closeVFS NotDidCloseTextDocument $ didCloseTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentCompletion          = hh nop ReqCompletion $ completionHandler h
handlerMap _ h J.CompletionItemResolve           = hh nop ReqCompletionItemResolve $ completionResolveHandler h
handlerMap _ h J.TextDocumentHover               = hh nop ReqHover $ hoverHandler h
handlerMap _ h J.TextDocumentSignatureHelp       = hh nop ReqSignatureHelp $ signatureHelpHandler h
handlerMap _ h J.TextDocumentDefinition          = hh nop ReqDefinition $ definitionHandler h
handlerMap _ h J.TextDocumentTypeDefinition      = hh nop ReqTypeDefinition $ typeDefinitionHandler h
handlerMap _ h J.TextDocumentImplementation      = hh nop ReqImplementation $ implementationHandler h
handlerMap _ h J.TextDocumentReferences          = hh nop ReqFindReferences $ referencesHandler h
handlerMap _ h J.TextDocumentDocumentHighlight   = hh nop ReqDocumentHighlights $ documentHighlightHandler h
handlerMap _ h J.TextDocumentDocumentSymbol      = hh nop ReqDocumentSymbols $ documentSymbolHandler h
handlerMap _ h J.TextDocumentFormatting          = hh nop ReqDocumentFormatting $ documentFormattingHandler h
handlerMap _ h J.TextDocumentRangeFormatting     = hh nop ReqDocumentRangeFormatting $ documentRangeFormattingHandler h
handlerMap _ h J.TextDocumentOnTypeFormatting    = hh nop ReqDocumentOnTypeFormatting $ documentTypeFormattingHandler h
handlerMap _ h J.TextDocumentCodeAction          = hh nop ReqCodeAction $ codeActionHandler h
handlerMap _ h J.TextDocumentCodeLens            = hh nop ReqCodeLens $ codeLensHandler h
handlerMap _ h J.CodeLensResolve                 = hh nop ReqCodeLensResolve $ codeLensResolveHandler h
handlerMap _ h J.TextDocumentDocumentColor       = hh nop ReqDocumentColor $ documentColorHandler h
handlerMap _ h J.TextDocumentColorPresentation   = hh nop ReqColorPresentation $ colorPresentationHandler h
handlerMap _ h J.TextDocumentDocumentLink        = hh nop ReqDocumentLink $ documentLinkHandler h
handlerMap _ h J.DocumentLinkResolve             = hh nop ReqDocumentLinkResolve $ documentLinkResolveHandler h
handlerMap _ h J.TextDocumentRename              = hh nop ReqRename $ renameHandler h
handlerMap _ h J.TextDocumentFoldingRange        = hh nop ReqFoldingRange $ foldingRangeHandler h
handlerMap _ _ J.WindowProgressCancel            = helper progressCancelHandler
handlerMap _ h (J.CustomClientMethod _)          = \ctxData val ->
    case val of
        J.Object o | "id" `HM.member` o ->
            -- Custom request
            hh nop ReqCustomClient (customRequestHandler h) ctxData val
        _ -> -- Custom notification
            hh nop NotCustomClient (customNotificationHandler h) ctxData val

-- ---------------------------------------------------------------------

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: forall b config. (J.FromJSON b)
   => (VFS -> b -> IO VFS) -> (b -> FromClientMessage) -> Maybe (Handler b) -> TVar (LanguageContextData config) -> J.Value -> IO ()
hh getVfs wrapper mh tvarDat json = do
      case J.fromJSON json of
        J.Success req -> do
          ctx <- readTVarIO tvarDat
          vfs' <- getVfs (resVFS ctx) req
          atomically $ modifyTVar' tvarDat (\c -> c {resVFS = vfs'})

          captureFromClient (wrapper req) (resCaptureFile ctx)

          case mh of
            Just h -> h req
            Nothing -> do
              let msg = T.pack $ unwords ["haskell-lsp:no handler for.", show json]
              sendErrorLog tvarDat msg
        J.Error  err -> do
          let msg = T.pack $ unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
          sendErrorLog tvarDat msg

handleInitialConfig
  :: (Show config)
  => InitializeCallbacks config
  -> Maybe (Handler J.InitializeRequest)
  -> TVar (LanguageContextData config)
  -> J.Value
  -> IO ()
handleInitialConfig (InitializeCallbacks { onInitialConfiguration, onStartup }) mh tvarDat json
  = handleMessageWithConfigChange ReqInitialize
                                  onInitialConfiguration
                                  (Just $ initializeRequestHandler' onStartup mh tvarDat)
                                  tvarDat
                                  json


hc
  :: (Show config)
  => InitializeCallbacks config
  -> Maybe (Handler J.DidChangeConfigurationNotification)
  -> TVar (LanguageContextData config)
  -> J.Value
  -> IO ()
hc (InitializeCallbacks { onConfigurationChange }) mh tvarDat json =
  handleMessageWithConfigChange NotDidChangeConfiguration
                                onConfigurationChange
                                mh
                                tvarDat
                                json

handleMessageWithConfigChange
  :: (J.FromJSON reqParams, Show reqParams, Show err)
  => (reqParams -> FromClientMessage) -- ^ The notification message from the client to expect
  -> (reqParams -> Either err config) -- ^ A function to parse the config out of the request
  -> Maybe (reqParams -> IO ()) -- ^ The upstream handler for the client request
  -> TVar (LanguageContextData config) -- ^ The context data containing the current configuration
  -> J.Value -- ^ The raw reqeust data
  -> IO ()
handleMessageWithConfigChange notification parseConfig mh tvarDat json =
  -- logs $ "haskell-lsp:hc DidChangeConfigurationNotification entered"
  case J.fromJSON json of
    J.Success req -> do
      ctx <- readTVarIO tvarDat

      captureFromClient (notification req) (resCaptureFile ctx)

      case parseConfig req of
        Left err -> do
          let
            msg =
              T.pack $ unwords
                ["haskell-lsp:configuration parse error.", show req, show err]
          sendErrorLog tvarDat msg
        Right newConfig -> do
          -- logs $ "haskell-lsp:hc DidChangeConfigurationNotification got newConfig:" ++ show newConfig
          let ctx' = ctx { resConfig = Just newConfig }
          atomically $ modifyTVar' tvarDat (const ctx')
      case mh of
        Just h  -> h req
        Nothing -> return ()
    J.Error err -> do
      let msg =
            T.pack
              $  unwords
              $  ["haskell-lsp:parse error.", show json, show err]
              ++ _ERR_MSG_URL
      sendErrorLog tvarDat msg

-- | Updates the list of workspace folders and then delegates back to 'hh'
hwf :: Maybe (Handler J.DidChangeWorkspaceFoldersNotification) -> TVar (LanguageContextData config) -> J.Value -> IO ()
hwf h tvarDat json = do
  case J.fromJSON json :: J.Result J.DidChangeWorkspaceFoldersNotification of
    J.Success (J.NotificationMessage _ _ params) -> atomically $ do

      oldWfs <- resWorkspaceFolders <$> readTVar tvarDat
      let J.List toRemove = params ^. J.event . J.removed
          wfs0 = foldr L.delete oldWfs toRemove
          J.List toAdd = params ^. J.event . J.added
          wfs1 = wfs0 <> toAdd

      modifyTVar' tvarDat (\c -> c {resWorkspaceFolders = wfs1})
    _ -> return ()
  hh nop NotDidChangeWorkspaceFolders h tvarDat json

-- ---------------------------------------------------------------------

getVirtualFile :: TVar (LanguageContextData config) -> J.NormalizedUri -> IO (Maybe VirtualFile)
getVirtualFile tvarDat uri = Map.lookup uri . resVFS <$> readTVarIO tvarDat

-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: TVar (LanguageContextData config) -> J.NormalizedUri -> IO FilePath
persistVirtualFile tvarDat uri = do
  st <- readTVarIO tvarDat
  let vfs = resVFS st
      revMap = reverseMap st

  (fn, new_vfs) <- persistFileVFS vfs uri
  let revMap' =
        -- TODO: Does the VFS make sense for URIs which are not files?
        -- The reverse map should perhaps be (FilePath -> URI)
        case J.uriToFilePath (J.fromNormalizedUri uri) of
          Just uri_fp -> Map.insert fn uri_fp revMap
          Nothing -> revMap

  atomically $ modifyTVar' tvarDat (\d -> d { resVFS = new_vfs
                                            , reverseMap = revMap' })
  return fn

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: TVar (LanguageContextData config)
               -> IO (FilePath -> FilePath)
reverseFileMap tvarDat = do
  revMap <- reverseMap <$> readTVarIO tvarDat
  let f fp = fromMaybe fp $ Map.lookup fp revMap
  return f

-- ---------------------------------------------------------------------

getConfig :: TVar (LanguageContextData config) -> IO (Maybe config)
getConfig tvar = resConfig <$> readTVarIO tvar

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
defaultLanguageContextData :: Handlers -> Options -> LspFuncs config -> TVar Int -> SendFunc -> Maybe FilePath -> LanguageContextData config
defaultLanguageContextData h o lf tv sf cf =
  LanguageContextData _INITIAL_RESPONSE_SEQUENCE h o sf mempty mempty mempty
                      Nothing tv lf cf mempty defaultProgressData

defaultProgressData :: ProgressData
defaultProgressData = ProgressData 0 Map.empty

-- ---------------------------------------------------------------------

handleMessage :: (Show config) => InitializeCallbacks config
              -> TVar (LanguageContextData config) -> BSL.ByteString -> IO ()
handleMessage dispatcherProc tvarDat jsonStr = do
  {-
  Message Types we must handle are the following

  Request      | jsonrpc | id | method | params?
  Response     | jsonrpc | id |        |         | response? | error?
  Notification | jsonrpc |    | method | params?

  -}

  case J.eitherDecode jsonStr :: Either String J.Object of
    Left  err -> do
      let msg =  T.pack $ unwords [ "haskell-lsp:incoming message parse error.", lbs2str jsonStr, show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"
      sendErrorLog tvarDat msg

    Right o -> do

      case HM.lookup "method" o of
        Just cmd@(J.String s) -> case J.fromJSON cmd of
                                   J.Success m -> handle (J.Object o) m
                                   J.Error _ -> do
                                     let msg = T.pack $ unwords ["haskell-lsp:unknown message received:method='"
                                                                 ++ T.unpack s ++ "',", lbs2str jsonStr]
                                     sendErrorLog tvarDat msg
        Just oops -> logs $ "haskell-lsp:got strange method param, ignoring:" ++ show oops
        Nothing -> do
          logs $ "haskell-lsp:Got reply message:" ++ show jsonStr
          handleResponse (J.Object o)

  where
    handleResponse json = do
      ctx <- readTVarIO tvarDat
      case responseHandler $ resHandlers ctx of
        Nothing -> sendErrorLog tvarDat $ T.pack $ "haskell-lsp: responseHandler is not defined, ignoring response " ++ lbs2str jsonStr
        Just h -> case J.fromJSON json of
          J.Success res -> h res
          J.Error err -> let msg = T.pack $ unwords $ ["haskell-lsp:response parse error.", lbs2str jsonStr, show err] ++ _ERR_MSG_URL
                           in sendErrorLog tvarDat msg
    -- capability based handlers
    handle json cmd = do
      ctx <- readTVarIO tvarDat
      let h = resHandlers ctx
      handlerMap dispatcherProc h cmd tvarDat json

-- ---------------------------------------------------------------------

makeResponseMessage :: J.RequestMessage J.ClientMethod req resp -> resp -> J.ResponseMessage resp
makeResponseMessage req result = J.ResponseMessage "2.0" (J.responseId $ req ^. J.id) (Just result) Nothing

makeResponseError :: J.LspIdRsp -> J.ResponseError -> J.ResponseMessage ()
makeResponseError origId err = J.ResponseMessage "2.0" origId Nothing (Just err)

-- ---------------------------------------------------------------------
-- |
--
sendEvent :: TVar (LanguageContextData config) -> FromServerMessage -> IO ()
sendEvent tvarCtx msg = sendResponse tvarCtx msg

-- |
--
sendResponse :: TVar (LanguageContextData config) -> FromServerMessage -> IO ()
sendResponse tvarCtx msg = do
  ctx <- readTVarIO tvarCtx
  resSendResponse ctx msg


-- ---------------------------------------------------------------------
-- |
--
--
sendErrorResponse :: TVar (LanguageContextData config) -> J.LspIdRsp -> Text -> IO ()
sendErrorResponse tv origId msg = sendErrorResponseS (sendEvent tv) origId J.InternalError msg

sendErrorResponseS ::  SendFunc -> J.LspIdRsp -> J.ErrorCode -> Text -> IO ()
sendErrorResponseS sf origId err msg = do
  sf $ RspError (J.ResponseMessage "2.0" origId Nothing
                  (Just $ J.ResponseError err msg Nothing) :: J.ErrorResponse)

sendErrorLog :: TVar (LanguageContextData config) -> Text -> IO ()
sendErrorLog tv msg = sendErrorLogS (sendEvent tv) msg

sendErrorLogS :: SendFunc -> Text -> IO ()
sendErrorLogS sf msg =
  sf $ NotLogMessage $ fmServerLogMessageNotification J.MtError msg

-- sendErrorShow :: String -> IO ()
-- sendErrorShow msg = sendErrorShowS sendEvent msg

sendErrorShowS :: SendFunc -> Text -> IO ()
sendErrorShowS sf msg =
  sf $ NotShowMessage $ fmServerShowMessageNotification J.MtError msg

-- ---------------------------------------------------------------------

defaultErrorHandlers :: (Show a) => TVar (LanguageContextData config) -> J.LspIdRsp -> a -> [E.Handler ()]
defaultErrorHandlers tvarDat origId req = [ E.Handler someExcept ]
  where
    someExcept (e :: E.SomeException) = do
      let msg = T.pack $ unwords ["request error.", show req, show e]
      sendErrorResponse tvarDat origId msg
      sendErrorLog tvarDat msg


-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler'
  :: (Show config)
  => (LspFuncs config -> IO (Maybe J.ResponseError))
  -> Maybe (Handler J.InitializeRequest)
  -> TVar (LanguageContextData config)
  -> J.InitializeRequest
  -> IO ()
initializeRequestHandler' onStartup mHandler tvarCtx req@(J.RequestMessage _ origId _ params) =
  flip E.catches (defaultErrorHandlers tvarCtx (J.responseId origId) req) $ do

    case mHandler of
      Just handler -> handler req
      Nothing -> return ()

    let wfs = case params ^. J.workspaceFolders of
                Just (J.List xs) -> xs
                Nothing -> []

    atomically $ modifyTVar' tvarCtx (\c -> c { resWorkspaceFolders = wfs })

    ctx0 <- readTVarIO tvarCtx
    let rootDir = getFirst $ foldMap First [ params ^. J.rootUri  >>= J.uriToFilePath
                                           , params ^. J.rootPath <&> T.unpack ]

    case rootDir of
      Nothing -> return ()
      Just dir -> do
        logs $ "haskell-lsp:initializeRequestHandler: setting current dir to project root:" ++ dir
        unless (null dir) $ setCurrentDirectory dir

    let
      getCapabilities :: J.InitializeParams -> C.ClientCapabilities
      getCapabilities (J.InitializeParams _ _ _ _ c _ _) = c
      getLspId tvId = atomically $ do
        cid <- readTVar tvId
        modifyTVar' tvId (+1)
        return $ J.IdInt cid

      clientSupportsWfs = fromMaybe False $ do
        let (C.ClientCapabilities mw _ _ _) = params ^. J.capabilities
        (C.WorkspaceClientCapabilities _ _ _ _ _ _ mwf _) <- mw
        mwf
      getWfs tvc
        | clientSupportsWfs = atomically $ Just . resWorkspaceFolders <$> readTVar tvc
        | otherwise = return Nothing

      clientSupportsProgress = fromMaybe False $ do
        let (C.ClientCapabilities _ _ wc _) = params ^. J.capabilities
        (C.WindowClientCapabilities mProgress) <- wc
        mProgress

      storeProgress :: Text -> Async a -> IO ()
      storeProgress n a = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let pc = progressCancel pd
            pc' = Map.insert n (cancelWith a ProgressCancelledException) pc
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressCancel = pc' }})

      -- Get a new id for the progress session and make a new one
      getNewProgressId :: IO Text
      getNewProgressId = fmap (T.pack . show) $ liftIO $ atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let x = progressNextId pd
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressNextId = x + 1 }})
        return x

      withProgressBase :: Bool -> (Text -> ProgressCancellable
                    -> ((Progress -> IO ()) -> IO a) -> IO a)
      withProgressBase indefinite title cancellable f
        | clientSupportsProgress = do
          sf <- liftIO $ resSendResponse <$> readTVarIO tvarCtx

          progId <- getNewProgressId

          let initialPercentage
                | indefinite = Nothing
                | otherwise = Just 0
              cancellable' = case cancellable of
                              Cancellable -> True
                              NotCancellable -> False

          -- Send initial notification
          liftIO $ sf $ NotProgressStart $ fmServerProgressStartNotification $
            J.ProgressStartParams progId title (Just cancellable')
              Nothing initialPercentage

          aid <- async $ f (updater progId sf)
          storeProgress progId aid
          res <- wait aid

          -- Send done notification
          liftIO $ sf $ NotProgressDone $ fmServerProgressDoneNotification $
            J.ProgressDoneParams progId

          return res
        | otherwise = f (const $ return ())
          where updater progId sf (Progress percentage msg) =
                  sf $ NotProgressReport $ fmServerProgressReportNotification $
                    J.ProgressReportParams progId msg percentage

      withProgress' :: Text -> ProgressCancellable -> ((Progress -> IO ()) -> IO a) -> IO a
      withProgress' = withProgressBase False

      withIndefiniteProgress' :: Text -> ProgressCancellable -> IO a -> IO a
      withIndefiniteProgress' title cancellable f =
        withProgressBase True title cancellable (const f)

    -- Launch the given process once the project root directory has been set
    let lspFuncs = LspFuncs (getCapabilities params)
                            (getConfig tvarCtx)
                            (resSendResponse ctx0)
                            (getVirtualFile tvarCtx)
                            (persistVirtualFile tvarCtx)
                            (reverseFileMap tvarCtx)
                            (publishDiagnostics tvarCtx)
                            (flushDiagnosticsBySource tvarCtx)
                            (getLspId $ resLspId ctx0)
                            rootDir
                            (getWfs tvarCtx)
                            withProgress'
                            withIndefiniteProgress'
    let ctx = ctx0 { resLspFuncs = lspFuncs }
    atomically $ writeTVar tvarCtx ctx

    initializationResult <- onStartup lspFuncs

    case initializationResult of
      Just errResp -> do
        sendResponse tvarCtx $ RspError $ makeResponseError (J.responseId origId) errResp

      Nothing -> do

        let
          h = resHandlers ctx
          o = resOptions  ctx

          supported (Just _) = Just True
          supported Nothing   = Nothing

          -- If a dynamic setting is provided use it, else set a
          -- static True if there is a handler.
          static (Just d) _ = Just d
          static _ (Just _) = Just (J.GotoOptionsStatic True)
          static _ Nothing  = Nothing

          sync = case textDocumentSync o of
                  Just x -> Just (J.TDSOptions x)
                  Nothing -> Nothing

          workspace = J.WorkspaceOptions workspaceFolder
          workspaceFolder = case didChangeWorkspaceFoldersNotificationHandler h of
            Just _ -> Just $
              -- sign up to receive notifications
              J.WorkspaceFolderOptions (Just True) (Just (J.WorkspaceFolderChangeNotificationsBool True))
            Nothing -> Nothing

          capa =
            J.InitializeResponseCapabilitiesInner
              { J._textDocumentSync                 = sync
              , J._hoverProvider                    = supported (hoverHandler h)
              , J._completionProvider               = completionProvider o
              , J._signatureHelpProvider            = signatureHelpProvider o
              , J._definitionProvider               = supported (definitionHandler h)
              , J._typeDefinitionProvider           = static (typeDefinitionProvider o) (typeDefinitionHandler h)
              , J._implementationProvider           = implementationProvider o
              , J._referencesProvider               = supported (referencesHandler h)
              , J._documentHighlightProvider        = supported (documentHighlightHandler h)
              , J._documentSymbolProvider           = supported (documentSymbolHandler h)
              , J._workspaceSymbolProvider          = supported (workspaceSymbolHandler h)
              , J._codeActionProvider               = codeActionProvider o
              , J._codeLensProvider                 = codeLensProvider o
              , J._documentFormattingProvider       = supported (documentFormattingHandler h)
              , J._documentRangeFormattingProvider  = supported (documentRangeFormattingHandler h)
              , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider o
              , J._renameProvider                   = supported (renameHandler h)
              , J._documentLinkProvider             = documentLinkProvider o
              , J._colorProvider                    = colorProvider o
              , J._foldingRangeProvider             = foldingRangeProvider o
              , J._executeCommandProvider           = executeCommandProvider o
              , J._workspace                        = Just workspace
              -- TODO: Add something for experimental
              , J._experimental                     = Nothing :: Maybe J.Value
              }

          -- TODO: wrap this up into a fn to create a response message
          res  = J.ResponseMessage "2.0" (J.responseId origId) (Just $ J.InitializeResponseCapabilities capa) Nothing

        sendResponse tvarCtx $ RspInitialize res

progressCancelHandler :: TVar (LanguageContextData config) -> J.ProgressCancelNotification -> IO ()
progressCancelHandler tvarCtx (J.NotificationMessage _ _ (J.ProgressCancelParams tid)) = do
  mact <- Map.lookup tid . progressCancel . resProgressData <$> readTVarIO tvarCtx
  case mact of
    Nothing -> return ()
    Just cancelAction -> cancelAction


-- |
--
shutdownRequestHandler :: TVar (LanguageContextData config) -> J.ShutdownRequest -> IO ()
shutdownRequestHandler tvarCtx req@(J.RequestMessage _ origId _ _) =
  flip E.catches (defaultErrorHandlers tvarCtx (J.responseId origId) req) $ do
  let res  = makeResponseMessage req Nothing

  sendResponse tvarCtx $ RspShutdown res

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
publishDiagnostics :: TVar (LanguageContextData config) -> PublishDiagnosticsFunc
publishDiagnostics tvarDat maxDiagnosticCount uri version diags = do
  ctx <- readTVarIO tvarDat
  let ds = updateDiagnostics (resDiagnostics ctx) uri version diags
  atomically $ writeTVar tvarDat $ ctx{resDiagnostics = ds}
  let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
  case mdp of
    Nothing -> return ()
    Just params -> do
      resSendResponse ctx $ NotPublishDiagnostics
        $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics params

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
flushDiagnosticsBySource :: TVar (LanguageContextData config) -> FlushDiagnosticsBySourceFunc
flushDiagnosticsBySource tvarDat maxDiagnosticCount msource = do
  -- logs $ "haskell-lsp:flushDiagnosticsBySource:source=" ++ show source
  ctx <- readTVarIO tvarDat
  let ds = flushBySource (resDiagnostics ctx) msource
  atomically $ writeTVar tvarDat $ ctx {resDiagnostics = ds}
  -- Send the updated diagnostics to the client
  forM_ (Map.keys ds) $ \uri -> do
    -- logs $ "haskell-lsp:flushDiagnosticsBySource:uri=" ++ show uri
    let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
    case mdp of
      Nothing -> return ()
      Just params -> do
        resSendResponse ctx $ NotPublishDiagnostics
          $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics params

-- |=====================================================================
--
--  utility


-- |
--  Logger
--
setupLogger :: Maybe FilePath -> [String] -> Priority -> IO ()
setupLogger mLogFile extraLogNames level = do

  logStream <- case mLogFile of
    Just logFile -> openFile logFile AppendMode
    Nothing      -> return stderr
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    L.updateGlobalLogger logName $ L.setHandlers [logHandler]
    L.updateGlobalLogger logName $ L.setLevel level


-- ---------------------------------------------------------------------

-- | The changes in a workspace edit should be applied from the end of the file
-- toward the start. Sort them into this order.
reverseSortEdit :: J.WorkspaceEdit -> J.WorkspaceEdit
reverseSortEdit (J.WorkspaceEdit cs dcs) = J.WorkspaceEdit cs' dcs'
  where
    cs' :: Maybe J.WorkspaceEditMap
    cs' = (fmap . fmap ) sortTextEdits cs

    dcs' :: Maybe (J.List J.TextDocumentEdit)
    dcs' = (fmap . fmap ) sortTextDocumentEdits dcs

    sortTextEdits :: J.List J.TextEdit -> J.List J.TextEdit
    sortTextEdits (J.List edits) = J.List (L.sortBy down edits)

    sortTextDocumentEdits :: J.TextDocumentEdit -> J.TextDocumentEdit
    sortTextDocumentEdits (J.TextDocumentEdit td (J.List edits)) = J.TextDocumentEdit td (J.List edits')
      where
        edits' = L.sortBy down edits

    down (J.TextEdit r1 _) (J.TextEdit r2 _) = r2 `compare` r1
