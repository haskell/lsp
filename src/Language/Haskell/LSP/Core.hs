{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.Haskell.LSP.Core (
    handleMessage
  , LanguageContextData(..)
  , Handler(..)
  , VFSData(..)
  , InitializeCallbacks(..)
  , LspFuncs(..)
  , Progress(..)
  , ProgressCancellable(..)
  , ProgressCancelledException
  , Handlers
  , Options(..)
  , ClientResponseHandler(..)
  , ServerResponseHandler(..)
  , makeResponseMessage
  , makeResponseError
  , setupLogger
  , reverseSortEdit
  , initializeRequestHandler
  , LspM
  , runReaderT
  , LanguageContextEnv
  , FromServerMessage
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Monad
import           Control.Applicative
import           Data.Functor.Product
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Lens ( (<&>), (^.), (^?), _Just )
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import           Data.IxMap
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid hiding (Product)
import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Constant
-- import           Language.Haskell.LSP.Types.MessageFuncs
import qualified Language.Haskell.LSP.Types.Capabilities    as J
import Language.Haskell.LSP.Types as J hiding (Progress)
import qualified Language.Haskell.LSP.Types.Lens as J
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

data LanguageContextEnv config =
  LanguageContextEnv
  { resHandlers            :: !Handlers
  , resParseConfig         :: !(DidChangeConfigurationNotification-> Either T.Text config)
  , resSendMessage         :: !(FromServerMessage -> IO ())
  , resData                :: !(TVar (LanguageContextData config))
  }

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData config =
  LanguageContextData
  { resVFS                 :: !VFSData
  , resDiagnostics         :: !DiagnosticStore
  , resConfig              :: !(Maybe config)
  , resWorkspaceFolders    :: ![WorkspaceFolder]
  , resProgressData        :: !ProgressData
  , resPendingResponses    :: !ResponseMap
  , resLspId               :: !Int
  }

type ResponseMap = IxMap LspId (Product SMethod ServerResponseHandler)

data ProgressData = ProgressData { progressNextId :: !Int
                                 , progressCancel :: !(Map.Map ProgressToken (IO ())) }

data VFSData =
  VFSData
    { vfsData :: !VFS
    , reverseMap :: !(Map.Map FilePath FilePath)
    }

type LspM config = ReaderT (LanguageContextEnv config) IO

modifyData :: (LanguageContextData config -> LanguageContextData config) -> LspM config ()
modifyData f = do
  tvarDat <- asks resData
  liftIO $ atomically $ modifyTVar' tvarDat f

stateData :: (LanguageContextData config -> (a,LanguageContextData config)) -> LspM config a
stateData f = do
  tvarDat <- asks resData
  liftIO $ atomically $ stateTVar tvarDat f

readData :: (LanguageContextData config -> a) -> LspM config a
readData f = do
  tvarDat <- asks resData
  liftIO $ f <$> readTVarIO tvarDat

-- ---------------------------------------------------------------------

-- | Language Server Protocol options that the server may configure.
-- If you set handlers for some requests, you may need to set some of these options.
data Options =
  Options
    { textDocumentSync                 :: Maybe J.TextDocumentSyncOptions
    -- |  The characters that trigger completion automatically.
    , completionTriggerCharacters      :: Maybe [Char]
    -- | The list of all possible characters that commit a completion. This field can be used
    -- if clients don't support individual commmit characters per completion item. See
    -- `_commitCharactersSupport`.
    , completionAllCommitCharacters    :: Maybe [Char]
    -- | The characters that trigger signature help automatically.
    , signatureHelpTriggerCharacters   :: Maybe [Char]
    -- | List of characters that re-trigger signature help.
    -- These trigger characters are only active when signature help is already showing. All trigger characters
    -- are also counted as re-trigger characters.
    , signatureHelpRetriggerCharacters :: Maybe [Char]
    -- | CodeActionKinds that this server may return.
    -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    -- may list out every specific kind they provide.
    , codeActionKinds                  :: Maybe [CodeActionKind]
    -- | The list of characters that triggers on type formatting.
    -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
    -- The first character is mandatory, so a 'NonEmpty' should be passed.
    , documentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
    -- | The commands to be executed on the server.
    -- If you set `executeCommandHandler`, you **must** set this.
    , executeCommandCommands           :: Maybe [Text]
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing

-- | A function to publish diagnostics. It aggregates all diagnostics pertaining
-- to a particular version of a document, by source, and sends a
-- 'textDocument/publishDiagnostics' notification with the total (limited by the
-- first parameter) whenever it is updated.
type PublishDiagnosticsFunc = Int -- Max number of diagnostics to send
                            -> NormalizedUri -> TextDocumentVersion -> DiagnosticsBySource -> IO ()

-- | A function to remove all diagnostics from a particular source, and send the updates to the client.
type FlushDiagnosticsBySourceFunc = Int -- Max number of diagnostics to send
                                  -> Maybe DiagnosticSource -> IO ()

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

type SendRequestFunc = forall m.
                        SServerMethod (m :: Method FromServer Request)
                     -> MessageParams m
                     -> (LspId m -> Either ResponseError (ResponseParams m) -> IO ())
                     -> IO (LspId m)
type SendNotifcationFunc = forall m.
                            SServerMethod (m :: Method FromServer Notification)
                         -> MessageParams m
                         -> IO ()

-- | Returned to the server on startup, providing ways to interact with the client.
data LspFuncs c =
  LspFuncs
    { clientCapabilities           :: !J.ClientCapabilities
    , config                       :: !(IO (Maybe c))
      -- ^ Derived from the DidChangeConfigurationNotification message via a
      -- server-provided function.
    , sendReq                      :: !SendRequestFunc
      -- ^ The function used to send requests to the client and handle their
      -- responses.
    , sendNot                      :: !SendNotifcationFunc
      -- ^ The function used to send notifications to the client.
    , getVirtualFileFunc           :: !(NormalizedUri -> IO (Maybe VirtualFile))
    , getVirtualFilesFunc          :: !(IO VFS)
      -- ^ Function to return the 'VirtualFile' associated with a
      -- given 'NormalizedUri', if there is one.
    , persistVirtualFileFunc       :: !(NormalizedUri -> IO (Maybe FilePath))
    , getVersionedTextDocFunc      :: !(TextDocumentIdentifier -> IO VersionedTextDocumentIdentifier)
      -- ^ Given a text document identifier, annotate it with the latest version.
    , reverseFileMapFunc           :: !(IO (FilePath -> FilePath))
    , publishDiagnosticsFunc       :: !PublishDiagnosticsFunc
    , flushDiagnosticsBySourceFunc :: !FlushDiagnosticsBySourceFunc
    , rootPath                     :: !(Maybe FilePath)
    , getWorkspaceFolders          :: !(IO (Maybe [WorkspaceFolder]))
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
    { onInitialConfiguration :: InitializeRequest -> Either T.Text config
      -- ^ Invoked on the first message from the language client, containg the client configuration
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onConfigurationChange :: DidChangeConfigurationNotification-> Either T.Text config
      -- ^ Invoked whenever the clients sends a message with a changed client configuration.
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onStartup :: LspFuncs config -> IO (Maybe ResponseError)
      -- ^ Once the initial configuration has been received, this callback will be invoked to offer
      -- the language server implementation the chance to create any processes or start new threads
      -- that may be necesary for the server lifecycle.
    }

newtype ClientResponseHandler (m :: Method FromClient t) = ClientResponseHandler (ResponseHandlerFunc m)

newtype ServerResponseHandler (m :: Method FromServer t) = ServerResponseHandler (ResponseHandlerFunc m)

mkClientResponseHandler :: SClientMethod m -> ClientMessage m -> LspM config (ClientResponseHandler m)
mkClientResponseHandler m cm = do
  sf <- asks resSendMessage
  pure $ ClientResponseHandler $ case splitClientMethod m of
    IsClientNot -> ()
    IsClientReq -> \mrsp -> case mrsp of
      Left err  -> sf $ FromServerRsp m $ makeResponseError   (cm ^. J.id) err
      Right rsp -> sf $ FromServerRsp m $ makeResponseMessage (cm ^. J.id) rsp
    IsClientEither -> case cm of
      NotMess _ -> ()
      ReqMess req -> \mrsp -> case mrsp of
        Left err  -> sf $ FromServerRsp m $ makeResponseError   (req ^. J.id) err
        Right rsp -> sf $ FromServerRsp m $ makeResponseMessage (req ^. J.id) rsp

-- | Return value signals if response handler was inserted succesfully
-- Might fail if the id was already in the map
addResponseHandler :: LspId m -> (Product SMethod ServerResponseHandler) m -> LspM config Bool
addResponseHandler lid h = do
  stateData $ \ctx@LanguageContextData{resPendingResponses} ->
    case insertIxMap lid h resPendingResponses of
      Just m -> (True, ctx { resPendingResponses = m})
      Nothing -> (False, ctx)

mkSendNotFunc :: forall (m :: Method FromServer Notification) config. SServerMethod m -> MessageParams m -> LspM config ()
mkSendNotFunc m params =
  let msg = NotificationMessage "2.0" m params
  in case splitServerMethod m of
        IsServerNot -> sendToClient $ fromServerNot msg
        IsServerEither -> sendToClient $ FromServerMess m $ NotMess msg

mkSendReqFunc :: forall (m :: Method FromServer Request) config.
                       SServerMethod m
                    -> MessageParams m
                    -> (LspId m -> Either ResponseError (ResponseParams m) -> IO ())
                    -> LspM config (LspId m)
mkSendReqFunc m params resHandler = do
  reqId <- IdInt <$> freshLspId
  success <- addResponseHandler reqId (Pair m (ServerResponseHandler (resHandler reqId)))
  unless success $ error "haskell-lsp: could not send FromServer request as id is reused"

  let msg = RequestMessage "2.0" reqId m params
  ~() <- case splitServerMethod m of
    IsServerReq -> sendToClient $ fromServerReq msg
    IsServerEither -> sendToClient $ FromServerMess m $ ReqMess msg
  return reqId

-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
newtype Handler m = Handler {runHandler :: ClientMessage m -> ClientResponseHandler m -> IO ()}
type Handlers = forall t (m :: Method FromClient t). SMethod m -> Maybe (Handler m)

-- ---------------------------------------------------------------------
nop :: Maybe (b -> LspM config ())
nop = Nothing

handlerMap :: (Show config) => SClientMethod m -> ClientMessage m -> LspM config ()
handlerMap c = case c of
  SWorkspaceDidChangeWorkspaceFolders -> hh (Just updateWorkspaceFolders) c
  SWorkspaceDidChangeConfiguration    -> hh (Just handleConfigChange) c
  STextDocumentDidOpen                -> hh (Just $ vfsFunc openVFS) c
  STextDocumentDidChange              -> hh (Just $ vfsFunc changeFromClientVFS) c
  STextDocumentDidClose               -> hh (Just $ vfsFunc closeVFS) c
  SWorkDoneProgressCancel             -> hh (Just progressCancelHandler) c
  _ -> hh nop c

-- ---------------------------------------------------------------------

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: Maybe (ClientMessage m -> LspM config ()) -> SClientMethod m -> ClientMessage m -> LspM config ()
hh mAction m req = do
  maybe (return ()) (\f -> f req) mAction
  getHandler <- asks resHandlers
  let handleReq h = do
        respH <- mkClientResponseHandler m req
        liftIO $ runHandler h req respH
  case getHandler m of
    Just h -> handleReq h
    Nothing
      | SExit <- m -> handleReq exitNotificationHandler
      | SShutdown <- m -> handleReq shutdownRequestHandler
      -- '$/' notifications should/could be ignored by server.
      -- Don't log errors in that case.
      -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
      | isOptionalNotification m -> return ()
      | otherwise -> do
          let msg = T.pack $ unwords ["haskell-lsp:no handler for: ", show m]
          sendErrorLog msg
  where
    isOptionalNotification (SCustomMethod method)
      | "$/" `T.isPrefixOf` method = True
    isOptionalNotification _  = False

handleConfigChange :: DidChangeConfigurationNotification -> LspM config ()
handleConfigChange req = do
  parseConfig <- asks resParseConfig
  case parseConfig req of
    Left err -> do
      let msg = T.pack $ unwords
            ["haskell-lsp:configuration parse error.", show req, show err]
      sendErrorLog msg
    Right newConfig ->
      modifyData $ \ctx -> ctx { resConfig = Just newConfig }

vfsFunc :: (VFS -> b -> (VFS, [String])) -> b -> LspM config ()
vfsFunc modifyVfs req = do
  join $ stateData $ \ctx@LanguageContextData{resVFS = VFSData vfs rm} ->
    let (vfs', ls) = modifyVfs vfs req
    in (liftIO $ mapM_ logs ls,ctx{ resVFS = VFSData vfs' rm})

-- | Updates the list of workspace folders
updateWorkspaceFolders :: Message WorkspaceDidChangeWorkspaceFolders -> LspM config ()
updateWorkspaceFolders (NotificationMessage _ _ params) = do
  let List toRemove = params ^. J.event . J.removed
      List toAdd = params ^. J.event . J.added
      newWfs oldWfs = foldr L.delete oldWfs toRemove <> toAdd
  modifyData $ \c -> c {resWorkspaceFolders = newWfs $ resWorkspaceFolders c}

-- ---------------------------------------------------------------------

-- | Return the 'VirtualFile' associated with a given 'NormalizedUri', if there is one.
getVirtualFile :: NormalizedUri -> LspM config (Maybe VirtualFile)
getVirtualFile uri = readData $ Map.lookup uri . vfsMap . vfsData . resVFS

getVirtualFiles :: LspM config VFS
getVirtualFiles = readData $ vfsData . resVFS

-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: NormalizedUri -> LspM config (Maybe FilePath)
persistVirtualFile uri = do
  join $ stateData $ \ctx@LanguageContextData{resVFS = vfs} ->
    case persistFileVFS (vfsData vfs) uri of
      Nothing -> (return Nothing, ctx)
      Just (fn, write) ->
        let revMap = case uriToFilePath (fromNormalizedUri uri) of
              Just uri_fp -> Map.insert fn uri_fp $ reverseMap vfs
              -- TODO: Does the VFS make sense for URIs which are not files?
              -- The reverse map should perhaps be (FilePath -> URI)
              Nothing -> reverseMap vfs
            act = do
              liftIO write
              pure (Just fn)
        in (act, ctx{resVFS = vfs {reverseMap = revMap} })

getVersionedTextDoc :: TextDocumentIdentifier -> LspM config VersionedTextDocumentIdentifier
getVersionedTextDoc doc = do
  let uri = doc ^. J.uri
  mvf <- getVirtualFile (toNormalizedUri uri)
  let ver = case mvf of
        Just (VirtualFile lspver _ _) -> Just lspver
        Nothing -> Nothing
  return (VersionedTextDocumentIdentifier uri ver)

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: LspM config (FilePath -> FilePath)
reverseFileMap = do
  vfs <- readData resVFS
  let f fp = fromMaybe fp . Map.lookup fp . reverseMap $ vfs
  return f

-- ---------------------------------------------------------------------

getConfig :: LspM config (Maybe config)
getConfig = readData resConfig

-- ---------------------------------------------------------------------

_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new haskell-lsp."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=xxxxxxxxxxxxxxx"
               ]

defaultProgressData :: ProgressData
defaultProgressData = ProgressData 0 Map.empty

-- ---------------------------------------------------------------------

handleMessage :: (Show config) => BSL.ByteString -> LspM config ()
handleMessage jsonStr = do
  tvarDat <- asks resData
  join $ liftIO $ atomically $ fmap handleErrors $ runExceptT $ do
      val <- except $ J.eitherDecode jsonStr
      ctx <- lift   $ readTVar tvarDat
      msg <- except $ J.parseEither (parser $ resPendingResponses ctx) val
      lift $ case msg of
        FromClientMess m mess ->
          pure $ handlerMap m mess
        FromClientRsp (Pair (ServerResponseHandler f) (Const newMap)) res -> do
          modifyTVar' tvarDat (\c -> c { resPendingResponses = newMap })
          pure $ liftIO $ f (res ^. J.result)
  where
    parser :: ResponseMap -> J.Value -> J.Parser (FromClientMessage' (Product ServerResponseHandler (Const ResponseMap)))
    parser rm = parseClientMessage $ \i ->
      let (mhandler, newMap) = pickFromIxMap i rm
        in (\(Pair m handler) -> (m,Pair handler (Const newMap))) <$> mhandler

    handleErrors = either (sendErrorLog . errMsg) id

    errMsg err = T.pack $ unwords [ "haskell-lsp:incoming message parse error.", lbs2str jsonStr, show err]
           ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
           ++ "\n"

-- ---------------------------------------------------------------------

makeResponseMessage :: LspId m -> ResponseParams m -> ResponseMessage m
makeResponseMessage rid result = ResponseMessage "2.0" (Just rid) (Right result)

makeResponseError :: LspId m -> ResponseError -> ResponseMessage m
makeResponseError origId err = ResponseMessage "2.0" (Just origId) (Left err)

-- ---------------------------------------------------------------------

sendToClient :: FromServerMessage -> LspM config ()
sendToClient msg = do
  f <- asks resSendMessage
  liftIO $ f msg

-- ---------------------------------------------------------------------

sendErrorLog :: Text -> LspM config ()
sendErrorLog msg =
  sendToClient $ fromServerNot $
    NotificationMessage "2.0" SWindowLogMessage (LogMessageParams MtError msg)

-- ---------------------------------------------------------------------

initializeErrorHandler :: (ResponseError -> IO ()) -> E.SomeException -> IO (Maybe a)
initializeErrorHandler sendResp e = do
    sendResp $ ResponseError InternalError msg Nothing
    pure Nothing
  where
    msg = T.pack $ unwords ["Error on initialize:", show e]

-- |=====================================================================
--
-- Handlers

freshLspId :: LspM config Int
freshLspId = do
  stateData $ \c ->
    (resLspId c, c{resLspId = resLspId c+1})

-- | Call this to initialize the session
initializeRequestHandler
  :: forall config. (Show config)
  => InitializeCallbacks config
  -> VFS
  -> Handlers
  -> Options
  -> (FromServerMessage -> IO ())
  -> Message Initialize
  -> IO (Maybe (LanguageContextEnv config))
initializeRequestHandler InitializeCallbacks{..} vfs handlers options sendFunc req = do
  let sendResp = sendFunc . FromServerRsp SInitialize
  flip E.catch (initializeErrorHandler $ sendResp . makeResponseError (req ^. J.id)) $ do

    let params = req ^. J.params

    let rootDir = getFirst $ foldMap First [ params ^. J.rootUri  >>= uriToFilePath
                                           , params ^. J.rootPath <&> T.unpack ]

    case rootDir of
      Nothing -> return ()
      Just dir -> do
        logs $ "haskell-lsp:initializeRequestHandler: setting current dir to project root:" ++ dir
        unless (null dir) $ setCurrentDirectory dir

    let
      clientSupportsWfs = fromMaybe False $ do
        let (J.ClientCapabilities mw _ _ _) = params ^. J.capabilities
        (J.WorkspaceClientCapabilities _ _ _ _ _ _ mwf _) <- mw
        mwf
      getWfs tvc
        | clientSupportsWfs = atomically $ Just . resWorkspaceFolders <$> readTVar tvc
        | otherwise = return Nothing

      clientSupportsProgress = fromMaybe False $ do
        let (J.ClientCapabilities _ _ wc _) = params ^. J.capabilities
        (J.WindowClientCapabilities mProgress) <- wc
        mProgress


    let wfs = case params ^. J.workspaceFolders of
                Just (List xs) -> xs
                Nothing -> []
        initialConfigRes = onInitialConfiguration req
        initialConfig = either (const Nothing) Just initialConfigRes

    tvarCtx <- newTVarIO $ LanguageContextData (VFSData vfs mempty) mempty initialConfig wfs defaultProgressData emptyIxMap 0

    -- Launch the given process once the project root directory has been set
    let lspFuncs = LspFuncs (params ^. J.capabilities)
                            (runReaderT getConfig env)
                            (\a b c -> flip runReaderT env $ mkSendReqFunc a b c)
                            (\a b -> flip runReaderT env $ mkSendNotFunc a b)
                            (flip runReaderT env . getVirtualFile)
                            (flip runReaderT env getVirtualFiles)
                            (flip runReaderT env . persistVirtualFile)
                            (flip runReaderT env . getVersionedTextDoc)
                            (runReaderT reverseFileMap env)
                            (\a b c d -> flip runReaderT env $ publishDiagnostics a b c d)
                            (\a b -> flip runReaderT env $ flushDiagnosticsBySource a b)
                            rootDir
                            (getWfs tvarCtx)
                            withProgressFunc
                            withIndefiniteProgressFunc
        env = LanguageContextEnv handlers onConfigurationChange sendFunc tvarCtx

        withProgressFunc :: Text -> ProgressCancellable -> ((Progress -> IO ()) -> IO a) -> IO a
        withProgressFunc t c f
          | clientSupportsProgress = flip runReaderT env $ withProgress' t c f
          | otherwise = f (const $ return ())
        withIndefiniteProgressFunc :: Text -> ProgressCancellable -> IO a -> IO a
        withIndefiniteProgressFunc t c f
          | clientSupportsProgress = flip runReaderT env $ withIndefiniteProgress' t c f
          | otherwise = f

    initializationResult <- onStartup lspFuncs
    case initializationResult of
      Just errResp -> do
        sendResp $ makeResponseError (req ^. J.id) errResp
      Nothing -> do
        let capa = serverCapabilities (params ^. J.capabilities) options handlers
        sendResp $ makeResponseMessage (req ^. J.id) (InitializeResponseCapabilities capa)


    case initialConfigRes of
      Right _ -> pure ()
      Left err -> do
        let msg = T.pack $ unwords
              ["haskell-lsp:configuration parse error.", show req, show err]
        runReaderT (sendErrorLog msg) env

    return $ Just env

--------------------------------------------------------------------------------
-- PROGRESS
--------------------------------------------------------------------------------

storeProgress :: ProgressToken -> Async a -> LspM config ()
storeProgress n a = do
  let f = Map.insert n (cancelWith a ProgressCancelledException) . progressCancel
  modifyData $ \ctx -> ctx { resProgressData = (resProgressData ctx) { progressCancel = f (resProgressData ctx)}}

deleteProgress :: ProgressToken -> LspM config ()
deleteProgress n = do
  let f = Map.delete n . progressCancel
  modifyData $ \ctx -> ctx { resProgressData = (resProgressData ctx) { progressCancel = f (resProgressData ctx)}}

-- Get a new id for the progress session and make a new one
getNewProgressId :: LspM config ProgressToken
getNewProgressId = do
  stateData $ \ctx@LanguageContextData{resProgressData} ->
    let x = progressNextId resProgressData
        ctx' = ctx { resProgressData = resProgressData { progressNextId = x + 1 }}
    in (ProgressNumericToken x, ctx')

withProgressBase :: Bool -> Text -> ProgressCancellable -> ((Progress -> IO ()) -> IO a) -> LspM config a
withProgressBase indefinite title cancellable f = do
  env <- ask
  let sf x = runReaderT (sendToClient x) env

  progId <- getNewProgressId

  let initialPercentage
        | indefinite = Nothing
        | otherwise = Just 0
      cancellable' = case cancellable of
                      Cancellable -> True
                      NotCancellable -> False

  -- Create progress token
  -- FIXME  : This needs to wait until the request returns before
  -- continuing!!!
  _ <- mkSendReqFunc SWindowWorkDoneProgressCreate
        (WorkDoneProgressCreateParams progId) $ \_ res -> do
          case res of
            -- An error ocurred when the client was setting it up
            -- No need to do anything then, as per the spec
            Left _err -> pure ()
            Right () -> pure ()

  -- Send initial notification
  mkSendNotFunc SProgress $
    fmap Begin $ ProgressParams progId $
      WorkDoneProgressBeginParams title (Just cancellable') Nothing initialPercentage

  aid <- liftIO $ async $ f (updater progId (sf . fromServerNot))
  storeProgress progId aid
  res <- liftIO $ wait aid

  -- Send done notification
  mkSendNotFunc SProgress $
    End <$> (ProgressParams progId (WorkDoneProgressEndParams Nothing))
  -- Delete the progress cancellation from the map
  -- If we don't do this then it's easy to leak things as the map contains any IO action.
  deleteProgress progId


  return res
  where updater progId sf (Progress percentage msg) =
          sf $ NotificationMessage "2.0" SProgress $
            fmap Report $ ProgressParams progId $
              WorkDoneProgressReportParams Nothing msg percentage

withProgress' :: Text -> ProgressCancellable -> ((Progress -> IO ()) -> IO a) -> LspM config a
withProgress' = withProgressBase False

withIndefiniteProgress' :: Text -> ProgressCancellable -> IO a -> LspM config a
withIndefiniteProgress' title cancellable f =
  withProgressBase True title cancellable (const f)

-- | Infers the capabilities based on registered handlers, and sets the appropriate options.
-- A provider should be set to Nothing if the server does not support it, unless it is a
-- static option.
serverCapabilities :: J.ClientCapabilities -> Options -> Handlers -> J.ServerCapabilities
serverCapabilities clientCaps o h =
  J.ServerCapabilities
    { J._textDocumentSync                 = sync
    , J._hoverProvider                    = supportedBool J.STextDocumentHover
    , J._completionProvider               = completionProvider
    , J._signatureHelpProvider            = signatureHelpProvider
    , J._definitionProvider               = supportedBool J.STextDocumentDefinition
    , J._typeDefinitionProvider           = supportedBool J.STextDocumentTypeDefinition
    , J._implementationProvider           = supportedBool J.STextDocumentImplementation
    , J._referencesProvider               = supportedBool J.STextDocumentReferences
    , J._documentHighlightProvider        = supportedBool J.STextDocumentDocumentHighlight
    , J._documentSymbolProvider           = supportedBool J.STextDocumentDocumentSymbol
    , J._workspaceSymbolProvider          = supported J.SWorkspaceSymbol
    , J._codeActionProvider               = codeActionProvider
    , J._codeLensProvider                 = supported' J.STextDocumentCodeLens $ J.CodeLensOptions
                                              (J.WorkDoneProgressOptions Nothing)
                                              (supported J.SCodeLensResolve)
    , J._documentFormattingProvider       = supportedBool J.STextDocumentFormatting
    , J._documentRangeFormattingProvider  = supportedBool J.STextDocumentRangeFormatting
    , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , J._renameProvider                   = supportedBool J.STextDocumentRename
    , J._documentLinkProvider             = supported' J.STextDocumentDocumentLink $ J.DocumentLinkOptions $
                                              supported J.SDocumentLinkResolve
    , J._colorProvider                    = supportedBool J.STextDocumentDocumentColor
    , J._foldingRangeProvider             = supportedBool J.STextDocumentFoldingRange
    , J._executeCommandProvider           = executeCommandProvider
    , J._workspace                        = Just workspace
    -- TODO: Add something for experimental
    , J._experimental                     = Nothing :: Maybe J.Value
    }
  where
    
    -- | For when we just return a simple @true@/@false@ to indicate if we
    -- support the capability
    supportedBool = Just . J.L . supported_b

    supported' m b
      | supported_b m = Just b
      | otherwise = Nothing

    supported :: forall m. J.SClientMethod m -> Maybe Bool
    supported = Just . supported_b

    supported_b :: forall m. J.SClientMethod m -> Bool
    supported_b m = isJust (h m)

    singleton :: a -> [a]
    singleton x = [x]

    completionProvider
      | supported_b J.STextDocumentCompletion = Just $
          J.CompletionOptions
            (J.WorkDoneProgressOptions Nothing)
            (supported J.SCompletionItemResolve)
            (map singleton <$> completionTriggerCharacters o)
            (map singleton <$> completionAllCommitCharacters o)
      | otherwise = Nothing

    clientSupportsCodeActionKinds = isJust $
      clientCaps ^? J.textDocument . _Just . J.codeAction . _Just . J.codeActionLiteralSupport

    codeActionProvider
      | clientSupportsCodeActionKinds
      , supported_b J.STextDocumentCodeAction = Just $ maybe (J.L True) (J.R . J.CodeActionOptions . Just) (codeActionKinds o)
      | supported_b J.STextDocumentCodeAction = Just (J.L True)
      | otherwise = Just (J.L False)

    signatureHelpProvider
      | supported_b J.STextDocumentSignatureHelp = Just $
          J.SignatureHelpOptions
            (J.WorkDoneProgressOptions Nothing)
            (map singleton <$> signatureHelpTriggerCharacters o)
            (map singleton <$> signatureHelpRetriggerCharacters o)
      | otherwise = Nothing

    documentOnTypeFormattingProvider
      | supported_b J.STextDocumentOnTypeFormatting
      , Just (first :| rest) <- documentOnTypeFormattingTriggerCharacters o = Just $
          J.DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singleton) rest))
      | supported_b J.STextDocumentOnTypeFormatting
      , Nothing <- documentOnTypeFormattingTriggerCharacters o =
          error "documentOnTypeFormattingTriggerCharacters needs to be set if a documentOnTypeFormattingHandler is set"
      | otherwise = Nothing

    executeCommandProvider
      | supported_b J.SWorkspaceExecuteCommand
      , Just cmds <- executeCommandCommands o = Just (J.ExecuteCommandOptions (J.List cmds))
      | supported_b J.SWorkspaceExecuteCommand
      , Nothing <- executeCommandCommands o =
          error "executeCommandCommands needs to be set if a executeCommandHandler is set"
      | otherwise = Nothing

    sync = case textDocumentSync o of
            Just x -> Just (J.TDSOptions x)
            Nothing -> Nothing

    workspace = J.WorkspaceServerCapabilities workspaceFolder
    workspaceFolder = supported' J.SWorkspaceDidChangeWorkspaceFolders $
        -- sign up to receive notifications
        J.WorkspaceFoldersServerCapabilities (Just True) (Just (J.R True))

progressCancelHandler :: J.WorkDoneProgressCancelNotification -> LspM config ()
progressCancelHandler (J.NotificationMessage _ _ (J.WorkDoneProgressCancelParams tid)) = do
  mact <- readData $ Map.lookup tid . progressCancel . resProgressData
  case mact of
    Nothing -> return ()
    Just cancelAction -> liftIO $ cancelAction

exitNotificationHandler :: Handler J.Exit
exitNotificationHandler = Handler $ \_ _ -> do
  logm $ B.pack "haskell-lsp:Got exit, exiting"
  exitSuccess

-- | Default Shutdown handler
shutdownRequestHandler :: Handler J.Shutdown
shutdownRequestHandler = Handler $ \_req (ClientResponseHandler k) -> do
  k $ Right J.Empty

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
publishDiagnostics :: Int -> NormalizedUri -> TextDocumentVersion -> DiagnosticsBySource -> LspM config ()
publishDiagnostics maxDiagnosticCount uri version diags = join $ stateData $ \ctx ->
  let ds = updateDiagnostics (resDiagnostics ctx) uri version diags
      ctx' = ctx{resDiagnostics = ds}
      mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
      act = case mdp of
        Nothing -> return ()
        Just params ->
          sendToClient $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params
      in (act,ctx')

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
flushDiagnosticsBySource :: Int -> Maybe DiagnosticSource -> LspM config ()
flushDiagnosticsBySource maxDiagnosticCount msource = join $ stateData $ \ctx ->
  let ds = flushBySource (resDiagnostics ctx) msource
      ctx' = ctx {resDiagnostics = ds}
      -- Send the updated diagnostics to the client
      act = forM_ (HM.keys ds) $ \uri -> do
        let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
        case mdp of
          Nothing -> return ()
          Just params -> do
            sendToClient $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params
      in (act,ctx')

-- =====================================================================
--
--  utility


--
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
