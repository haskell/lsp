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
  , ServerMessageFunc
  , SendFunc
  , SomeServerMessageWithResponse(..)
  , Handlers
  , Options(..)
  , ClientResponseHandler(..)
  , ServerResponseHandler(..)
  , defaultLanguageContextData
  , makeResponseMessage
  , makeResponseError
  , setupLogger
  , reverseSortEdit
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Monad
import           Control.Applicative
import           Data.Functor.Product
import           Control.Monad.IO.Class
import           Control.Lens ( (<&>), (^.), (^?), _Just )
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import           Data.IxMap
import           Data.Scientific (floatingOrInteger)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid hiding (Product)
import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Constant
import           Language.Haskell.LSP.Types.MessageFuncs
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
type SendFunc = J.FromServerMessage -> IO ()

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData config =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resHandlers            :: !Handlers
  , resOptions             :: !Options
  , resSendResponse        :: !SendFunc
  , resVFS                 :: !VFSData
  , resDiagnostics         :: !DiagnosticStore
  , resConfig              :: !(Maybe config)
  , resLspId               :: !(TVar Int)
  , resLspFuncs            :: LspFuncs config -- NOTE: Cannot be strict, lazy initialization
  , resWorkspaceFolders    :: ![J.WorkspaceFolder]
  , resProgressData        :: !ProgressData
  , resPendingResponses    :: !(IxMap J.LspId (Product J.SMethod ServerResponseHandler))
  }

data ProgressData = ProgressData { progressNextId :: !Int
                                 , progressCancel :: !(Map.Map J.ProgressToken (IO ())) }

data VFSData =
  VFSData
    { vfsData :: !VFS
    , reverseMap :: !(Map.Map FilePath FilePath)
    }

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
    , codeActionKinds                  :: Maybe [J.CodeActionKind]
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

data SomeServerMessageWithResponse where
  SomeServerMessageWithResponse
    :: (J.ToJSON (J.ServerMessage m))
    => J.SServerMethod m
    -> J.ServerMessage m
    -> ServerResponseHandler m
    -> SomeServerMessageWithResponse

type ServerMessageFunc = SomeServerMessageWithResponse -> IO ()

-- | Returned to the server on startup, providing ways to interact with the client.
data LspFuncs c =
  LspFuncs
    { clientCapabilities           :: !C.ClientCapabilities
    , config                       :: !(IO (Maybe c))
      -- ^ Derived from the DidChangeConfigurationNotification message via a
      -- server-provided function.
    , sendFunc                     :: !ServerMessageFunc
    , getVirtualFileFunc           :: !(J.NormalizedUri -> IO (Maybe VirtualFile))
    , getVirtualFilesFunc          :: !(IO VFS)
      -- ^ Function to return the 'VirtualFile' associated with a
      -- given 'NormalizedUri', if there is one.
    , persistVirtualFileFunc       :: !(J.NormalizedUri -> IO (Maybe FilePath))
    , reverseFileMapFunc           :: !(IO (FilePath -> FilePath))
    , publishDiagnosticsFunc       :: !PublishDiagnosticsFunc
    , flushDiagnosticsBySourceFunc :: !FlushDiagnosticsBySourceFunc
    , getNextReqId                 :: !(IO Int)
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

data ClientResponseHandler (m :: J.Method J.FromClient t) where
  ClientResponseHandler :: J.ResponseHandlerFunc m -> ClientResponseHandler m

data ServerResponseHandler (m :: J.Method J.FromServer t) where
  ServerResponseHandler :: J.ResponseHandlerFunc m -> ServerResponseHandler m

mkClientResponseHandler :: J.SClientMethod m -> J.ClientMessage m -> TVar (LanguageContextData config) -> ClientResponseHandler m
mkClientResponseHandler m cm tvarDat =
  case J.splitClientMethod m of
    J.IsClientNot -> ClientResponseHandler ()
    J.IsClientReq -> ClientResponseHandler $ \mrsp -> case mrsp of
      Left err -> sendErrorResponseE tvarDat m (cm ^. J.id) err
      Right rsp -> sendToClient tvarDat $ J.FromServerRsp m $ makeResponseMessage (cm ^. J.id) rsp
    J.IsClientEither -> ClientResponseHandler $ case cm of
      J.NotMess _ -> Nothing
      J.ReqMess req -> Just $ \mrsp -> case mrsp of
        Left err -> sendErrorResponseE tvarDat m (req ^. J.id) err
        Right rsp -> sendToClient tvarDat $ J.FromServerRsp m $ makeResponseMessage (req ^. J.id) rsp

-- | Return value signals if response handler was inserted succesfully
-- Might fail if the id was already in the map
addResponseHandler :: TVar (LanguageContextData config) -> J.LspId m -> (Product J.SMethod ServerResponseHandler) m -> IO Bool
addResponseHandler tv lid h = atomically $ stateTVar tv $ \ctx@LanguageContextData{resPendingResponses} ->
  case insertIxMap lid h resPendingResponses of
    Just m -> (True,ctx { resPendingResponses = m})
    Nothing -> (False, ctx)

mkServerRequestFunc :: TVar (LanguageContextData config) -> SomeServerMessageWithResponse -> IO ()
mkServerRequestFunc tvarDat (SomeServerMessageWithResponse m msg resHandler) =
  case J.splitServerMethod m of
    J.IsServerNot -> sendToClient tvarDat $ J.fromServerNot msg
    J.IsServerReq -> do
      success <- addResponseHandler tvarDat (msg ^. J.id) (Pair m resHandler)
      if success
      then sendToClient tvarDat $ J.fromServerReq msg
      else do
        let mess = T.pack $
              unwords ["haskell-lsp: could not send FromServer request as id is reused"
                      , show (msg ^. J.id), show $ J.toJSON msg]
        sendErrorLog tvarDat mess
    J.IsServerEither -> case msg of
      J.NotMess _ -> sendToClient tvarDat $ J.FromServerMess m msg
      J.ReqMess req -> do
        success <- addResponseHandler tvarDat (req ^. J.id) (Pair m resHandler)
        if success
        then sendToClient tvarDat $ J.FromServerMess m msg
        else do
          let mess = T.pack $
                unwords ["haskell-lsp: could not send FromServer request as id is reused"
                        , show (req ^. J.id), show req]
          sendErrorLog tvarDat mess


-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
newtype Handler m = Handler {runHandler :: J.ClientMessage m -> ClientResponseHandler m -> IO ()}
type Handlers = forall t (m :: J.Method J.FromClient t). J.SMethod m -> Maybe (Handler m)

-- ---------------------------------------------------------------------
nop :: Maybe (LSPVar config -> b -> IO ())
nop = Nothing

handlerMap :: (Show config) => InitializeCallbacks config
           -> Handlers -> J.SomeClientMethod -> (TVar (LanguageContextData config) -> J.Value -> IO ())
handlerMap InitializeCallbacks{..} hm (J.SomeClientMethod c)
  = case c of
  J.SInitialize -> \tv -> hh c (Just $ handleConfigChange onInitialConfiguration) (Just $ initializeRequestHandler' onStartup h tv) tv
  J.SShutdown -> hh c nop (h <|> Just shutdownRequestHandler) -- use shutdownRequestHandler as a default
  J.SExit -> hh c nop (h <|> Just exitNotificationHandler)
  J.SWorkspaceDidChangeWorkspaceFolders -> hh c (Just updateWorkspaceFolders) h
  J.SWorkspaceDidChangeConfiguration -> hh c (Just $ handleConfigChange onConfigurationChange) h
  J.STextDocumentDidOpen -> hh c (Just $ vfsFunc openVFS) h
  J.STextDocumentDidChange -> hh c (Just $ vfsFunc changeFromClientVFS) h
  J.STextDocumentDidClose -> hh c (Just $ vfsFunc closeVFS) h
  J.SWorkDoneProgressCancel -> hh c (Just progressCancelHandler) h
  _ -> \tvar json -> case J.splitClientMethod c of
    J.IsClientReq -> hh c nop h tvar json
    J.IsClientNot -> hh c nop h tvar json
    J.IsClientEither
      | J.Object v <- json
      , HM.member "id" v -- Request
        -> let m' = (J.SCustomMethod m :: J.SMethod (J.CustomMethod :: J.Method J.FromClient J.Request))
               h' = hm m'
             in hh m' nop h' tvar json
      | otherwise -- Notification
        -> let m' = (J.SCustomMethod m :: J.SMethod (J.CustomMethod :: J.Method J.FromClient J.Notification))
               h' = hm m'
             in hh m' nop h' tvar json
      where
        J.SCustomMethod m = c
  where h = hm c

-- ---------------------------------------------------------------------

type LSPVar config = TVar (LanguageContextData config)

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: forall m b config. (J.FromJSON b, J.ToJSON b, b ~ J.ClientMessage m)
   => J.SClientMethod m -> Maybe (LSPVar config -> b -> IO ()) -> Maybe (Handler m)
   -> LSPVar config -> J.Value -> IO ()
hh m mAction mh tvarDat json = do
      case J.fromJSON json of
        J.Success req -> do
          maybe (return ()) (\f -> f tvarDat req) mAction
          case mh of
            Just h -> runHandler h req (mkClientResponseHandler m req tvarDat)
            Nothing
              -- '$/' notifications should/could be ignored by server.
              -- Don't log errors in that case.
              -- See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#-notifications-and-requests.
              | isOptionalNotification m -> return ()
              | otherwise -> do
                  let msg = T.pack $ unwords ["haskell-lsp:no handler for.", show json]
                  sendErrorLog tvarDat msg
        J.Error  err -> do
          let msg = T.pack $ unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
          sendErrorLog tvarDat msg
  where
    isOptionalNotification (J.SCustomMethod method)
      | "$/" `T.isPrefixOf` method = True
    isOptionalNotification _  = False

handleConfigChange :: (Show a,Show err) =>  (a -> Either err config) -> LSPVar config -> a -> IO ()
handleConfigChange parseConfig tvarDat req =
  case parseConfig req of
    Left err -> do
      let msg = T.pack $ unwords
            ["haskell-lsp:configuration parse error.", show req, show err]
      sendErrorLog tvarDat msg
    Right newConfig ->
      atomically $ modifyTVar' tvarDat (\ctx' -> ctx' { resConfig = Just newConfig })

vfsFunc :: (VFS -> b -> (VFS, [String])) -> LSPVar config -> b -> IO ()
vfsFunc modifyVfs tvarDat req =
  join $ atomically $ modifyVFSData tvarDat $ \(VFSData vfs rm) ->
    let (vfs', ls) = modifyVfs vfs req
    in (VFSData vfs' rm, mapM_ logs ls)


-- | Updates the list of workspace folders
updateWorkspaceFolders :: LSPVar config -> J.Message J.WorkspaceDidChangeWorkspaceFolders -> IO ()
updateWorkspaceFolders tvarDat (J.NotificationMessage _ _ params) = atomically $ do
  oldWfs <- resWorkspaceFolders <$> readTVar tvarDat
  let J.List toRemove = params ^. J.event . J.removed
      wfs0 = foldr L.delete oldWfs toRemove
      J.List toAdd = params ^. J.event . J.added
      wfs1 = wfs0 <> toAdd
  modifyTVar' tvarDat (\c -> c {resWorkspaceFolders = wfs1})

-- ---------------------------------------------------------------------

modifyVFSData :: TVar (LanguageContextData config) -> (VFSData -> (VFSData, a)) -> STM a
modifyVFSData tvarDat f = do
  (vfs', a) <- f . resVFS <$> readTVar tvarDat
  modifyTVar tvarDat $ \vd -> vd { resVFS = vfs' }
  return a

-- ---------------------------------------------------------------------

-- | Return the 'VirtualFile' associated with a given 'NormalizedUri', if there is one.
getVirtualFile :: TVar (LanguageContextData config) -> J.NormalizedUri -> IO (Maybe VirtualFile)
getVirtualFile tvarDat uri = Map.lookup uri . vfsMap . vfsData . resVFS <$> readTVarIO tvarDat

getVirtualFiles :: TVar (LanguageContextData config) -> IO VFS
getVirtualFiles tvarDat = vfsData . resVFS <$> readTVarIO tvarDat

-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: TVar (LanguageContextData config) -> J.NormalizedUri -> IO (Maybe FilePath)
persistVirtualFile tvarDat uri = join $ atomically $ do
  st <- readTVar tvarDat
  let vfs_data = resVFS st
      cur_vfs = vfsData vfs_data
      revMap = reverseMap vfs_data

  case persistFileVFS cur_vfs uri of
    Nothing -> return (return Nothing)
    Just (fn, write) -> do
      let revMap' =
        -- TODO: Does the VFS make sense for URIs which are not files?
        -- The reverse map should perhaps be (FilePath -> URI)
            case J.uriToFilePath (J.fromNormalizedUri uri) of
              Just uri_fp -> Map.insert fn uri_fp revMap
              Nothing -> revMap

      modifyVFSData tvarDat (\d -> (d { reverseMap = revMap' }, ()))
      return ((Just fn) <$ write)

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: TVar (LanguageContextData config)
               -> IO (FilePath -> FilePath)
reverseFileMap tvarDat = do
    vfs <- resVFS <$> readTVarIO tvarDat
    let f fp = fromMaybe fp . Map.lookup fp . reverseMap $ vfs
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
_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new haskell-lsp."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=xxxxxxxxxxxxxxx"
               ]


-- |
--
--
defaultLanguageContextData :: Handlers -> Options -> LspFuncs config -> TVar Int -> SendFunc -> VFS -> LanguageContextData config
defaultLanguageContextData h o lf tv sf vfs =
  LanguageContextData _INITIAL_RESPONSE_SEQUENCE h o sf (VFSData vfs mempty) mempty
                      Nothing tv lf mempty defaultProgressData emptyIxMap

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
          case HM.lookup "id" o of
            Just (J.Number (floatingOrInteger @Double -> Right i)) -> handleResponse (Left i) o

            Just (J.String s) -> handleResponse (Right s) o
            _ -> do
               let msg = T.pack $ unwords ["haskell-lsp: could not decode id field of response ", lbs2str jsonStr]
               sendErrorLog tvarDat msg
  where
    handleResponse :: Either Int Text -> J.Object -> IO ()
    handleResponse baseId resobj = do
      resHandler <- atomically $ do
        ctx <- readTVar tvarDat
        let (handler, newMap) = pickFromIxMap' baseId (resPendingResponses ctx)
        modifyTVar' tvarDat (\c -> c { resPendingResponses = newMap })
        return handler
      case resHandler of
        Nothing -> sendErrorLog tvarDat $ T.pack $ "haskell-lsp: No handler for " ++ (show resobj)
        Just (Pair (m :: J.SMethod m) (ServerResponseHandler f)) -> case J.splitServerMethod m of
          J.IsServerReq -> case J.fromJSON $ J.Object resobj of
            J.Error e -> do
               let msg = T.pack $ unwords ["haskell-lsp: got error while decoding response:", show e, "in", show resobj]
               sendErrorLog tvarDat msg
               f (Left $ J.ResponseError J.ParseError msg Nothing)
            J.Success (res :: J.ResponseMessage m) -> case res ^. J.error of
              Just err -> f (Left err)
              Nothing -> case res ^. J.result of
                Nothing -> do
                  let msg = T.pack $ unwords ["haskell-lsp: Got neither a result nor an error in response: ", show resobj]
                  sendErrorLog tvarDat msg
                  f (Left $ J.ResponseError J.ParseError msg Nothing)
                Just result -> f (Right result)
          J.IsServerEither -> case f of
            Nothing ->
              sendErrorLog tvarDat $
                T.pack $ "haskell-lsp: No handler for "
                      ++ (show resobj) ++ " with method" ++ (show m)
            Just f' ->  case J.fromJSON $ J.Object resobj of
              J.Error e -> do
                 let msg = T.pack $ unwords ["haskell-lsp: got error while decoding response:", show e, "in", show resobj]
                 sendErrorLog tvarDat msg
                 f' (Left $ J.ResponseError J.ParseError msg Nothing)
              J.Success (res :: J.ResponseMessage m) -> case res ^. J.error of
                Just err -> f' (Left err)
                Nothing -> case res ^. J.result of
                  Just result -> f' (Right result)
                  Nothing -> do
                    let msg = T.pack $ unwords ["haskell-lsp: Got neither a result nor an error in response: ", show res]
                    sendErrorLog tvarDat msg
                    f' (Left $ J.ResponseError J.ParseError msg Nothing)
    -- capability based handlers
    handle json cmd = do
      ctx <- readTVarIO tvarDat
      handlerMap dispatcherProc (resHandlers ctx) cmd tvarDat json

-- ---------------------------------------------------------------------

makeResponseMessage :: J.LspId m -> J.ResponseParams m -> J.ResponseMessage m
makeResponseMessage rid result = J.ResponseMessage "2.0" (Just rid) (Just result) Nothing

makeResponseError :: J.LspId m -> J.ResponseError -> J.ResponseMessage m
makeResponseError origId err = J.ResponseMessage "2.0" (Just origId) Nothing (Just err)

-- ---------------------------------------------------------------------

sendToClient :: TVar (LanguageContextData config) -> J.FromServerMessage -> IO ()
sendToClient tvarCtx msg = do
  ctx <- readTVarIO tvarCtx
  resSendResponse ctx msg


-- ---------------------------------------------------------------------

sendErrorResponseE
  :: forall (m :: J.Method J.FromClient J.Request) config.
     TVar (LanguageContextData config)
  -> J.SMethod m -> J.LspId (m :: J.Method J.FromClient J.Request) -> J.ResponseError -> IO ()
sendErrorResponseE sf m origId err = do
  sendToClient sf $ J.FromServerRsp m (J.ResponseMessage "2.0" (Just origId) Nothing (Just err))

sendErrorLog :: TVar (LanguageContextData config) -> Text -> IO ()
sendErrorLog tv msg =
  sendToClient tv $ J.fromServerNot $ fmServerLogMessageNotification J.MtError msg

-- ---------------------------------------------------------------------

initializeErrorHandler :: J.ResponseHandlerFunc J.Initialize -> E.SomeException -> IO ()
initializeErrorHandler sendResp e =
    sendResp $ Left $ J.ResponseError J.InternalError msg Nothing
  where
    msg = T.pack $ unwords ["Error on initialize:", show e]

-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler'
  :: (Show config)
  => (LspFuncs config -> IO (Maybe J.ResponseError))
  -> Maybe (Handler J.Initialize)
  -> TVar (LanguageContextData config)
  -> Handler J.Initialize
initializeRequestHandler' onStartup mHandler tvarCtx = Handler $ \req rspH@(ClientResponseHandler sendResp) ->
  flip E.catch (initializeErrorHandler sendResp) $ do

    let params = req ^. J.params

    case mHandler of
      Just handler -> runHandler handler req rspH
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
      getLspId tvId = atomically $ do
        cid <- readTVar tvId
        modifyTVar' tvId (+1)
        return cid

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

      storeProgress :: J.ProgressToken -> Async a -> IO ()
      storeProgress n a = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let pc = progressCancel pd
            pc' = Map.insert n (cancelWith a ProgressCancelledException) pc
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressCancel = pc' }})

      deleteProgress :: J.ProgressToken -> IO ()
      deleteProgress n = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let x = progressCancel pd
            x' = Map.delete n x
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressCancel = x' }})

      -- Get a new id for the progress session and make a new one
      getNewProgressId :: IO J.ProgressToken
      getNewProgressId = liftIO $ atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let x = progressNextId pd
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressNextId = x + 1 }})
        return $ J.ProgressNumericToken x

      withProgressBase :: Bool -> (Text -> ProgressCancellable
                    -> ((Progress -> IO ()) -> IO a) -> IO a)
      withProgressBase indefinite title cancellable f
        | clientSupportsProgress = do
          let sf = sendToClient tvarCtx

          progId <- getNewProgressId

          let initialPercentage
                | indefinite = Nothing
                | otherwise = Just 0
              cancellable' = case cancellable of
                              Cancellable -> True
                              NotCancellable -> False

          rId <- getLspId $ resLspId ctx0

          -- Create progress token
          liftIO $ sf $ J.fromServerReq $
            fmServerWorkDoneProgressCreateRequest (J.IdInt rId) $ J.WorkDoneProgressCreateParams progId

          -- Send initial notification
          liftIO $ sf $ J.fromServerNot $ fmServerWorkDoneProgressBeginNotification $
            J.ProgressParams progId $
            J.WorkDoneProgressBeginParams title (Just cancellable') Nothing initialPercentage

          aid <- async $ f (updater progId (sf . J.fromServerNot))
          storeProgress progId aid
          res <- wait aid

          -- Send done notification
          liftIO $ sf $ J.fromServerNot $ fmServerWorkDoneProgressEndNotification $
            J.ProgressParams progId $
            J.WorkDoneProgressEndParams Nothing
          -- Delete the progress cancellation from the map
          -- If we don't do this then it's easy to leak things as the map contains any IO action.
          deleteProgress progId


          return res
        | otherwise = f (const $ return ())
          where updater progId sf (Progress percentage msg) =
                  sf $ fmServerWorkDoneProgressReportNotification $
                    J.ProgressParams progId $
                    J.WorkDoneProgressReportParams Nothing msg percentage

      withProgress' :: Text -> ProgressCancellable -> ((Progress -> IO ()) -> IO a) -> IO a
      withProgress' = withProgressBase False

      withIndefiniteProgress' :: Text -> ProgressCancellable -> IO a -> IO a
      withIndefiniteProgress' title cancellable f =
        withProgressBase True title cancellable (const f)

    -- Launch the given process once the project root directory has been set
    let lspFuncs = LspFuncs (params ^. J.capabilities)
                            (getConfig tvarCtx)
                            (mkServerRequestFunc tvarCtx)
                            (getVirtualFile tvarCtx)
                            (getVirtualFiles tvarCtx)
                            (persistVirtualFile tvarCtx)
                            (reverseFileMap tvarCtx)
                            (publishDiagnostics tvarCtx)
                            (flushDiagnosticsBySource tvarCtx)
                            (getLspId $ resLspId ctx0)
                            rootDir
                            (getWfs tvarCtx)
                            withProgress'
                            withIndefiniteProgress'
    atomically $ modifyTVar tvarCtx (\cur_ctx -> cur_ctx { resLspFuncs = lspFuncs })

    ctx <- readTVarIO tvarCtx

    initializationResult <- onStartup lspFuncs

    case initializationResult of
      Just errResp -> do
        sendResp $ Left errResp

      Nothing -> do
        let capa = serverCapabilities (params ^. J.capabilities) (resOptions ctx) (resHandlers ctx)
        sendResp $ Right (J.InitializeResponseCapabilities capa)

-- | Infers the capabilities based on registered handlers, and sets the appropriate options.
-- A provider should be set to Nothing if the server does not support it, unless it is a
-- static option.
serverCapabilities :: C.ClientCapabilities -> Options -> Handlers -> J.InitializeResponseCapabilitiesInner
serverCapabilities clientCaps o h =
  J.InitializeResponseCapabilitiesInner
    { J._textDocumentSync                 = sync
    , J._hoverProvider                    = supported J.STextDocumentHover
    , J._completionProvider               = completionProvider
    , J._signatureHelpProvider            = signatureHelpProvider
    , J._definitionProvider               = supported J.STextDocumentDefinition
    , J._typeDefinitionProvider           = Just $ J.GotoOptionsStatic $ supported_b J.STextDocumentTypeDefinition
    , J._implementationProvider           = Just $ J.GotoOptionsStatic $ supported_b J.STextDocumentImplementation
    , J._referencesProvider               = supported J.STextDocumentReferences
    , J._documentHighlightProvider        = supported J.STextDocumentDocumentHighlight
    , J._documentSymbolProvider           = supported J.STextDocumentDocumentSymbol
    , J._workspaceSymbolProvider          = supported J.SWorkspaceSymbol
    , J._codeActionProvider               = codeActionProvider
    , J._codeLensProvider                 = supported' J.STextDocumentCodeLens $ J.CodeLensOptions $
                                              supported J.SCodeLensResolve
    , J._documentFormattingProvider       = supported J.STextDocumentFormatting
    , J._documentRangeFormattingProvider  = supported J.STextDocumentRangeFormatting
    , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , J._renameProvider                   = Just $ J.RenameOptionsStatic $ supported_b J.STextDocumentRename
    , J._documentLinkProvider             = supported' J.STextDocumentDocumentLink $ J.DocumentLinkOptions $
                                              supported J.SDocumentLinkResolve
    , J._colorProvider                    = Just $ J.ColorOptionsStatic $ supported_b J.STextDocumentDocumentColor
    , J._foldingRangeProvider             = Just $ J.FoldingRangeOptionsStatic $ supported_b J.STextDocumentFoldingRange
    , J._executeCommandProvider           = executeCommandProvider
    , J._workspace                        = Just workspace
    -- TODO: Add something for experimental
    , J._experimental                     = Nothing :: Maybe J.Value
    }
  where

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
            (supported J.SCompletionItemResolve)
            (map singleton <$> completionTriggerCharacters o)
            (map singleton <$> completionAllCommitCharacters o)
      | otherwise = Nothing

    clientSupportsCodeActionKinds = isJust $
      clientCaps ^? J.textDocument . _Just . J.codeAction . _Just . J.codeActionLiteralSupport

    codeActionProvider
      | clientSupportsCodeActionKinds
      , supported_b J.STextDocumentCodeAction = Just $ maybe (J.CodeActionOptionsStatic True) (J.CodeActionOptions . Just) (codeActionKinds o)
      | supported_b J.STextDocumentCodeAction = Just (J.CodeActionOptionsStatic True)
      | otherwise = Just (J.CodeActionOptionsStatic False)

    signatureHelpProvider
      | supported_b J.STextDocumentSignatureHelp = Just $
          J.SignatureHelpOptions
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

    workspace = J.WorkspaceOptions workspaceFolder
    workspaceFolder = supported' J.SWorkspaceDidChangeWorkspaceFolders $
        -- sign up to receive notifications
        J.WorkspaceFolderOptions (Just True) (Just (J.WorkspaceFolderChangeNotificationsBool True))

progressCancelHandler :: TVar (LanguageContextData config) -> J.WorkDoneProgressCancelNotification -> IO ()
progressCancelHandler tvarCtx (J.NotificationMessage _ _ (J.WorkDoneProgressCancelParams tid)) = do
  mact <- Map.lookup tid . progressCancel . resProgressData <$> readTVarIO tvarCtx
  case mact of
    Nothing -> return ()
    Just cancelAction -> cancelAction

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
publishDiagnostics :: TVar (LanguageContextData config) -> PublishDiagnosticsFunc
publishDiagnostics tvarDat maxDiagnosticCount uri version diags = do
  join $ atomically $ do
    ctx <- readTVar tvarDat
    let ds = updateDiagnostics (resDiagnostics ctx) uri version diags
    writeTVar tvarDat $ ctx{resDiagnostics = ds}
    let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
    return $ case mdp of
      Nothing -> return ()
      Just params ->
        resSendResponse ctx $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
flushDiagnosticsBySource :: TVar (LanguageContextData config) -> FlushDiagnosticsBySourceFunc
flushDiagnosticsBySource tvarDat maxDiagnosticCount msource = join $ atomically $ do
  -- logs $ "haskell-lsp:flushDiagnosticsBySource:source=" ++ show source
  ctx <- readTVar tvarDat
  let ds = flushBySource (resDiagnostics ctx) msource
  writeTVar tvarDat $ ctx {resDiagnostics = ds}
  -- Send the updated diagnostics to the client
  return $ forM_ (HM.keys ds) $ \uri -> do
    -- logs $ "haskell-lsp:flushDiagnosticsBySource:uri=" ++ show uri
    let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
    case mdp of
      Nothing -> return ()
      Just params -> do
        resSendResponse ctx $ J.fromServerNot $ J.NotificationMessage "2.0" J.STextDocumentPublishDiagnostics params

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
