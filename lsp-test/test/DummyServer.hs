{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DummyServer where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson hiding (Null, defaultOptions)
import Data.Aeson qualified as J
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.String
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.Directory
import System.FilePath
import System.IO
import System.Process
import UnliftIO
import UnliftIO.Concurrent

withDummyServer :: ((Handle, Handle) -> IO ()) -> IO ()
withDummyServer f = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  handlerEnv <- HandlerEnv <$> newEmptyMVar <*> newEmptyMVar
  let
    definition =
      ServerDefinition
        { doInitialize = \env _req -> pure $ Right env
        , defaultConfig = 1 :: Int
        , configSection = "dummy"
        , parseConfig = \_old new -> case fromJSON new of
            J.Success v -> Right v
            J.Error err -> Left $ T.pack err
        , onConfigChange = const $ pure ()
        , staticHandlers = \_caps -> handlers
        , interpretHandler = \env ->
            Iso (\m -> runLspT env (runReaderT m handlerEnv)) liftIO
        , options = defaultOptions{optExecuteCommandCommands = Just ["doAnEdit"]}
        }

  bracket
    (forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite definition)
    killThread
    (const $ f (hinWrite, houtRead))

data HandlerEnv = HandlerEnv
  { relRegToken :: MVar (RegistrationToken Method_WorkspaceDidChangeWatchedFiles)
  , absRegToken :: MVar (RegistrationToken Method_WorkspaceDidChangeWatchedFiles)
  }

handlers :: Handlers (ReaderT HandlerEnv (LspM Int))
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $
        \_noti ->
          sendNotification SMethod_WindowLogMessage $
            LogMessageParams MessageType_Log "initialized"
    , requestHandler (SMethod_CustomMethod (Proxy @"getConfig")) $ \_req resp -> do
        config <- getConfig
        resp $ Right $ toJSON config
    , requestHandler SMethod_TextDocumentHover $
        \_req responder ->
          responder $
            Right $
              InL $
                Hover (InL (MarkupContent MarkupKind_PlainText "hello")) Nothing
    , requestHandler SMethod_TextDocumentDocumentSymbol $
        \_req responder ->
          responder $
            Right $
              InR $
                InL
                  [ DocumentSymbol
                      "foo"
                      Nothing
                      SymbolKind_Object
                      Nothing
                      Nothing
                      (mkRange 0 0 3 6)
                      (mkRange 0 0 3 6)
                      Nothing
                  ]
    , notificationHandler SMethod_TextDocumentDidOpen $
        \noti -> do
          let TNotificationMessage _ _ (DidOpenTextDocumentParams doc) = noti
              TextDocumentItem uri _ _ _ = doc
              Just fp = uriToFilePath uri
              diag =
                Diagnostic
                  (mkRange 0 0 0 1)
                  (Just DiagnosticSeverity_Warning)
                  (Just (InL 42))
                  Nothing
                  (Just "dummy-server")
                  "Here's a warning"
                  Nothing
                  Nothing
                  Nothing
          withRunInIO $
            \runInIO -> do
              when (".hs" `isSuffixOf` fp) $
                void $
                  forkIO $
                    do
                      threadDelay (2 * 10 ^ 6)
                      runInIO $
                        sendNotification SMethod_TextDocumentPublishDiagnostics $
                          PublishDiagnosticsParams uri Nothing [diag]
              -- also act as a registerer for workspace/didChangeWatchedFiles
              when (".register" `isSuffixOf` fp) $
                do
                  let regOpts =
                        DidChangeWatchedFilesRegistrationOptions
                          [ FileSystemWatcher
                              (GlobPattern $ InL $ Pattern "*.watch")
                              (Just WatchKind_Create)
                          ]
                  Just token <- runInIO $
                    registerCapability mempty SMethod_WorkspaceDidChangeWatchedFiles regOpts $
                      \_noti ->
                        sendNotification SMethod_WindowLogMessage $
                          LogMessageParams MessageType_Log "got workspace/didChangeWatchedFiles"
                  runInIO $ asks relRegToken >>= \v -> putMVar v token
              when (".register.abs" `isSuffixOf` fp) $
                do
                  curDir <- getCurrentDirectory
                  let regOpts =
                        DidChangeWatchedFilesRegistrationOptions
                          [ FileSystemWatcher
                              (GlobPattern $ InL $ Pattern $ fromString $ curDir </> "*.watch")
                              (Just WatchKind_Create)
                          ]
                  Just token <- runInIO $
                    registerCapability mempty SMethod_WorkspaceDidChangeWatchedFiles regOpts $
                      \_noti ->
                        sendNotification SMethod_WindowLogMessage $
                          LogMessageParams MessageType_Log "got workspace/didChangeWatchedFiles"
                  runInIO $ asks absRegToken >>= \v -> putMVar v token
              -- also act as an unregisterer for workspace/didChangeWatchedFiles
              when (".unregister" `isSuffixOf` fp) $
                do
                  Just token <- runInIO $ asks relRegToken >>= tryReadMVar
                  runInIO $ unregisterCapability token
              when (".unregister.abs" `isSuffixOf` fp) $
                do
                  Just token <- runInIO $ asks absRegToken >>= tryReadMVar
                  runInIO $ unregisterCapability token
    , -- this handler is used by the
      -- "text document VFS / sends back didChange notifications (documentChanges)" test
      notificationHandler SMethod_TextDocumentDidChange $ \noti -> do
        let TNotificationMessage _ _ params = noti
        void $ sendNotification (SMethod_CustomMethod (Proxy @"custom/textDocument/didChange")) (toJSON params)
    , requestHandler SMethod_WorkspaceExecuteCommand $ \req resp -> do
        case req of
          TRequestMessage _ _ _ (ExecuteCommandParams Nothing "doAnEdit" (Just [val])) -> do
            let
              Success docUri = fromJSON val
              edit = [TextEdit (mkRange 0 0 0 5) "howdy"]
              params =
                ApplyWorkspaceEditParams (Just "Howdy edit") $
                  WorkspaceEdit (Just (M.singleton docUri edit)) Nothing Nothing
            resp $ Right $ InR $ Null
            void $ sendRequest SMethod_WorkspaceApplyEdit params (const (pure ()))
          TRequestMessage _ _ _ (ExecuteCommandParams Nothing "doAVersionedEdit" (Just [val])) -> do
            let
              Success versionedDocUri = fromJSON val
              edit = [InL (TextEdit (mkRange 0 0 0 5) "howdy")]
              documentEdit = TextDocumentEdit versionedDocUri edit
              params =
                ApplyWorkspaceEditParams (Just "Howdy edit") $
                  WorkspaceEdit Nothing (Just [InL documentEdit]) Nothing
            resp $ Right $ InR Null
            void $ sendRequest SMethod_WorkspaceApplyEdit params (const (pure ()))
          TRequestMessage _ _ _ (ExecuteCommandParams _ name _) ->
            error $ "unsupported command: " <> show name
    , requestHandler SMethod_TextDocumentCodeAction $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            CodeActionParams _ _ _ _ cactx = params
            CodeActionContext diags _ _ = cactx
            codeActions = fmap diag2ca diags
            diag2ca d =
              CodeAction
                "Delete this"
                Nothing
                (Just [d])
                Nothing
                Nothing
                Nothing
                (Just (Command "" "deleteThis" Nothing))
                Nothing
        resp $ Right $ InL $ InR <$> codeActions
    , requestHandler SMethod_TextDocumentCompletion $ \_req resp -> do
        let res = CompletionList True Nothing [item]
            item =
              CompletionItem
                "foo"
                Nothing
                (Just CompletionItemKind_Constant)
                (Just [])
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
        resp $ Right $ InR $ InL res
    , requestHandler SMethod_TextDocumentPrepareCallHierarchy $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            CallHierarchyPrepareParams _ pos _ = params
            Position x y = pos
            item =
              CallHierarchyItem
                "foo"
                SymbolKind_Method
                Nothing
                Nothing
                (Uri "")
                (Range (Position 2 3) (Position 4 5))
                (Range (Position 2 3) (Position 4 5))
                Nothing
        if x == 0 && y == 0
          then resp $ Right $ InR Null
          else resp $ Right $ InL [item]
    , requestHandler SMethod_CallHierarchyIncomingCalls $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            CallHierarchyIncomingCallsParams _ _ item = params
        resp $
          Right $
            InL
              [CallHierarchyIncomingCall item [Range (Position 2 3) (Position 4 5)]]
    , requestHandler SMethod_CallHierarchyOutgoingCalls $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            CallHierarchyOutgoingCallsParams _ _ item = params
        resp $
          Right $
            InL
              [CallHierarchyOutgoingCall item [Range (Position 4 5) (Position 2 3)]]
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_req resp -> do
        let tokens = makeSemanticTokens defaultSemanticTokensLegend [SemanticTokenAbsolute 0 1 2 SemanticTokenTypes_Type []]
        case tokens of
          Left t -> resp $ Left $ TResponseError (InR ErrorCodes_InternalError) t Nothing
          Right tokens -> resp $ Right $ InL tokens
    , requestHandler SMethod_TextDocumentInlayHint $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            InlayHintParams _ _ (Range start end) = params
            ih =
              InlayHint
                end
                (InL ":: Text")
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (Just $ toJSON start)
        resp $ Right $ InL [ih]
    , requestHandler SMethod_InlayHintResolve $ \req resp -> do
        let TRequestMessage _ _ _ params = req
            (InlayHint{_data_ = Just data_, ..}) = params
            start :: Position
            Success start = fromJSON data_
            ih = InlayHint{_data_ = Nothing, _tooltip = Just $ InL $ "start at " <> T.pack (show start), ..}
        resp $ Right ih
    ]
