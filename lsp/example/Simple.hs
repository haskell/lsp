{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC qualified as TRPC
import JSONRPC.Typed.Server hiding (ServerDefinition)
import Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Server.Core
import Language.LSP.Server.DynamicRegistration as Registration

handlers :: LanguageServerHandle () -> Handlers Server LSP.Method
handlers h =
  mconcat
    [ notificationHandler SMethod_Initialized $ mkNotificationHandler $ \_p -> do
        let params =
              ShowMessageRequestParams
                MessageType_Info
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        res <- requestAwaitResponse h.serverHandle SMethod_WindowShowMessageRequest params
        case res of
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

            _ <- Registration.registerCapability
              h.registrationHandle
              SMethod_TextDocumentCodeLens
              regOpts
              $ requestHandler SMethod_TextDocumentCodeLens
              $ mkRequestHandler
              $ \_req -> do
                let cmd = Command "Say hello" "lsp-hello-command" Nothing
                    rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
                pure $ Right $ InL rsp
            pure ()
          Right _ ->
            TRPC.sendNotification h.serverHandle.rpcHandle SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            TRPC.sendNotification h.serverHandle.rpcHandle SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure ()
    , requestHandler SMethod_TextDocumentHover $ mkRequestHandler $ \(HoverParams _doc pos _workDone) -> do
        let Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        pure $ Right $ InL rsp
    ]

main :: IO ()
main =
  runLspServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \h _req -> pure $ Right $ handlers h
      , options = defaultOptions
      }
