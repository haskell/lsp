{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Language.LSP.Server
import Language.LSP.Protocol.Types 
import Language.LSP.Protocol.Message 
import Control.Monad.IO.Class
import qualified Data.Text as T

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      let params = ShowMessageRequestParams MessageType_Info "Turn on code lenses?"
            (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)
              
            _ <- registerCapability SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ InL rsp
            pure ()
          Right _ ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
      pure ()
  , requestHandler SMethod_TextDocumentHover $ \req responder -> do
      let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover (InL ms) (Just range)
          ms = mkMarkdown "Hello world"
          range = Range pos pos
      responder (Right $ InL rsp)
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "demo"
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = \_caps -> handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }
