{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Default
import Language.Haskell.LSP.Control
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types

handlers :: Handlers ()
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageRequestParams MtInfo "Turn on code lenses?"
            (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      sendRequest SWindowShowMessageRequest params $ \res ->
        case res of
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)
              
            registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err -> 
            sendNotification SWindowShowMessage (ShowMessageParams MtError "Something went wrong!")
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]

initCallbacks = InitializeCallbacks
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = const $ pure Nothing
  }

main = run initCallbacks handlers def
