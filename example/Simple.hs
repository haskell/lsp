{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Default
import Language.Haskell.LSP.Control
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types

handlers :: Handlers
handlers STextDocumentHover = Just $ \req responder -> do
  let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
      range = Range pos pos
  responder (Right rsp)
handlers _ = Nothing

initCallbacks = InitializeCallbacks
  { onInitialConfiguration = const $ Right ()
  , onConfigurationChange = const $ Right ()
  , onStartup = const $ pure Nothing
  }

main = run initCallbacks handlers def
