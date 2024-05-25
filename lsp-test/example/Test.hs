{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test

main :: IO ()
main = runSession "lsp-demo-reactor-server" fullLatestClientCaps "test/data/" $ do
  doc <- openDoc "Rename.hs" "haskell"

  -- Use your favourite favourite combinators.
  skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)

  -- Send requests and notifications and receive responses
  rsp <-
    request SMethod_TextDocumentDocumentSymbol $
      DocumentSymbolParams Nothing Nothing doc
  liftIO $ print rsp

  -- Or use one of the helper functions
  getDocumentSymbols doc >>= liftIO . print
