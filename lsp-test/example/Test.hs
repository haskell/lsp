{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Language.LSP.Test
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message

main = runSession "lsp-demo-reactor-server" fullCaps "test/data/" $ do
  doc <- openDoc "Rename.hs" "haskell"
  
  -- Use your favourite favourite combinators.
  skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)

  -- Send requests and notifications and receive responses
  rsp <- request SMethod_TextDocumentDocumentSymbol $
          DocumentSymbolParams Nothing Nothing doc
  liftIO $ print rsp

  -- Or use one of the helper functions
  getDocumentSymbols doc >>= liftIO . print

