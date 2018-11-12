import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types

main = runSession "hie" fullCaps "../test/data/" $ do
  docItem <- openDoc "Rename.hs" "haskell"
  
  -- Use your favourite favourite combinators.
  skipManyTill loggingNotification (count 2 publishDiagnosticsNotification)

  -- Send requests and notifications and receive responses
  let params = DocumentSymbolParams docItem
  rsp <- request TextDocumentDocumentSymbol params :: Session DocumentSymbolsResponse
  liftIO $ print rsp

  -- Or use one of the helper functions
  getDocumentSymbols docItem >>= liftIO . print

