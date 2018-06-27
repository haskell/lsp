import Language.Haskell.LSP.Test
import Language.Haskell.LSP.TH.DataTypesJSON

import Control.Monad.IO.Class

main = runSession "hie --lsp" "test/recordings/renamePass" $ do
  docItem <- openDoc "Desktop/simple.hs" "haskell"
  
  sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams docItem)

  skipMany loggingNotification

  anyResponse >>= liftIO . print
