import Language.Haskell.LSP.Test
import Language.Haskell.LSP.TH.DataTypesJSON
import Data.Proxy

import Control.Monad.IO.Class

main = runSession "hie --lsp" "test/recordings/renamePass" $ do

  docItem <- getDocItem "Desktop/simple.hs" "haskell"
  docId <- TextDocumentIdentifier <$> getDocUri "Desktop/simple.hs"

  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams docItem)
  
  sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams docId)

  skipMany loggingNotification

  anyResponse >>= liftIO . print
