import Language.Haskell.LSP.Test
import Language.Haskell.LSP.TH.DataTypesJSON
import Data.Proxy

import Control.Monad.IO.Class

main = runSession "test/recordings/renamePass" $ do

  docItem <- getDocItem "Desktop/simple.hs" "haskell"
  docId <- TextDocumentIdentifier <$> getDocUri "Desktop/simple.hs"

  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams docItem)
  
  sendRequest (Proxy :: Proxy DocumentSymbolRequest) TextDocumentDocumentSymbol (DocumentSymbolParams docId)

  syms <- getMessage :: Session DocumentSymbolsResponse
  liftIO $ print syms