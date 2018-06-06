{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Data.Proxy
import           Control.Monad.IO.Class
import           Control.Lens hiding (List)
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.TH.DataTypesJSON

main = hspec $ do
  describe "manual session validation" $ 
    it "passes a test" $
      runSession "test/recordings/renamePass" $ do
        docItem <- getDocItem "Desktop/simple.hs" "haskell"
        docId   <- TextDocumentIdentifier <$> getDocUri "Desktop/simple.hs"

        sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams docItem)

        (NotificationMessage _ TextDocumentPublishDiagnostics (PublishDiagnosticsParams _ (List diags))) <-
          getMessage :: Session PublishDiagnosticsNotification

        liftIO $ diags `shouldBe` []
        
        sendRequest (Proxy :: Proxy DocumentSymbolRequest)
                    TextDocumentDocumentSymbol
                    (DocumentSymbolParams docId)

        (ResponseMessage _ _ (Just (List symbols)) Nothing) <- getMessage :: Session DocumentSymbolsResponse
        liftIO $ do
          let mainSymbol = head symbols
          mainSymbol ^. name `shouldBe` "main"
          mainSymbol ^. kind `shouldBe` SkFunction
          mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
          mainSymbol ^. containerName `shouldBe` Nothing
  
  describe "replay session" $
    it "passes a test" $
      replaySession "test/recordings/renamePass" `shouldReturn` True