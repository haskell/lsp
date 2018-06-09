{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Data.Maybe
import           Data.Proxy
import           Control.Monad.IO.Class
import           Control.Lens hiding (List)
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Messages
import           ParsingTests

main = hspec $ do
  describe "manual session validation" $ do
    it "passes a test" $
      runSession "test/recordings/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"

        skipMany loggingNotification

        NotPublishDiagnostics diagsNot <- notification

        liftIO $ diagsNot ^. params . diagnostics `shouldBe` List []
        
        sendRequest (Proxy :: Proxy DocumentSymbolRequest)
                    TextDocumentDocumentSymbol
                    (DocumentSymbolParams doc)

        RspDocumentSymbols rspSymbols <- response
        
        liftIO $ do
          let (List symbols) = fromJust (rspSymbols ^. result)
              mainSymbol = head symbols
          mainSymbol ^. name `shouldBe` "main"
          mainSymbol ^. kind `shouldBe` SkFunction
          mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
          mainSymbol ^. containerName `shouldBe` Nothing
    
    it "fails a test" $
      -- TODO: Catch the exception in haskell-lsp-test and provide nicer output
      let session = runSession "test/recordings/renamePass" $ do
                    openDoc "Desktop/simple.hs" "haskell"
                    skipMany loggingNotification
                    request
        in session `shouldThrow` anyException
  
  describe "replay session" $ do
    it "passes a test" $
      replaySession "test/recordings/renamePass" `shouldReturn` True
    it "fails a test" $
      replaySession "test/recordings/renameFail" `shouldReturn` False
  
  parsingSpec