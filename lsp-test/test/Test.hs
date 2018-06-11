{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Lens hiding (List)
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.Types
import           ParsingTests

main = hspec $ do
  describe "manual session" $ do
    it "passes a test" $
      runSession "hie --lsp" "test/recordings/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"

        skipMany loggingNotification

        diagsNot <- notification :: Session PublishDiagnosticsNotification

        liftIO $ diagsNot ^. params . diagnostics `shouldBe` List []
        
        sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)

        rspSymbols <- response :: Session DocumentSymbolsResponse
        
        liftIO $ do
          let (List symbols) = fromJust (rspSymbols ^. result)
              mainSymbol = head symbols
          mainSymbol ^. name `shouldBe` "main"
          mainSymbol ^. kind `shouldBe` SkFunction
          mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
          mainSymbol ^. containerName `shouldBe` Nothing
    
    it "fails a test" $
      -- TODO: Catch the exception in haskell-lsp-test and provide nicer output
      let session = runSession "hie --lsp" "test/recordings/renamePass" $ do
                      openDoc "Desktop/simple.hs" "haskell"
                      skipMany loggingNotification
                      anyRequest
        in session `shouldThrow` anyException
  
  describe "replay session" $ do
    it "passes a test" $
      replaySession "hie --lsp" "test/recordings/renamePass" `shouldReturn` True
    it "fails a test" $
      replaySession "hie --lsp" "test/recordings/renameFail" `shouldReturn` False
  
  parsingSpec
