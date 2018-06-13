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
      runSession "hie --lsp" "test/data/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"

        skipMany loggingNotification

        checkNoDiagnostics
        
        rspSymbols <- documentSymbols doc
        
        liftIO $ do
          let (List symbols) = fromJust (rspSymbols ^. result)
              mainSymbol = head symbols
          mainSymbol ^. name `shouldBe` "main"
          mainSymbol ^. kind `shouldBe` SkFunction
          mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
          mainSymbol ^. containerName `shouldBe` Nothing
    
    it "fails a test" $
      -- TODO: Catch the exception in haskell-lsp-test and provide nicer output
      let session = runSession "hie --lsp" "test/data/renamePass" $ do
                      openDoc "Desktop/simple.hs" "haskell"
                      skipMany loggingNotification
                      anyRequest
        in session `shouldThrow` anyException
    it "can get initialize response" $ runSession "hie --lsp" "test/data/renamePass" $ do
      rsp <- getInitializeResponse
      liftIO $ rsp ^. result `shouldNotBe` Nothing
  
  describe "replay session" $ do
    it "passes a test" $
      replaySession "hie --lsp" "test/data/renamePass" `shouldReturn` True
    it "fails a test" $
      replaySession "hie --lsp" "test/data/renameFail" `shouldReturn` False

  describe "manual javascript session" $
    it "passes a test" $
      runSession "javascript-typescript-stdio" "test/data/javascriptPass" $ do
        doc <- openDoc "test.js" "javascript"
        
        checkNoDiagnostics

        rspSymbols <- documentSymbols doc

        let (List symbols) = fromJust (rspSymbols ^. result)
            fooSymbol = head symbols
        liftIO $ do
          fooSymbol ^. name `shouldBe` "foo"
          fooSymbol ^. kind `shouldBe` SkFunction
  
  parsingSpec

checkNoDiagnostics :: Session ()
checkNoDiagnostics = do
  diagsNot <- notification :: Session PublishDiagnosticsNotification
  liftIO $ diagsNot ^. params . diagnostics `shouldBe` List []

documentSymbols :: TextDocumentIdentifier -> Session DocumentSymbolsResponse
documentSymbols doc = do
  sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
  response