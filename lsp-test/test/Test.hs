import Test.Hspec
import Language.Haskell.LSP.Test.Recorded

main = hspec $
  describe "Replay" $ do
    it "passes a test" $
      replay "test/recordings/renamePass/client.log"
             "test/recordings/renamePass/server.log"
             "test/recordings/renamePass"
        `shouldReturn` True
    it "fails a test" $
      replay "test/recordings/documentSymbolFail/client.log"
             "test/recordings/documentSymbolFail/server.log" 
             "test/recordings/documentSymbolFail"
        `shouldReturn` False
