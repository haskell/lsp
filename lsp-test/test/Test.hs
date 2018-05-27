import Test.Hspec
import Language.Haskell.LSP.Test.Recorded

main = hspec $ do
  describe "Replay" $ do
    it "passes a test" $ do
      replay "test/recordings/renamePass/client.log"
             "test/recordings/renamePass/server.log"
             "test/recordings/renamePass"
        `shouldReturn` True
    -- it "fails a test" $
    --   replay "test/recordings/documentSymbolFail/client.log"
    --          "test/recordings/documentSymbolFail/server.log" 
    --     `shouldReturn` False
