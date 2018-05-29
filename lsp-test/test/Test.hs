import Test.Hspec
import System.IO
import System.Directory
import Control.Lens
import Language.Haskell.LSP.Test.Recorded
-- import Language.Haskell.LSP.Test.Parsing
-- import Language.Haskell.LSP.Test.Files
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP

main = hspec $ do
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

  -- describe "file swapping" $ do
  --   it "gets the base directory" $ do
  --     h <- openFile "test/recordings/renamePass/client.log" ReadMode
  --     msgs <- getAllMessages h
  --     rootDir msgs `shouldBe` "/Users/luke/Desktop"
  
    -- it "gets builds a mapping of files" $ do
    --   h <- openFile "test/recordings/renamePass/client.log" ReadMode
    --   msgs <- getAllMessages h
    --   let root = rootDir msgs
    --   swapped <- swapFiles root "test/recordings/renamePass/" msgs
    --   let (Just n) = decode (swapped !! 3) :: Maybe LSP.DidOpenNotification

    --   cd <- getCurrentDirectory

    --   n .^ params . uri `shouldBe` LSP.uriFromFilePath (cd </> "test/recordings/renamePass/")
