import Test.Hspec
import System.IO
import System.Directory
import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test.Recorded
-- import Language.Haskell.LSP.Test.Parsing
-- import Language.Haskell.LSP.Test.Files
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP

main = hspec $
  describe "replay" $
    it "passes a replay" $
      replaySession "test/recordings/renamePass" $ do
        x <- sendNextRequest
        liftIO $ print x
        y <- sendNextRequest
        liftIO $ print y