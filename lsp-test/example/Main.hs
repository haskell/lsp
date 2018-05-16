import Language.Haskell.LSP.Test
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import qualified Data.Text.IO as T
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Environment

main = do
  files <- getArgs
  forM_ files $ \file -> session $ do
    file <- liftIO $ canonicalizePath file
    openDocument file
    symbols <- documentSymbols file
    liftIO $ mapM_ T.putStrLn (symbols ^.. traverse . LSP.name)
