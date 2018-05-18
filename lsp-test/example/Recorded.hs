import           Language.Haskell.LSP.Test.Recorded
import           System.Directory
import           System.Environment

main = do
  file <- (head <$> getArgs) >>= canonicalizePath
  replay file
