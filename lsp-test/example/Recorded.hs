import           Language.Haskell.LSP.Test.Recorded
import           System.Directory
import           System.Environment

main = do
  [session, dir] <- (take 2 <$> getArgs) >>= mapM canonicalizePath
  passed <- replay session dir
  putStrLn $ if passed then "Passed" else "Failed"
