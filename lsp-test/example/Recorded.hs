import           Language.Haskell.LSP.Test.Recorded
import           System.Directory
import           System.Environment

main = do
  [client, server, dir] <- (take 3 <$> getArgs) >>= mapM canonicalizePath
  passed <- replay client server dir
  putStrLn $ if passed then "Passed" else "Failed"
