import           Language.Haskell.LSP.Test.Recorded
import           System.Directory
import           System.Environment

main = do
  [client, server] <- (take 2 <$> getArgs) >>= mapM canonicalizePath
  passed <- replay client server
  putStrLn $ if passed then "Passed" else "Failed"
