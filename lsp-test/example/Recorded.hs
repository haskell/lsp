import           Language.Haskell.LSP.Test.Recorded
import           System.Directory
import           System.Environment
import           Control.Monad.IO.Class

main = do
  sessionFile <- (head <$> getArgs) >>= canonicalizePath
  replay sessionFile $ do
    x <- sendNextRequest
    liftIO $ print x
    y <- sendNextRequest
    liftIO $ print y