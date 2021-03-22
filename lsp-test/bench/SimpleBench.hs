{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}
module Main where

import Language.LSP.Server
import qualified Language.LSP.Test as Test
import Language.LSP.Types
import Control.Monad.IO.Class
import Control.Monad
import System.Process
import System.Environment
import System.Time.Extra
import Control.Concurrent
import Data.IORef

handlers :: Handlers (LspM ())
handlers = mconcat
  [ requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  , requestHandler STextDocumentDefinition $ \req responder -> do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier doc) pos _ _) = req
      responder (Right $ InL $ Location doc $ Range pos pos)
  ]

server :: ServerDefinition ()
server = ServerDefinition
  { onConfigurationChange = const $ const $ Right ()
  , defaultConfig = ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }

main :: IO ()
main = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  n <- read . head <$> getArgs

  forkIO $ void $ runServerWithHandles hinRead houtWrite server
  liftIO $ putStrLn $ "Starting " <> show n <> " rounds"

  i <- newIORef 0

  Test.runSessionWithHandles hinWrite houtRead Test.defaultConfig Test.fullCaps "." $ do
    start <- liftIO offsetTime
    replicateM_ n $ do
      n <- liftIO $ readIORef i
      liftIO $ when (n `mod` 1000 == 0) $ putStrLn $ show n
      ResponseMessage{_result=Right (Just _)} <- Test.request STextDocumentHover $
                                                              HoverParams (TextDocumentIdentifier $ Uri "test") (Position 1 100) Nothing
      ResponseMessage{_result=Right (InL _)} <- Test.request STextDocumentDefinition $
                                                              DefinitionParams (TextDocumentIdentifier $ Uri "test") (Position 1000 100) Nothing Nothing

      liftIO $ modifyIORef' i (+1)
      pure ()
    end <- liftIO start
    liftIO $ putStrLn $ "Completed " <> show n <> " rounds in " <> showDuration end

