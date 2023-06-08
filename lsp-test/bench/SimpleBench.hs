{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Language.LSP.Server
import qualified Language.LSP.Test as Test
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Control.Monad.IO.Class
import Control.Monad
import System.Process hiding (env)
import System.Environment
import System.Time.Extra
import Control.Concurrent
import Data.IORef

handlers :: Handlers (LspM ())
handlers = mconcat
  [ requestHandler SMethod_TextDocumentHover $ \req responder -> do
      let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = InL $ mkMarkdown "Hello world"
          range = Range pos pos
      responder (Right $ InL rsp)
  , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
      let TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier doc) pos _ _) = req
      responder (Right $ InL $ Definition $ InL $ Location doc $ Range pos pos)
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

  _ <- forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite server
  liftIO $ putStrLn $ "Starting " <> show n <> " rounds"

  i <- newIORef (0 :: Integer)

  Test.runSessionWithHandles hinWrite houtRead Test.defaultConfig Test.fullCaps "." $ do
    start <- liftIO offsetTime
    replicateM_ n $ do
      v <- liftIO $ readIORef i
      liftIO $ when (v `mod` 1000 == 0) $ putStrLn $ show v
      TResponseMessage{_result=Right (InL _)} <- Test.request SMethod_TextDocumentHover $
                                                              HoverParams (TextDocumentIdentifier $ Uri "test") (Position 1 100) Nothing
      TResponseMessage{_result=Right (InL _)} <- Test.request SMethod_TextDocumentDefinition $
                                                              DefinitionParams (TextDocumentIdentifier $ Uri "test") (Position 1000 100) Nothing Nothing

      liftIO $ modifyIORef' i (+1)
      pure ()
    end <- liftIO start
    liftIO $ putStrLn $ "Completed " <> show n <> " rounds in " <> showDuration end

