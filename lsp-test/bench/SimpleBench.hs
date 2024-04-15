{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Test qualified as Test
import System.Environment
import System.Process hiding (env)
import System.Time.Extra

handlers :: Handlers (LspM ())
handlers =
  mconcat
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
server =
  ServerDefinition
    { parseConfig = const $ const $ Right ()
    , onConfigChange = const $ pure ()
    , defaultConfig = ()
    , configSection = "demo"
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = \_caps -> handlers
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
      TResponseMessage{_result = Right (InL _)} <-
        Test.request SMethod_TextDocumentHover $
          HoverParams (TextDocumentIdentifier $ Uri "test") (Position 1 100) Nothing
      TResponseMessage{_result = Right (InL _)} <-
        Test.request SMethod_TextDocumentDefinition $
          DefinitionParams (TextDocumentIdentifier $ Uri "test") (Position 1000 100) Nothing Nothing

      liftIO $ modifyIORef' i (+ 1)
      pure ()
    end <- liftIO start
    liftIO $ putStrLn $ "Completed " <> show n <> " rounds in " <> showDuration end
