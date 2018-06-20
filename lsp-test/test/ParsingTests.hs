{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module ParsingTests where

import Control.Lens hiding (List)
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Data.Conduit
import Data.Conduit.Parser
import Data.Default
import Test.Hspec

type TestSession = ConduitParser FromServerMessage IO

instance MonadSessionConfig IO where
  sessionConfig = return def

parsingSpec :: Spec
parsingSpec =
  describe "diagnostics" $ do
    let testDiag = NotPublishDiagnostics
                   (NotificationMessage "2.0"
                                       TextDocumentPublishDiagnostics
                                       (PublishDiagnosticsParams (Uri "foo")
                                                                 (List [])))
    it "get picked up" $ do
      let source = yield testDiag
          session = do
            diags <- publishDiagnosticsNotification :: TestSession PublishDiagnosticsNotification
            return $ diags ^. params . uri
      runConduit (source .| runConduitParser session) `shouldReturn` Uri "foo"
    it "get picked up after skipping others before" $ do
      let testDiag = NotPublishDiagnostics
                    (NotificationMessage "2.0"
                                          TextDocumentPublishDiagnostics
                                          (PublishDiagnosticsParams (Uri "foo")
                                                                    (List [])))
          notTestDiag = NotLogMessage (NotificationMessage "2.0" WindowLogMessage (LogMessageParams MtLog "foo"))
          source = yield notTestDiag >> yield testDiag
          session = do
            diags <- skipManyTill anyNotification notification :: TestSession PublishDiagnosticsNotification
            return $ diags ^. params . uri
      runConduit (source .| runConduitParser session) `shouldReturn` Uri "foo"
