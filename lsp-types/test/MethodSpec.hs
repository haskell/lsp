{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MethodSpec where

import Control.Monad
import Data.Aeson qualified as J
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as J
import Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Method enum aeson instance consistency" diagnosticsSpec

-- ---------------------------------------------------------------------

clientMethods :: [T.Text]
clientMethods =
  [ -- General
    "initialize"
  , "initialized"
  , "shutdown"
  , "exit"
  , "$/cancelRequest"
  , -- Workspace
    "workspace/didChangeConfiguration"
  , "workspace/didChangeWatchedFiles"
  , "workspace/symbol"
  , "workspace/executeCommand"
  , -- Document
    "textDocument/didOpen"
  , "textDocument/didChange"
  , "textDocument/willSave"
  , "textDocument/willSaveWaitUntil"
  , "textDocument/didSave"
  , "textDocument/didClose"
  , "textDocument/completion"
  , "completionItem/resolve"
  , "textDocument/hover"
  , "textDocument/signatureHelp"
  , "textDocument/references"
  , "textDocument/documentHighlight"
  , "textDocument/documentSymbol"
  , "textDocument/formatting"
  , "textDocument/rangeFormatting"
  , "textDocument/onTypeFormatting"
  , "textDocument/definition"
  , "textDocument/codeAction"
  , "textDocument/codeLens"
  , "codeLens/resolve"
  , "textDocument/documentLink"
  , "documentLink/resolve"
  , "textDocument/rename"
  , "textDocument/prepareRename"
  , "textDocument/prepareCallHierarchy"
  , "callHierarchy/incomingCalls"
  , "callHierarchy/outgoingCalls"
  , -- FIXME: weird method
    -- ,"textDocument/semanticTokens"
    "textDocument/semanticTokens/full"
  , "textDocument/semanticTokens/full/delta"
  , "textDocument/semanticTokens/range"
  ]

serverMethods :: [T.Text]
serverMethods =
  [ -- Window
    "window/showMessage"
  , "window/showMessageRequest"
  , "window/logMessage"
  , "telemetry/event"
  , -- Client
    "client/registerCapability"
  , "client/unregisterCapability"
  , -- Workspace
    "workspace/applyEdit"
  , "workspace/semanticTokens/refresh"
  , -- Document
    "textDocument/publishDiagnostics"
  ]

diagnosticsSpec :: Spec
diagnosticsSpec = do
  describe "Client Methods" $ do
    it "maintains roundtrip consistency" $ do
      forM_ clientMethods $ \m -> do
        (J.toJSON <$> (J.fromJSON (J.String m) :: J.Result (J.SomeClientMethod)))
          `shouldBe` (J.Success $ J.String m)
  describe "Server Methods" $ do
    it "maintains roundtrip consistency" $ do
      forM_ serverMethods $ \m -> do
        (J.toJSON <$> (J.fromJSON (J.String m) :: J.Result (J.SomeServerMethod)))
          `shouldBe` (J.Success $ J.String m)

-- ---------------------------------
