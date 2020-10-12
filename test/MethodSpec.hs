{-# LANGUAGE OverloadedStrings, DataKinds #-}
module MethodSpec where


import           Control.Monad
import qualified Data.Aeson as J
import qualified Language.LSP.Types            as J
import           Test.Hspec
import qualified Data.Text as T

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Method enum aeson instance consistency" diagnosticsSpec

-- ---------------------------------------------------------------------

clientMethods :: [T.Text]
clientMethods = [
  -- General
   "initialize"
  ,"initialized"
  ,"shutdown"
  ,"exit"
  ,"$/cancelRequest"
 -- Workspace
  ,"workspace/didChangeConfiguration"
  ,"workspace/didChangeWatchedFiles"
  ,"workspace/symbol"
  ,"workspace/executeCommand"
 -- Document
  ,"textDocument/didOpen"
  ,"textDocument/didChange"
  ,"textDocument/willSave"
  ,"textDocument/willSaveWaitUntil"
  ,"textDocument/didSave"
  ,"textDocument/didClose"
  ,"textDocument/completion"
  ,"completionItem/resolve"
  ,"textDocument/hover"
  ,"textDocument/signatureHelp"
  ,"textDocument/references"
  ,"textDocument/documentHighlight"
  ,"textDocument/documentSymbol"
  ,"textDocument/formatting"
  ,"textDocument/rangeFormatting"
  ,"textDocument/onTypeFormatting"
  ,"textDocument/definition"
  ,"textDocument/codeAction"
  ,"textDocument/codeLens"
  ,"codeLens/resolve"
  ,"textDocument/documentLink"
  ,"documentLink/resolve"
  ,"textDocument/rename"
  ,"textDocument/prepareRename"
  ]

serverMethods :: [T.Text]
serverMethods = [
  -- Window
   "window/showMessage"
  ,"window/showMessageRequest"
  ,"window/logMessage"
  ,"telemetry/event"
  -- Client
  ,"client/registerCapability"
  ,"client/unregisterCapability"
  -- Workspace
  ,"workspace/applyEdit"
  -- Document
  ,"textDocument/publishDiagnostics"
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
