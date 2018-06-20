{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import           Test.Hspec
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Lens hiding (List)
import           GHC.Generics
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.TH.ClientCapabilities
import           Language.Haskell.LSP.Types
import           ParsingTests

main = hspec $ do
  describe "manual session" $ do
    it "passes a test" $
      runSession "hie --lsp" "test/data/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"

        skipMany loggingNotification

        checkNoDiagnostics

        rspSymbols <- documentSymbols doc

        liftIO $ do
          let (List symbols) = fromJust (rspSymbols ^. result)
              mainSymbol = head symbols
          mainSymbol ^. name `shouldBe` "main"
          mainSymbol ^. kind `shouldBe` SkFunction
          mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
          mainSymbol ^. containerName `shouldBe` Nothing

    it "fails a test" $
      -- TODO: Catch the exception in haskell-lsp-test and provide nicer output
      let session = runSession "hie --lsp" "test/data/renamePass" $ do
                      openDoc "Desktop/simple.hs" "haskell"
                      skipMany loggingNotification
                      anyRequest
        in session `shouldThrow` anyException
    it "can get initialize response" $ runSession "hie --lsp" "test/data/renamePass" $ do
      rsp <- getInitializeResponse
      liftIO $ rsp ^. result `shouldNotBe` Nothing

    it "can register specific capabilities" $ do
      let caps = def { _workspace = Just workspaceCaps }
          workspaceCaps = def { _didChangeConfiguration = Just configCaps }
          configCaps = DidChangeConfigurationClientCapabilities (Just True)
      runSessionWithCapabilities caps "hie --lsp" "test/data/renamePass" $ return ()

  describe "replay session" $ do
    it "passes a test" $
      replaySession "hie --lsp" "test/data/renamePass" `shouldReturn` True
    it "fails a test" $
      replaySession "hie --lsp" "test/data/renameFail" `shouldReturn` False

  describe "manual javascript session" $
    it "passes a test" $
      runSession "javascript-typescript-stdio" "test/data/javascriptPass" $ do
        doc <- openDoc "test.js" "javascript"

        checkNoDiagnostics

        rspSymbols <- documentSymbols doc

        let (List symbols) = fromJust (rspSymbols ^. result)
            fooSymbol = head symbols
        liftIO $ do
          fooSymbol ^. name `shouldBe` "foo"
          fooSymbol ^. kind `shouldBe` SkFunction

  describe "text document state" $
    it "sends back didChange notifications" $
      runSession "hie --lsp" "test/data/refactor" $ do
        doc <- openDoc "Main.hs" "haskell"

        let args = toJSON $ AOP (doc ^. uri)
                                (Position 1 14)
                                "Redundant bracket"
            reqParams = ExecuteCommandParams "applyrefact:applyOne" (Just (List [args]))
        sendRequest WorkspaceExecuteCommand reqParams
        skipMany anyNotification
        _ <- response :: Session ExecuteCommandResponse

        editReq <- request :: Session ApplyWorkspaceEditRequest
        liftIO $ do
          let (Just cs) = editReq ^. params . edit . changes
              [(u, List es)] = HM.toList cs
          u `shouldBe` doc ^. uri
          es `shouldBe` [TextEdit (Range (Position 1 0) (Position 1 18)) "main = return 42"]

        checkNoDiagnostics

        contents <- documentContents doc
        liftIO $ contents `shouldBe` "main :: IO Int\nmain = return 42"

  parsingSpec

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  , hintTitle :: String
  } deriving (Generic, ToJSON)

checkNoDiagnostics :: Session ()
checkNoDiagnostics = do
  diagsNot <- notification :: Session PublishDiagnosticsNotification
  liftIO $ diagsNot ^. params . diagnostics `shouldBe` List []

documentSymbols :: TextDocumentIdentifier -> Session DocumentSymbolsResponse
documentSymbols doc = do
  sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
  response
