{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import           Test.Hspec
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens hiding (List)
import           GHC.Generics
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.TH.ClientCapabilities
import           Language.Haskell.LSP.Types hiding (message, capabilities)
import           System.Timeout

main = hspec $ do
  describe "manual session" $ do
    it "fails a test" $
      -- TODO: Catch the exception in haskell-lsp-test and provide nicer output
      let session = runSession "hie --lsp" "test/data/renamePass" $ do
                      openDoc "Desktop/simple.hs" "haskell"
                      skipMany loggingNotification
                      anyRequest
        in session `shouldThrow` anyException
    it "can get initialize response" $ runSession "hie --lsp" "test/data/renamePass" $ do
      rsp <- initializeResponse
      liftIO $ rsp ^. result `shouldNotBe` Nothing

    it "can register specific capabilities" $
      runSessionWithConfig (def { capabilities = didChangeCaps })
        "hie --lsp" "test/data/renamePass" $ return ()

    describe "withTimeout" $ do
      it "times out" $
        let sesh = runSession "hie --lsp" "test/data/renamePass" $ do
                    openDoc "Desktop/simple.hs" "haskell"
                    -- won't receive a request - will timeout
                    -- incoming logging requests shouldn't increase the
                    -- timeout
                    withTimeout 5 $ skipManyTill anyMessage message :: Session ApplyWorkspaceEditRequest
          -- wait just a bit longer than 5 seconds so we have time
          -- to open the document
          in timeout 6000000 sesh `shouldThrow` anySessionException
          
      it "doesn't time out" $
        let sesh = runSession "hie --lsp" "test/data/renamePass" $ do
                    openDoc "Desktop/simple.hs" "haskell"
                    withTimeout 5 $ skipManyTill anyMessage publishDiagnosticsNotification
          in void $ timeout 6000000 sesh

      it "further timeout messages are ignored" $ runSession "hie --lsp" "test/data/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"
        withTimeout 3 $ getDocumentSymbols doc
        liftIO $ threadDelay 5000000
        -- shouldn't throw an exception
        getDocumentSymbols doc
        return ()

      it "overrides global message timeout" $
        let sesh =
              runSessionWithConfig (def { messageTimeout = 5 }) "hie --lsp" "test/data/renamePass" $ do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                return True
        in sesh `shouldReturn` True

      it "unoverrides global message timeout" $
        let sesh =
              runSessionWithConfig (def { messageTimeout = 5 }) "hie --lsp" "test/data/renamePass" $ do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                -- should now timeout
                skipManyTill anyMessage message :: Session ApplyWorkspaceEditRequest
        in sesh `shouldThrow` (== TimeoutException)


    describe "exceptions" $ do
      it "throw on time out" $
        let sesh = runSessionWithConfig (def {messageTimeout = 10}) "hie --lsp" "test/data/renamePass" $ do
                skipMany loggingNotification
                _ <- message :: Session ApplyWorkspaceEditRequest
                return ()
        in sesh `shouldThrow` anySessionException

      it "don't throw when no time out" $ runSessionWithConfig (def {messageTimeout = 5}) "hie --lsp" "test/data/renamePass" $ do
        loggingNotification
        liftIO $ threadDelay 10
        _ <- openDoc "Desktop/simple.hs" "haskell"
        return ()

      describe "UnexpectedMessageException" $ do
        it "throws when there's an unexpected message" $
          let selector (UnexpectedMessageException "Publish diagnostics notification" (NotLogMessage _)) = True
              selector _ = False
            in runSession "hie --lsp" "test/data/renamePass" publishDiagnosticsNotification `shouldThrow` selector
        it "provides the correct types that were expected and received" $
          let selector (UnexpectedMessageException "ResponseMessage WorkspaceEdit" (RspDocumentSymbols _)) = True
              selector _ = False
              sesh = do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                sendRequest' TextDocumentDocumentSymbol (DocumentSymbolParams doc)
                skipMany anyNotification
                message :: Session RenameResponse -- the wrong type
            in runSession "hie --lsp" "test/data/renamePass" sesh
              `shouldThrow` selector

  describe "replay session" $ do
    it "passes a test" $
      replaySession "hie --lsp" "test/data/renamePass"
    it "fails a test" $
      let selector (ReplayOutOfOrderException _ _) = True
          selector _ = False
        in replaySession "hie --lsp" "test/data/renameFail" `shouldThrow` selector

  describe "manual javascript session" $
    it "passes a test" $
      runSession "javascript-typescript-stdio" "test/data/javascriptPass" $ do
        doc <- openDoc "test.js" "javascript"

        noDiagnostics

        (fooSymbol:_) <- getDocumentSymbols doc

        liftIO $ do
          fooSymbol ^. name `shouldBe` "foo"
          fooSymbol ^. kind `shouldBe` SkFunction

  describe "text document VFS" $
    it "sends back didChange notifications" $
      runSession "hie --lsp" "test/data/refactor" $ do
        doc <- openDoc "Main.hs" "haskell"

        let args = toJSON $ AOP (doc ^. uri)
                                (Position 1 14)
                                "Redundant bracket"
            reqParams = ExecuteCommandParams "applyrefact:applyOne" (Just (List [args]))
        sendRequest_ WorkspaceExecuteCommand reqParams

        editReq <- message :: Session ApplyWorkspaceEditRequest
        liftIO $ do
          let (Just cs) = editReq ^. params . edit . changes
              [(u, List es)] = HM.toList cs
          u `shouldBe` doc ^. uri
          es `shouldBe` [TextEdit (Range (Position 1 0) (Position 1 18)) "main = return 42"]

        noDiagnostics

        contents <- documentContents doc
        liftIO $ contents `shouldBe` "main :: IO Int\nmain = return 42"

  describe "documentEdit" $
    it "automatically consumes applyedit requests" $
      runSession "hie --lsp" "test/data/refactor" $ do
        doc <- openDoc "Main.hs" "haskell"

        let args = toJSON $ AOP (doc ^. uri)
                                (Position 1 14)
                                "Redundant bracket"
            reqParams = ExecuteCommandParams "applyrefact:applyOne" (Just (List [args]))
        sendRequest_ WorkspaceExecuteCommand reqParams
        contents <- getDocumentEdit doc
        liftIO $ contents `shouldBe` "main :: IO Int\nmain = return 42"
        noDiagnostics

  describe "getAllCodeActions" $
    it "works" $ runSession "hie --lsp" "test/data/refactor" $ do
      doc <- openDoc "Main.hs" "haskell"
      _ <- waitForDiagnostics
      actions <- getAllCodeActions doc
      liftIO $ do
        let [CommandOrCodeActionCommand action] = actions
        action ^. title `shouldBe` "Apply hint:Redundant bracket"
        action ^. command `shouldSatisfy` T.isSuffixOf ":applyrefact:applyOne"

  describe "getDocumentSymbols" $
    it "works" $ runSession "hie --lsp" "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"

      skipMany loggingNotification

      noDiagnostics

      (mainSymbol:_) <- getDocumentSymbols doc

      liftIO $ do
        mainSymbol ^. name `shouldBe` "main"
        mainSymbol ^. kind `shouldBe` SkFunction
        mainSymbol ^. location . range `shouldBe` Range (Position 3 0) (Position 3 4)
        mainSymbol ^. containerName `shouldBe` Nothing

  describe "applyEdit" $ do
    it "increments the version" $ runSessionWithConfig (def { capabilities = docChangesCaps }) "hie --lsp" "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"
      VersionedTextDocumentIdentifier _ (Just oldVersion) <- getVersionedDoc doc
      let edit = TextEdit (Range (Position 1 1) (Position 1 3)) "foo" 
      VersionedTextDocumentIdentifier _ (Just newVersion) <- applyEdit edit doc
      liftIO $ newVersion `shouldBe` oldVersion + 1
    it "changes the document contents" $ runSession "hie --lsp" "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"
      let edit = TextEdit (Range (Position 0 0) (Position 0 2)) "foo" 
      applyEdit edit doc
      contents <- documentContents doc
      liftIO $ contents `shouldSatisfy` T.isPrefixOf "foodule"


didChangeCaps :: ClientCapabilities
didChangeCaps = def { _workspace = Just workspaceCaps }
  where
    workspaceCaps = def { _didChangeConfiguration = Just configCaps }
    configCaps = DidChangeConfigurationClientCapabilities (Just True)

docChangesCaps :: ClientCapabilities
docChangesCaps = def { _workspace = Just workspaceCaps }
  where
    workspaceCaps = def { _workspaceEdit = Just editCaps }
    editCaps = WorkspaceEditClientCapabilities (Just True)

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  , hintTitle :: String
  } deriving (Generic, ToJSON)
