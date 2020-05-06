{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import           Test.Hspec
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Either
import           Data.Maybe
import qualified Data.Text as T
import           Control.Applicative.Combinators
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens hiding (List)
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens as LSP hiding
  (capabilities, message, rename, applyEdit)
import           Language.Haskell.LSP.Types.Capabilities as LSP
import           System.Directory
import           System.FilePath
import           System.Timeout

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}


main = findServer >>= \serverExe -> hspec $ do
  describe "Session" $ do
    it "fails a test" $ do
      let session = runSession serverExe fullCaps "test/data/renamePass" $ do
                      openDoc "Desktop/simple.hs" "haskell"
                      anyRequest
        in session `shouldThrow` anySessionException
    it "initializeResponse" $ runSession serverExe fullCaps "test/data/renamePass" $ do
      rsp <- initializeResponse
      liftIO $ rsp ^. result `shouldSatisfy` isRight

    it "runSessionWithConfig" $
      runSession serverExe didChangeCaps "test/data/renamePass" $ return ()

    describe "withTimeout" $ do
      it "times out" $
        let sesh = runSession serverExe fullCaps "test/data/renamePass" $ do
                    openDoc "Desktop/simple.hs" "haskell"
                    -- won't receive a request - will timeout
                    -- incoming logging requests shouldn't increase the
                    -- timeout
                    withTimeout 5 $ skipManyTill anyMessage message :: Session ApplyWorkspaceEditRequest
          -- wait just a bit longer than 5 seconds so we have time
          -- to open the document
          in timeout 6000000 sesh `shouldThrow` anySessionException

      it "doesn't time out" $
        let sesh = runSession serverExe fullCaps "test/data/renamePass" $ do
                    openDoc "Desktop/simple.hs" "haskell"
                    withTimeout 5 $ skipManyTill anyMessage publishDiagnosticsNotification
          in void $ timeout 6000000 sesh

      it "further timeout messages are ignored" $ runSession serverExe fullCaps "test/data/renamePass" $ do
        doc <- openDoc "Desktop/simple.hs" "haskell"
        -- shouldn't timeout
        withTimeout 3 $ getDocumentSymbols doc
        -- longer than the original timeout
        liftIO $ threadDelay (5 * 10^6)
        -- shouldn't throw an exception
        getDocumentSymbols doc
        return ()

      it "overrides global message timeout" $
        let sesh =
              runSessionWithConfig (def { messageTimeout = 5 }) serverExe fullCaps "test/data/renamePass" $ do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                return True
        in sesh `shouldReturn` True

      it "unoverrides global message timeout" $
        let sesh =
              runSessionWithConfig (def { messageTimeout = 5 }) serverExe fullCaps "test/data/renamePass" $ do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                -- should now timeout
                skipManyTill anyMessage message :: Session ApplyWorkspaceEditRequest
            isTimeout (Timeout _) = True
            isTimeout _ = False
        in sesh `shouldThrow` isTimeout


    describe "SessionException" $ do
      it "throw on time out" $
        let sesh = runSessionWithConfig (def {messageTimeout = 10}) serverExe fullCaps "test/data/renamePass" $ do
                skipMany loggingNotification
                _ <- message :: Session ApplyWorkspaceEditRequest
                return ()
        in sesh `shouldThrow` anySessionException

      it "don't throw when no time out" $ runSessionWithConfig (def {messageTimeout = 5}) serverExe fullCaps "test/data/renamePass" $ do
        loggingNotification
        liftIO $ threadDelay $ 6 * 1000000
        _ <- openDoc "Desktop/simple.hs" "haskell"
        return ()

      describe "UnexpectedMessageException" $ do
        it "throws when there's an unexpected message" $
          let selector (UnexpectedMessage "Publish diagnostics notification" (NotLogMessage _)) = True
              selector _ = False
            in runSession serverExe fullCaps "test/data/renamePass" publishDiagnosticsNotification `shouldThrow` selector
        it "provides the correct types that were expected and received" $
          let selector (UnexpectedMessage "ResponseMessage WorkspaceEdit" (RspDocumentSymbols _)) = True
              selector _ = False
              sesh = do
                doc <- openDoc "Desktop/simple.hs" "haskell"
                sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc Nothing)
                skipMany anyNotification
                message :: Session RenameResponse -- the wrong type
            in runSession serverExe fullCaps "test/data/renamePass" sesh
              `shouldThrow` selector

  -- This is too fickle at the moment
  -- describe "replaySession" $
  --   it "passes a test" $
  --     replaySession serverExe "test/data/renamePass"
  --   it "fails a test" $
  --     let selector (ReplayOutOfOrder _ _) = True
  --         selector _ = False
  --       in replaySession serverExe "test/data/renameFail" `shouldThrow` selector

  -- describe "manual javascript session" $
  --   it "passes a test" $
  --     runSession "javascript-typescript-stdio" fullCaps "test/data/javascriptPass" $ do
  --       doc <- openDoc "test.js" "javascript"

  --       noDiagnostics

  --       Right (fooSymbol:_) <- getDocumentSymbols doc

  --       liftIO $ do
  --         fooSymbol ^. name `shouldBe` "foo"
  --         fooSymbol ^. kind `shouldBe` SkFunction

  describe "text document VFS" $
    it "sends back didChange notifications" $
      runSession serverExe def "test/data/refactor" $ do
        doc <- openDoc "Main.hs" "haskell"

        let args = toJSON (doc ^. uri)
            reqParams = ExecuteCommandParams "doAnEdit" (Just (List [args])) Nothing
        request_ WorkspaceExecuteCommand reqParams

        editReq <- message :: Session ApplyWorkspaceEditRequest
        liftIO $ do
          let (Just cs) = editReq ^. params . edit . changes
              [(u, List es)] = HM.toList cs
          u `shouldBe` doc ^. uri
          es `shouldBe` [TextEdit (Range (Position 0 0) (Position 0 5)) "howdy"]
        contents <- documentContents doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getDocumentEdit" $
    it "automatically consumes applyedit requests" $
      runSession serverExe fullCaps "test/data/refactor" $ do
        doc <- openDoc "Main.hs" "haskell"

        let args = toJSON (doc ^. uri)
            reqParams = ExecuteCommandParams "doAnEdit" (Just (List [args])) Nothing
        request_ WorkspaceExecuteCommand reqParams
        contents <- getDocumentEdit doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getCodeActions" $
    it "works" $ runSession serverExe fullCaps "test/data/refactor" $ do
      doc <- openDoc "Main.hs" "haskell"
      waitForDiagnostics
      [CACodeAction action] <- getCodeActions doc (Range (Position 1 14) (Position 1 18))
      liftIO $ action ^. title `shouldBe` "Delete this"

  describe "getAllCodeActions" $
    it "works" $ runSession serverExe fullCaps "test/data/refactor" $ do
      doc <- openDoc "Main.hs" "haskell"
      _ <- waitForDiagnostics
      actions <- getAllCodeActions doc
      liftIO $ do
        let [CACodeAction action] = actions
        action ^. title `shouldBe` "Delete this"
        action ^. command . _Just . command `shouldBe` "deleteThis"

  describe "getDocumentSymbols" $
    it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"

      skipMany loggingNotification

      Left (mainSymbol:_) <- getDocumentSymbols doc

      liftIO $ do
        mainSymbol ^. name `shouldBe` "foo"
        mainSymbol ^. kind `shouldBe` SkObject
        mainSymbol ^. range `shouldBe` mkRange 0 0 3 6

  describe "applyEdit" $ do
    it "increments the version" $ runSession serverExe docChangesCaps "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"
      VersionedTextDocumentIdentifier _ (Just oldVersion) <- getVersionedDoc doc
      let edit = TextEdit (Range (Position 1 1) (Position 1 3)) "foo"
      VersionedTextDocumentIdentifier _ (Just newVersion) <- applyEdit doc edit
      liftIO $ newVersion `shouldBe` oldVersion + 1
    it "changes the document contents" $ runSession serverExe fullCaps "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"
      let edit = TextEdit (Range (Position 0 0) (Position 0 2)) "foo"
      applyEdit doc edit
      contents <- documentContents doc
      liftIO $ contents `shouldSatisfy` T.isPrefixOf "foodule"

  -- describe "getCompletions" $
  --   it "works" $ runSession serverExe def "test/data/renamePass" $ do
  --     doc <- openDoc "Desktop/simple.hs" "haskell"

  --     -- wait for module to be loaded
  --     skipMany loggingNotification
  --     noDiagnostics
  --     noDiagnostics

  --     comps <- getCompletions doc (Position 5 5)
  --     let item = head (filter (\x -> x ^. label == "interactWithUser") comps)
  --     liftIO $ do
  --       item ^. label `shouldBe` "interactWithUser"
  --       item ^. kind `shouldBe` Just CiFunction
  --       item ^. detail `shouldBe` Just "Items -> IO ()\nMain"

  -- describe "getReferences" $
  --   it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
  --     doc <- openDoc "Desktop/simple.hs" "haskell"
  --     let pos = Position 40 3 -- interactWithUser
  --         uri = doc ^. LSP.uri
  --     refs <- getReferences doc pos True
  --     liftIO $ refs `shouldContain` map (Location uri) [
  --         mkRange 41 0 41 16
  --       , mkRange 75 6 75 22
  --       , mkRange 71 6 71 22
  --       ]

  -- describe "getDefinitions" $
  --   it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
  --     doc <- openDoc "Desktop/simple.hs" "haskell"
  --     let pos = Position 49 25 -- addItem
  --     defs <- getDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 28 0 28 7)]

  -- describe "getTypeDefinitions" $
  --   it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
  --     doc <- openDoc "Desktop/simple.hs" "haskell"
  --     let pos = Position 20 23  -- Quit value
  --     defs <- getTypeDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 10 0 14 19)]  -- Type definition

  describe "waitForDiagnosticsSource" $
    it "works" $ runSession serverExe fullCaps "test/data" $ do
      openDoc "Error.hs" "haskell"
      [diag] <- waitForDiagnosticsSource "dummy-server"
      liftIO $ do
        diag ^. severity `shouldBe` Just DsWarning
        diag ^. source `shouldBe` Just "dummy-server"

  -- describe "rename" $ do
  --   it "works" $ pendingWith "HaRe not in hie-bios yet"
  --   it "works on javascript" $
  --     runSession "javascript-typescript-stdio" fullCaps "test/data/javascriptPass" $ do
  --       doc <- openDoc "test.js" "javascript"
  --       rename doc (Position 2 11) "bar"
  --       documentContents doc >>= liftIO . (`shouldContain` "function bar()") . T.unpack

  describe "getHover" $
    it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
      doc <- openDoc "Desktop/simple.hs" "haskell"
      hover <- getHover doc (Position 45 9)
      liftIO $ hover `shouldSatisfy` isJust

  -- describe "getHighlights" $
  --   it "works" $ runSession serverExe fullCaps "test/data/renamePass" $ do
  --     doc <- openDoc "Desktop/simple.hs" "haskell"
  --     skipManyTill loggingNotification $ count 2 noDiagnostics
  --     highlights <- getHighlights doc (Position 27 4) -- addItem
  --     liftIO $ length highlights `shouldBe` 4

  -- describe "formatDoc" $
  --   it "works" $ runSession serverExe fullCaps "test/data" $ do
  --     doc <- openDoc "Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatDoc doc (FormattingOptions 4 True)
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  -- describe "formatRange" $
  --   it "works" $ runSession serverExe fullCaps "test/data" $ do
  --     doc <- openDoc "Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatRange doc (FormattingOptions 4 True) (Range (Position 1 10) (Position 2 10))
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  describe "closeDoc" $
    it "works" $
      let sesh =
            runSession serverExe fullCaps "test/data" $ do
              doc <- openDoc "Format.hs" "haskell"
              closeDoc doc
              -- need to evaluate to throw
              documentContents doc >>= liftIO . print
      in sesh `shouldThrow` anyException

  describe "satisfy" $
    it "works" $ runSession serverExe fullCaps "test/data" $ do
      openDoc "Format.hs" "haskell"
      let pred (NotLogMessage _) = True
          pred _ = False
      void $ satisfy pred

  describe "ignoreLogNotifications" $
    it "works" $
      runSessionWithConfig (defaultConfig { ignoreLogNotifications = True }) serverExe  fullCaps "test/data" $ do
        openDoc "Format.hs" "haskell"
        void publishDiagnosticsNotification

mkRange sl sc el ec = Range (Position sl sc) (Position el ec)

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


findExeRecursive :: FilePath -> FilePath -> IO (Maybe FilePath)
findExeRecursive exe dir = do
  me <- listToMaybe <$> findExecutablesInDirectories [dir] exe
  case me of
    Just e -> return (Just e)
    Nothing -> do
      subdirs <- (fmap (dir </>)) <$> listDirectory dir >>= filterM doesDirectoryExist
      foldM (\acc subdir -> case acc of
                              Just y -> pure $ Just y
                              Nothing -> findExeRecursive exe subdir)
            Nothing
            subdirs

-- | So we can find the dummy-server with cabal run
-- since it doesnt put build tools on the path (only cabal test)
findServer = do
  let serverName = "dummy-server"
  e <- findExecutable serverName
  e' <- findExeRecursive serverName "dist-newstyle"
  pure $ fromJust $ e <|> e'