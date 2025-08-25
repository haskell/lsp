{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson qualified as J
import Data.Default
import Data.Either
import Data.List.Extra
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import DummyServer
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory
import System.FilePath
import System.Timeout
import Test.Hspec

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}

main = hspec $ around withDummyServer $ do
  describe "Session" $ do
    it "fails a test" $ \(hin, hout) ->
      let session = runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
            openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
            anyRequest
       in session `shouldThrow` anySessionException
    it "initializeResponse" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      rsp <- initializeResponse
      liftIO $ rsp ^. L.result `shouldSatisfy` isRight

    it "runSessionWithConfig" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullLatestClientCaps "." $ return ()

    describe "withTimeout" $ do
      it "times out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
              openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
              -- won't receive a request - will timeout
              -- incoming logging requests shouldn't increase the
              -- timeout
              withTimeout 5 $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
         in -- wait just a bit longer than 5 seconds so we have time
            -- to open the document
            timeout 6000000 sesh `shouldThrow` anySessionException

      it "doesn't time out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
              openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
              withTimeout 5 $ skipManyTill anyMessage publishDiagnosticsNotification
         in void $ timeout 6000000 sesh

      it "further timeout messages are ignored" $ \(hin, hout) ->
        runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
          doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
          -- shouldn't timeout
          withTimeout 3 $ getDocumentSymbols doc
          -- longer than the original timeout
          liftIO $ threadDelay (5 * 10 ^ 6)
          -- shouldn't throw an exception
          getDocumentSymbols doc
          return ()

      it "overrides global message timeout" $ \(hin, hout) ->
        let sesh =
              runSessionWithHandles hin hout (def{messageTimeout = 5}) fullLatestClientCaps "." $ do
                doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                return True
         in sesh `shouldReturn` True

      it "unoverrides global message timeout" $ \(hin, hout) ->
        let sesh =
              runSessionWithHandles hin hout (def{messageTimeout = 5}) fullLatestClientCaps "." $ do
                doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                -- should now timeout
                skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
            isTimeout (Timeout _) = True
            isTimeout _ = False
         in sesh `shouldThrow` isTimeout

    describe "SessionException" $ do
      it "throw on time out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout (def{messageTimeout = 10}) fullLatestClientCaps "." $ do
              _ <- message SMethod_WorkspaceApplyEdit
              return ()
         in sesh `shouldThrow` anySessionException

      it "don't throw when no time out" $ \(hin, hout) ->
        runSessionWithHandles hin hout (def{messageTimeout = 5}) fullLatestClientCaps "." $ do
          liftIO $ threadDelay $ 6 * 1000000
          _ <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
          return ()

      describe "UnexpectedMessageException" $ do
        it "throws when there's an unexpected message" $ \(hin, hout) ->
          let selector (UnexpectedMessage "Publish diagnostics notification" (FromServerMess SMethod_WindowLogMessage _)) = True
              selector _ = False
           in runSessionWithHandles hin hout (def{ignoreLogNotifications = False}) fullLatestClientCaps "." publishDiagnosticsNotification `shouldThrow` selector
        it "provides the correct types that were expected and received" $ \(hin, hout) ->
          let selector (UnexpectedMessage "Response for: SMethod_TextDocumentRename" (FromServerRsp SMethod_TextDocumentDocumentSymbol _)) = True
              selector _ = False
              sesh = do
                doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
                sendRequest SMethod_TextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
                skipMany anyNotification
                response SMethod_TextDocumentRename -- the wrong type
           in runSessionWithHandles hin hout def fullLatestClientCaps "." sesh
                `shouldThrow` selector

  describe "config" $ do
    it "updates config correctly" $ \(hin, hout) ->
      runSessionWithHandles hin hout (def{ignoreConfigurationRequests = False}) fullLatestClientCaps "." $ do
        configurationRequest -- initialized configuration request
        let requestConfig = do
              resp <- request (SMethod_CustomMethod (Proxy @"getConfig")) J.Null
              case resp ^? L.result . _Right of
                Just val -> case fromJSON @Int val of
                  J.Success v -> pure v
                  J.Error err -> fail err
                Nothing -> fail "no result"

        c <- requestConfig
        -- from the server definition
        liftIO $ c `shouldBe` 1
        setConfigSection "dummy" (toJSON @Int 2)
        -- ensure the configuration change has happened
        configurationRequest
        c <- requestConfig
        liftIO $ c `shouldBe` 2

        pure ()

  describe "text document VFS" $ do
    it "sends back didChange notifications (documentChanges)" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
        doc <- openDoc "test/data/refactor/Main.hs" "haskell"
        VersionedTextDocumentIdentifier _ beforeVersion <- getVersionedDoc doc

        let args = toJSON (VersionedTextDocumentIdentifier (doc ^. L.uri) beforeVersion)
            reqParams = ExecuteCommandParams Nothing "doAVersionedEdit" (Just [args])

        request_ SMethod_WorkspaceExecuteCommand reqParams

        editReq <- message SMethod_WorkspaceApplyEdit
        liftIO $ do
          let Just [InL (TextDocumentEdit vdoc [InL edit_])] =
                editReq ^. L.params . L.edit . L.documentChanges
          vdoc `shouldBe` OptionalVersionedTextDocumentIdentifier (doc ^. L.uri) (InL beforeVersion)
          edit_ `shouldBe` TextEdit (Range (Position 0 0) (Position 0 5)) "howdy"

        change <- customNotification (Proxy @"custom/textDocument/didChange")
        let NotMess (TNotificationMessage _ _ (c :: Value)) = change
            Success (DidChangeTextDocumentParams reportedVDoc _edit) = fromJSON c
            VersionedTextDocumentIdentifier _ reportedVersion = reportedVDoc

        contents <- documentContents doc

        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"
        VersionedTextDocumentIdentifier _ afterVersion <- getVersionedDoc doc
        liftIO $ afterVersion `shouldNotBe` beforeVersion

        liftIO $ reportedVersion `shouldNotBe` beforeVersion

    it "sends back didChange notifications" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
        doc <- openDoc "test/data/refactor/Main.hs" "haskell"

        let args = toJSON (doc ^. L.uri)
            reqParams = ExecuteCommandParams Nothing "doAnEdit" (Just [args])
        request_ SMethod_WorkspaceExecuteCommand reqParams

        editReq <- message SMethod_WorkspaceApplyEdit
        liftIO $ do
          let (Just cs) = editReq ^. L.params . L.edit . L.changes
              [(u, es)] = M.toList cs
          u `shouldBe` doc ^. L.uri
          es `shouldBe` [TextEdit (Range (Position 0 0) (Position 0 5)) "howdy"]
        contents <- documentContents doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getDocumentEdit" $
    it "automatically consumes applyedit requests" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
        doc <- openDoc "test/data/refactor/Main.hs" "haskell"

        let args = toJSON (doc ^. L.uri)
            reqParams = ExecuteCommandParams Nothing "doAnEdit" (Just [args])
        request_ SMethod_WorkspaceExecuteCommand reqParams
        contents <- getDocumentEdit doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getCodeActions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/refactor/Main.hs" "haskell"
      waitForDiagnostics
      [InR action] <- getCodeActions doc (Range (Position 0 0) (Position 0 2))
      actions <- getCodeActions doc (Range (Position 1 14) (Position 1 18))
      liftIO $ action ^. L.title `shouldBe` "Delete this"
      liftIO $ actions `shouldSatisfy` null

  describe "getAllCodeActions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/refactor/Main.hs" "haskell"
      _ <- waitForDiagnostics
      actions <- getAllCodeActions doc
      liftIO $ do
        let [InR action] = actions
        action ^. L.title `shouldBe` "Delete this"
        action ^. L.command . _Just . L.command `shouldBe` "deleteThis"

  describe "getDocumentSymbols" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"

      Right (mainSymbol : _) <- getDocumentSymbols doc

      liftIO $ do
        mainSymbol ^. L.name `shouldBe` "foo"
        mainSymbol ^. L.kind `shouldBe` SymbolKind_Object
        mainSymbol ^. L.range `shouldBe` mkRange 0 0 3 6

  describe "applyEdit" $ do
    it "increments the version" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      VersionedTextDocumentIdentifier _ oldVersion <- getVersionedDoc doc
      let edit = TextEdit (Range (Position 1 1) (Position 1 3)) "foo"
      VersionedTextDocumentIdentifier _ newVersion <- applyEdit doc edit
      liftIO $ newVersion `shouldBe` oldVersion + 1
    it "changes the document contents" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      let edit = TextEdit (Range (Position 0 0) (Position 0 2)) "foo"
      applyEdit doc edit
      contents <- documentContents doc
      liftIO $ contents `shouldSatisfy` T.isPrefixOf "foodule"

  describe "getCompletions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"

      comps <- getCompletions doc (Position 5 5)
      let item = head comps
      liftIO $ item ^. L.label `shouldBe` "foo"

  -- describe "getReferences" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 40 3 -- interactWithUser
  --         uri = doc ^. LSP.uri
  --     refs <- getReferences doc pos True
  --     liftIO $ refs `shouldContain` map (Location uri) [
  --         mkRange 41 0 41 16
  --       , mkRange 75 6 75 22
  --       , mkRange 71 6 71 22
  --       ]

  -- describe "getDefinitions" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 49 25 -- addItem
  --     defs <- getDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 28 0 28 7)]

  -- describe "getTypeDefinitions" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 20 23  -- Quit value
  --     defs <- getTypeDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 10 0 14 19)]  -- Type definition

  describe "waitForDiagnosticsSource" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      openDoc "test/data/Error.hs" "haskell"
      [diag] <- waitForDiagnosticsSource "dummy-server"
      liftIO $ do
        diag ^. L.severity `shouldBe` Just DiagnosticSeverity_Warning
        diag ^. L.source `shouldBe` Just "dummy-server"

  -- describe "rename" $ do
  --   it "works" $ \(hin, hout) -> pendingWith "HaRe not in hie-bios yet"
  --   it "works on javascript" $
  --     runSessionWithHandles hin hout "javascript-typescript-stdio" fullLatestClientCaps "test/data/javascriptPass" $ do
  --       doc <- openDoc "test.js" "javascript"
  --       rename doc (Position 2 11) "bar"
  --       documentContents doc >>= liftIO . (`shouldContain` "function bar()") . T.unpack

  describe "getHover" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      hover <- getHover doc (Position 45 9)
      liftIO $ hover `shouldSatisfy` isJust

  describe "getSignatureHelp" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      signatureHelp <- getSignatureHelp doc (Position 22 32) Nothing
      liftIO $ signatureHelp `shouldSatisfy` isJust

  -- describe "getHighlights" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
  --     skipManyTill loggingNotification $ count 2 noDiagnostics
  --     highlights <- getHighlights doc (Position 27 4) -- addItem
  --     liftIO $ length highlights `shouldBe` 4

  -- describe "formatDoc" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatDoc doc (FormattingOptions 4 True)
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  -- describe "formatRange" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
  --     doc <- openDoc "test/data/Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatRange doc (FormattingOptions 4 True) (Range (Position 1 10) (Position 2 10))
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  describe "closeDoc" $
    it "works" $ \(hin, hout) ->
      let sesh =
            runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
              doc <- openDoc "test/data/Format.hs" "haskell"
              closeDoc doc
              -- need to evaluate to throw
              documentContents doc >>= liftIO . print
       in sesh `shouldThrow` anyException

  describe "satisfy" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout (def{ignoreLogNotifications = False}) fullLatestClientCaps "." $ do
      openDoc "test/data/Format.hs" "haskell"
      let pred (FromServerMess SMethod_WindowLogMessage _) = True
          pred _ = False
      void $ satisfy pred

  describe "satisfyMaybe" $ do
    it "returns matched data on match" $ \(hin, hout) -> runSessionWithHandles hin hout (def{ignoreLogNotifications = False}) fullLatestClientCaps "." $ do
      -- Wait for window/logMessage "initialized" from the server.
      let pred (FromServerMess SMethod_WindowLogMessage _) = Just "match" :: Maybe String
          pred _ = Nothing :: Maybe String
      result <- satisfyMaybe pred
      liftIO $ result `shouldBe` "match"

    it "doesn't return if no match" $ \(hin, hout) -> runSessionWithHandles hin hout (def{ignoreLogNotifications = False}) fullLatestClientCaps "." $ do
      let pred (FromServerMess SMethod_TextDocumentPublishDiagnostics _) = Just "matched" :: Maybe String
          pred _ = Nothing :: Maybe String
      -- We expect a window/logMessage from the server, but
      -- not a textDocument/publishDiagnostics.
      result <- satisfyMaybe pred <|> (message SMethod_WindowLogMessage *> pure "no match")
      liftIO $ result `shouldBe` "no match"

  describe "ignoreLogNotifications" $
    it "works" $ \(hin, hout) ->
      runSessionWithHandles hin hout (def{ignoreLogNotifications = True}) fullLatestClientCaps "." $ do
        openDoc "test/data/Format.hs" "haskell"
        void publishDiagnosticsNotification

  describe "dynamic capabilities" $ do
    let config = def{ignoreLogNotifications = False}
    it "keeps track" $ \(hin, hout) -> runSessionWithHandles hin hout config fullLatestClientCaps "." $ do
      loggingNotification -- initialized log message
      createDoc ".register" "haskell" ""
      setIgnoringRegistrationRequests False
      message SMethod_ClientRegisterCapability

      doc <- createDoc "Foo.watch" "haskell" ""
      msg <- message SMethod_WindowLogMessage
      liftIO $ msg ^. L.params . L.message `shouldBe` "got workspace/didChangeWatchedFiles"

      -- Look for the registration, we might have one for didChangeConfiguration in there too
      registeredCaps <- getRegisteredCapabilities
      let
        regOpts :: Maybe DidChangeWatchedFilesRegistrationOptions
        regOpts = flip firstJust registeredCaps $ \(SomeRegistration (TRegistration _ regMethod regOpts)) ->
          case regMethod of
            SMethod_WorkspaceDidChangeWatchedFiles -> regOpts
            _ -> Nothing
      liftIO $
        regOpts
          `shouldBe` ( Just $
                        DidChangeWatchedFilesRegistrationOptions
                          [FileSystemWatcher (GlobPattern $ InL $ Pattern "*.watch") (Just WatchKind_Create)]
                     )

      -- now unregister it by sending a specific createDoc
      createDoc ".unregister" "haskell" ""
      message SMethod_ClientUnregisterCapability

      createDoc "Bar.watch" "haskell" ""
      void $ sendRequest SMethod_TextDocumentHover $ HoverParams doc (Position 0 0) Nothing
      void $ anyResponse

    it "handles absolute patterns" $ \(hin, hout) -> runSessionWithHandles hin hout config fullLatestClientCaps "" $ do
      loggingNotification -- initialized log message
      curDir <- liftIO $ getCurrentDirectory

      setIgnoringRegistrationRequests False
      createDoc ".register.abs" "haskell" ""
      message SMethod_ClientRegisterCapability

      doc <- createDoc (curDir </> "Foo.watch") "haskell" ""
      msg <- message SMethod_WindowLogMessage
      liftIO $ msg ^. L.params . L.message `shouldBe` "got workspace/didChangeWatchedFiles"

      -- now unregister it by sending a specific createDoc
      createDoc ".unregister.abs" "haskell" ""
      message SMethod_ClientUnregisterCapability

      createDoc (curDir </> "Bar.watch") "haskell" ""
      void $ sendRequest SMethod_TextDocumentHover $ HoverParams doc (Position 0 0) Nothing
      void $ anyResponse

  describe "call hierarchy" $ do
    let workPos = Position 1 0
        notWorkPos = Position 0 0
        params pos = CallHierarchyPrepareParams (TextDocumentIdentifier (Uri "")) pos Nothing
        item =
          CallHierarchyItem
            "foo"
            SymbolKind_Function
            Nothing
            Nothing
            (Uri "")
            (Range (Position 1 2) (Position 3 4))
            (Range (Position 1 2) (Position 3 4))
            Nothing
    it "prepare works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      rsp <- prepareCallHierarchy (params workPos)
      liftIO $ head rsp ^. L.range `shouldBe` Range (Position 2 3) (Position 4 5)
    it "prepare not works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      rsp <- prepareCallHierarchy (params notWorkPos)
      liftIO $ rsp `shouldBe` []
    it "incoming calls" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      [CallHierarchyIncomingCall _ fromRanges] <- incomingCalls (CallHierarchyIncomingCallsParams Nothing Nothing item)
      liftIO $ head fromRanges `shouldBe` Range (Position 2 3) (Position 4 5)
    it "outgoing calls" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      [CallHierarchyOutgoingCall _ fromRanges] <- outgoingCalls (CallHierarchyOutgoingCallsParams Nothing Nothing item)
      liftIO $ head fromRanges `shouldBe` Range (Position 4 5) (Position 2 3)

  describe "semantic tokens" $ do
    it "full works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      let doc = TextDocumentIdentifier (Uri "")
      InL toks <- getSemanticTokens doc
      liftIO $ toks ^. L.data_ `shouldBe` [0, 1, 2, 1, 0]

  describe "inlay hints" $ do
    it "get works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      inlayHints <- getInlayHints doc (Range (Position 1 2) (Position 3 4))
      liftIO $ head inlayHints ^. L.label `shouldBe` InL ":: Text"
    it "resolve tooltip works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullLatestClientCaps "." $ do
      doc <- openDoc "test/data/renamePass/Desktop/simple.hs" "haskell"
      inlayHints <- getAndResolveInlayHints doc (Range (Position 1 2) (Position 3 4))
      liftIO $ head inlayHints ^. L.tooltip `shouldBe` Just (InL $ "start at " <> T.pack (show (Position 1 2)))
