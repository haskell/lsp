{-# LANGUAGE OverloadedStrings #-}

module WorkspaceFoldersSpec where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types hiding (error, trace)
import Language.Haskell.LSP.Types.Capabilities
import Test.Hspec

spec :: Spec
spec = describe "workspace folders" $
  it "keeps track of open workspace folders" $ do

    lfVar <- newEmptyMVar

    let initCb :: InitializeCallback ()
        initCb = (const $ Left "", \lf -> putMVar lfVar lf >> return Nothing)
        handlers = def

    tvarLspId <- newTVarIO 0
    tvarCtx <- newTVarIO $ defaultLanguageContextData handlers def (error "ass") tvarLspId (const $ return ()) Nothing

    let putMsg msg =
          let jsonStr = encode msg
              clStr = BSL.pack $ "Content-Length: " ++ show (BSL.length jsonStr)
            in handleMessage initCb tvarCtx clStr jsonStr

    let starterWorkspaces = List [wf0]
        initParams = InitializeParams Nothing Nothing (Just (Uri "/foo")) Nothing fullCaps Nothing (Just starterWorkspaces)
        initMsg :: InitializeRequest
        initMsg = RequestMessage "2.0" (IdInt 0) Initialize initParams

    putMsg initMsg

    firstWorkspaces <- readMVar lfVar >>= getWorkspaceFolders
    firstWorkspaces `shouldBe` Just [wf0]


    putMsg (makeNotif [wf1] [])
    readMVar lfVar >>= \lf -> do
      Just wfs <- getWorkspaceFolders lf
      wfs `shouldContain` [wf1]
      wfs `shouldContain` [wf0]

    putMsg (makeNotif [wf2] [wf1])
    readMVar lfVar >>= \lf -> do
      Just wfs <- getWorkspaceFolders lf
      wfs `shouldNotContain` [wf1]
      wfs `shouldContain` [wf0]
      wfs `shouldContain` [wf2]

  where
    wf0 = WorkspaceFolder "one" "Starter workspace"
    wf1 = WorkspaceFolder "/foo/bar" "My workspace"
    wf2 = WorkspaceFolder "/foo/baz" "My other workspace"

    makeNotif add rmv =
      let addedFolders = List add
          removedFolders = List rmv
          ev = WorkspaceFoldersChangeEvent addedFolders removedFolders
          ps = DidChangeWorkspaceFoldersParams ev
        in NotificationMessage "2.0" WorkspaceDidChangeWorkspaceFolders ps
