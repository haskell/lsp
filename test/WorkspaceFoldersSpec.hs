{-# LANGUAGE OverloadedStrings #-}

module WorkspaceFoldersSpec where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Aeson
import Data.Default
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Types.Capabilities
import Test.Hspec

spec :: Spec
spec = pure ()
  -- TODO: Convert to a functional test
  -- describe "workspace folders" $ it "keeps track of open workspace folders" $ initVFS $ \vfs -> do

  --   envVar <- newEmptyMVar

  --   let initCb :: InitializeCallbacks String
  --       initCb = InitializeCallbacks
  --         initialConfigHandler
  --         (const $ pure $ Left "")
  --         (LspT ask >>= liftIO . putMVar envVar >> return Nothing)

  --   tvarLspId <- newTVarIO 0
  --   tvarCtx   <- newTVarIO $ defaultLanguageContextState handlers
  --                                                       def
  --                                                       undefined
  --                                                       tvarLspId
  --                                                       (const $ return ())
  --                                                       noCapture
  --                                                       vfs

  --   let putMsg msg =
  --         let jsonStr = encode msg
  --           in processMessage initCb tvarCtx jsonStr

  --   let starterWorkspaces = List [wf0]
  --       initParams = InitializeParams
  --         Nothing Nothing (Just (Uri "/foo")) Nothing fullCaps Nothing (Just starterWorkspaces)
  --       initMsg :: InitializeRequest
  --       initMsg = RequestMessage "2.0" (IdInt 0) Initialize initParams

  --   putMsg initMsg

  --   firstWorkspaces <- readMVar lfVar >>= getWorkspaceFolders
  --   firstWorkspaces `shouldBe` Just [wf0]


  --   putMsg (makeNotif [wf1] [])
  --   readMVar lfVar >>= \lf -> do
  --     Just wfs <- getWorkspaceFolders lf
  --     wfs `shouldContain` [wf1]
  --     wfs `shouldContain` [wf0]

  --   putMsg (makeNotif [wf2] [wf1])
  --   readMVar lfVar >>= \lf -> do
  --     Just wfs <- getWorkspaceFolders lf
  --     wfs `shouldNotContain` [wf1]
  --     wfs `shouldContain` [wf0]
  --     wfs `shouldContain` [wf2]

  -- where
  --   wf0 = WorkspaceFolder "one" "Starter workspace"
  --   wf1 = WorkspaceFolder "/foo/bar" "My workspace"
  --   wf2 = WorkspaceFolder "/foo/baz" "My other workspace"

  --   makeNotif add rmv =
  --     let addedFolders = List add
  --         removedFolders = List rmv
  --         ev = WorkspaceFoldersChangeEvent addedFolders removedFolders
  --         ps = DidChangeWorkspaceFoldersParams ev
  --       in NotificationMessage "2.0" WorkspaceDidChangeWorkspaceFolders ps
