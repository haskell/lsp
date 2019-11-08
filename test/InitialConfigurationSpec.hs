{-# LANGUAGE OverloadedStrings #-}

module InitialConfigurationSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Default
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Types.Capabilities
import           Test.Hspec

spec :: Spec
spec =
  describe "initial configuration" $ it "stores initial configuration data" $ initVFS $ \vfs -> do

    lfVar <- newEmptyMVar

    let
        initialConfigHandler (RequestMessage _ _ Initialize InitializeParams{_initializationOptions = Just opts}) =
          case (fromJSON opts :: Result String) of
                Success s -> Right s
                _         -> Left "Could not decode configuration"
        initialConfigHandler _ =
          error "Got the wrong request for the onInitialConfiguration callback"

        initCb :: InitializeCallbacks String
        initCb = InitializeCallbacks
          initialConfigHandler
          (const $ Left "")
          (\lf -> putMVar lfVar lf >> return Nothing)

        handlers = def

    tvarLspId <- newTVarIO 0
    mvarVfs <- newMVar (VFSData mempty mempty)
    tvarCtx   <- newTVarIO $ defaultLanguageContextData handlers
                                                        def
                                                        undefined
                                                        tvarLspId
                                                        (const $ return ())
                                                        mvarVfs
                                                        Nothing
                                                        vfs

    let putMsg msg =
          let jsonStr = encode msg in handleMessage initCb tvarCtx jsonStr

    let
        initParams        = InitializeParams
          Nothing
          Nothing
          (Just (Uri "/foo"))
          (Just (Data.Aeson.String "configuration"))
          fullCaps
          Nothing
          Nothing

        initMsg :: InitializeRequest
        initMsg = RequestMessage "2.0" (IdInt 0) Initialize initParams

    putMsg initMsg
    contents <- readTVarIO tvarCtx
    resConfig contents  `shouldBe` Just "configuration"

