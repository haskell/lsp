{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.LSP.Test.Files
  ( swapFiles
  , rootDir
  )
where

import           Language.LSP.Types
import           Language.LSP.Types.Lens hiding (id)
import           Control.Lens
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Maybe
import           System.Directory
import           System.FilePath
import Data.Time.Clock

data Event
  = ClientEv UTCTime FromClientMessage
  | ServerEv UTCTime FromServerMessage

swapFiles :: FilePath -> [Event] -> IO [Event]
swapFiles relCurBaseDir msgs = do
  let capturedBaseDir = rootDir msgs

  curBaseDir <- (</> relCurBaseDir) <$> getCurrentDirectory
  let transform uri =
        let fp = fromMaybe (error "Couldn't transform uri") (uriToFilePath uri)
            newFp = curBaseDir </> makeRelative capturedBaseDir fp
          in filePathToUri newFp
      newMsgs = map (mapUris transform) msgs

  return newMsgs

rootDir :: [Event] -> FilePath
rootDir (ClientEv _ (FromClientMess SInitialize req):_) =
  fromMaybe (error "Couldn't find root dir") $ do
    rootUri <- req ^. params .rootUri
    uriToFilePath rootUri
rootDir _ = error "Couldn't find initialize request in session"

mapUris :: (Uri -> Uri) -> Event -> Event
mapUris f event =
  case event of
    ClientEv t msg -> ClientEv t (fromClientMsg msg)
    ServerEv t msg -> ServerEv t (fromServerMsg msg)

  where
    --TODO: Handle all other URIs that might need swapped
    fromClientMsg (FromClientMess m@SInitialize                 r) = FromClientMess m $ params .~ transformInit (r ^. params) $ r
    fromClientMsg (FromClientMess m@STextDocumentDidOpen        n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentDidChange      n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentWillSave       n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentDidSave        n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentDidClose       n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentDocumentSymbol n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@STextDocumentRename         n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg x = x

    fromServerMsg :: FromServerMessage -> FromServerMessage
    fromServerMsg (FromServerMess m@SWorkspaceApplyEdit r) = FromServerMess m $ params . edit .~ swapWorkspaceEdit (r ^. params . edit) $ r
    fromServerMsg (FromServerMess m@STextDocumentPublishDiagnostics n) = FromServerMess m $ swapUri params n
    fromServerMsg (FromServerRsp m@STextDocumentDocumentSymbol r) =
      let swapUri' :: (List DocumentSymbol |? List SymbolInformation) -> List DocumentSymbol |? List SymbolInformation
          swapUri' (InR si) = InR (swapUri location <$> si)
          swapUri' (InL dss) = InL dss -- no file locations here
      in FromServerRsp m $ r & result %~ (fmap swapUri')
    fromServerMsg (FromServerRsp m@STextDocumentRename r) = FromServerRsp m $ r & result %~ (fmap swapWorkspaceEdit)
    fromServerMsg x = x

    swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
    swapWorkspaceEdit e =
      let swapDocumentChangeUri :: DocumentChange -> DocumentChange
          swapDocumentChangeUri (InL textDocEdit) = InL $ swapUri textDocument textDocEdit
          swapDocumentChangeUri (InR (InL createFile)) = InR $ InL $ swapUri id createFile
          -- for RenameFile, we swap `newUri`
          swapDocumentChangeUri (InR (InR (InL renameFile))) = InR $ InR $ InL $ newUri .~ f (renameFile ^. newUri) $ renameFile
          swapDocumentChangeUri (InR (InR (InR deleteFile))) = InR $ InR $ InR $ swapUri id deleteFile

          newDocChanges = fmap (fmap swapDocumentChangeUri) $ e ^. documentChanges
          newChanges = fmap (swapKeys f) $ e ^. changes
       in WorkspaceEdit newChanges newDocChanges Nothing

    swapKeys :: (Uri -> Uri) -> HM.HashMap Uri b -> HM.HashMap Uri b
    swapKeys f = HM.foldlWithKey' (\acc k v -> HM.insert (f k) v acc) HM.empty

    swapUri :: HasUri b Uri => Lens' a b -> a -> a
    swapUri lens x =
      let newUri = f (x ^. lens . uri)
        in (lens . uri) .~ newUri $ x

    -- | Transforms rootUri/rootPath.
    transformInit :: InitializeParams -> InitializeParams
    transformInit x =
      let newRootUri = fmap f (x ^. rootUri)
          newRootPath = do
            fp <- T.unpack <$> x ^. rootPath
            let uri = filePathToUri fp
            T.pack <$> uriToFilePath (f uri)
        in (rootUri .~ newRootUri) $ (rootPath .~ newRootPath) x
