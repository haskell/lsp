{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Test.Files (
  swapFiles,
  rootDir,
)
where

import Control.Lens
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Time.Clock
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import System.Directory
import System.FilePath

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
rootDir (ClientEv _ (FromClientMess SMethod_Initialize req) : _) =
  fromMaybe (error "Couldn't find root dir") $ do
    rootUri <- case req ^. L.params . L.rootUri of
      InL r -> Just r
      InR _ -> error "Couldn't find root dir"
    uriToFilePath rootUri
rootDir _ = error "Couldn't find initialize request in session"

mapUris :: (Uri -> Uri) -> Event -> Event
mapUris f event =
  case event of
    ClientEv t msg -> ClientEv t (fromClientMsg msg)
    ServerEv t msg -> ServerEv t (fromServerMsg msg)
 where
  -- TODO: Handle all other URIs that might need swapped
  fromClientMsg (FromClientMess m@SMethod_Initialize r) = FromClientMess m $ L.params .~ transformInit (r ^. L.params) $ r
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidOpen n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidChange n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentWillSave n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidSave n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidClose n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDocumentSymbol n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg (FromClientMess m@SMethod_TextDocumentRename n) = FromClientMess m $ swapUri (L.params . L.textDocument) n
  fromClientMsg x = x

  fromServerMsg :: FromServerMessage -> FromServerMessage
  fromServerMsg (FromServerMess m@SMethod_WorkspaceApplyEdit r) = FromServerMess m $ L.params . L.edit .~ swapWorkspaceEdit (r ^. L.params . L.edit) $ r
  fromServerMsg (FromServerMess m@SMethod_TextDocumentPublishDiagnostics n) = FromServerMess m $ swapUri L.params n
  fromServerMsg (FromServerRsp m@SMethod_TextDocumentDocumentSymbol r) =
    let swapUri' :: ([SymbolInformation] |? [DocumentSymbol] |? Null) -> [SymbolInformation] |? [DocumentSymbol] |? Null
        swapUri' (InR (InL dss)) = InR $ InL dss -- no file locations here
        swapUri' (InR (InR n)) = InR $ InR n
        swapUri' (InL si) = InL (swapUri L.location <$> si)
     in FromServerRsp m $ r & L.result . _Right %~ swapUri'
  fromServerMsg (FromServerRsp m@SMethod_TextDocumentRename r) = FromServerRsp m $ r & L.result . _Right . _L %~ swapWorkspaceEdit
  fromServerMsg x = x

  swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
  swapWorkspaceEdit e =
    let swapDocumentChangeUri :: DocumentChange -> DocumentChange
        swapDocumentChangeUri (InL textDocEdit) = InL $ swapUri L.textDocument textDocEdit
        swapDocumentChangeUri (InR (InL createFile)) = InR $ InL $ swapUri id createFile
        -- for RenameFile, we swap `newUri`
        swapDocumentChangeUri (InR (InR (InL renameFile))) = InR $ InR $ InL $ L.newUri .~ f (renameFile ^. L.newUri) $ renameFile
        swapDocumentChangeUri (InR (InR (InR deleteFile))) = InR $ InR $ InR $ swapUri id deleteFile
     in e
          & L.changes . _Just %~ swapKeys f
          & L.documentChanges . _Just . traversed %~ swapDocumentChangeUri

  swapKeys :: (Uri -> Uri) -> M.Map Uri b -> M.Map Uri b
  swapKeys f = M.foldlWithKey' (\acc k v -> M.insert (f k) v acc) M.empty

  swapUri :: L.HasUri b Uri => Lens' a b -> a -> a
  swapUri lens x =
    let newUri = f (x ^. lens . L.uri)
     in (lens . L.uri) .~ newUri $ x

  -- \| Transforms rootUri/rootPath.
  transformInit :: InitializeParams -> InitializeParams
  transformInit x =
    let modifyRootPath p =
          let fp = T.unpack p
              uri = filePathToUri fp
           in case uriToFilePath (f uri) of
                Just fp -> T.pack fp
                Nothing -> p
     in x
          & L.rootUri . _L %~ f
          & L.rootPath . _Just . _L %~ modifyRootPath
