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

import           Language.LSP.Protocol.Message hiding (error)
import           Language.LSP.Protocol.Types hiding (id)
import           Control.Lens
import qualified Data.Map.Strict           as M
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
rootDir (ClientEv _ (FromClientMess SMethod_Initialize req):_) =
  fromMaybe (error "Couldn't find root dir") $ do
    rootUri <- case req ^. params . rootUri of
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
    --TODO: Handle all other URIs that might need swapped
    fromClientMsg (FromClientMess m@SMethod_Initialize                 r) = FromClientMess m $ params .~ transformInit (r ^. params) $ r
    fromClientMsg (FromClientMess m@SMethod_TextDocumentDidOpen        n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentDidChange      n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentWillSave       n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentDidSave        n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentDidClose       n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentDocumentSymbol n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg (FromClientMess m@SMethod_TextDocumentRename         n) = FromClientMess m $ swapUri (params . textDocument) n
    fromClientMsg x = x

    fromServerMsg :: FromServerMessage -> FromServerMessage
    fromServerMsg (FromServerMess m@SMethod_WorkspaceApplyEdit r) = FromServerMess m $ params . edit .~ swapWorkspaceEdit (r ^. params . edit) $ r
    fromServerMsg (FromServerMess m@SMethod_TextDocumentPublishDiagnostics n) = FromServerMess m $ swapUri params n
    fromServerMsg (FromServerRsp m@SMethod_TextDocumentDocumentSymbol r) =
      let swapUri' :: ([SymbolInformation] |? [DocumentSymbol] |? Null) -> [SymbolInformation] |? [DocumentSymbol] |? Null
          swapUri' (InR (InL dss)) = InR $ InL dss -- no file locations here
          swapUri' (InR (InR n)) = InR $ InR n
          swapUri' (InL si) = InL (swapUri location <$> si)
      in FromServerRsp m $ r & result . _Right %~ swapUri'
    fromServerMsg (FromServerRsp m@SMethod_TextDocumentRename r) = FromServerRsp m $ r & result . _Right . _L %~ swapWorkspaceEdit
    fromServerMsg x = x

    swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
    swapWorkspaceEdit e =
      let swapDocumentChangeUri :: DocumentChange -> DocumentChange
          swapDocumentChangeUri (InL textDocEdit) = InL $ swapUri textDocument textDocEdit
          swapDocumentChangeUri (InR (InL createFile)) = InR $ InL $ swapUri id createFile
          -- for RenameFile, we swap `newUri`
          swapDocumentChangeUri (InR (InR (InL renameFile))) = InR $ InR $ InL $ newUri .~ f (renameFile ^. newUri) $ renameFile
          swapDocumentChangeUri (InR (InR (InR deleteFile))) = InR $ InR $ InR $ swapUri id deleteFile
       in e & changes . _Just %~ swapKeys f
            & documentChanges . _Just . traversed%~ swapDocumentChangeUri

    swapKeys :: (Uri -> Uri) -> M.Map Uri b -> M.Map Uri b
    swapKeys f = M.foldlWithKey' (\acc k v -> M.insert (f k) v acc) M.empty

    swapUri :: HasUri b Uri => Lens' a b -> a -> a
    swapUri lens x =
      let newUri = f (x ^. lens . uri)
        in (lens . uri) .~ newUri $ x

    -- | Transforms rootUri/rootPath.
    transformInit :: InitializeParams -> InitializeParams
    transformInit x =
      let modifyRootPath p =
            let fp = T.unpack p
                uri = filePathToUri fp
            in case uriToFilePath (f uri) of
              Just fp -> T.pack fp
              Nothing -> p
      in x & rootUri . _L %~ f
           & rootPath . _Just . _L %~ modifyRootPath
