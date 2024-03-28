{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Test.Files (
  swapFiles,
  rootDir,
)
where

import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (field')
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Time.Clock
import Language.LSP.Protocol.Message hiding (error)
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
    rootUri <- case req.params.rootUri of
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
  fromClientMsg (FromClientMess m@SMethod_Initialize r) = FromClientMess m $ r & #params %~ transformInit
  -- in FromClientMess m $ msg { params = transformInit p }
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidOpen n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidChange n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentWillSave n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidSave n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDidClose n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentDocumentSymbol n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg (FromClientMess m@SMethod_TextDocumentRename n) = FromClientMess m $ n & field' @"params" . #textDocument . #uri %~ f
  fromClientMsg x = x

  fromServerMsg :: FromServerMessage -> FromServerMessage
  fromServerMsg (FromServerMess m@SMethod_WorkspaceApplyEdit r) = FromServerMess m $ r & field' @"params" . #edit %~ swapWorkspaceEdit
  fromServerMsg (FromServerMess m@SMethod_TextDocumentPublishDiagnostics n) = FromServerMess m $ n & field' @"params" . #uri %~ f
  fromServerMsg (FromServerRsp m@SMethod_TextDocumentDocumentSymbol r) =
    let swapUri' :: ([SymbolInformation] |? [DocumentSymbol] |? Null) -> [SymbolInformation] |? [DocumentSymbol] |? Null
        swapUri' (InR (InL dss)) = InR $ InL dss -- no file locations here
        swapUri' (InR (InR n)) = InR $ InR n
        swapUri' (InL si) = InL (swapUri (#location . #uri) <$> si)
     in FromServerRsp m $ r & #result . _Right %~ swapUri'
  fromServerMsg (FromServerRsp m@SMethod_TextDocumentRename r) = FromServerRsp m $ r & field' @"result" . _Right . #_InL %~ swapWorkspaceEdit
  fromServerMsg x = x

  swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
  swapWorkspaceEdit e =
    let swapDocumentChangeUri :: DocumentChange -> DocumentChange
        swapDocumentChangeUri (InL textDocEdit) = InL $ swapUri (#textDocument . #uri) textDocEdit
        swapDocumentChangeUri (InR (InL createFile)) = InR $ InL $ swapUri #uri createFile
        -- for RenameFile, we swap `newUri`
        swapDocumentChangeUri (InR (InR (InL renameFile))) = InR $ InR $ InL $ #newUri .~ f (renameFile ^. #newUri) $ renameFile
        swapDocumentChangeUri (InR (InR (InR deleteFile))) = InR $ InR $ InR $ swapUri #uri deleteFile
     in e
          & #changes . _Just %~ swapKeys f
          & #documentChanges . _Just . traversed %~ swapDocumentChangeUri

  swapKeys :: (Uri -> Uri) -> M.Map Uri b -> M.Map Uri b
  swapKeys f = M.foldlWithKey' (\acc k v -> M.insert (f k) v acc) M.empty

  swapUri :: Lens' a Uri -> a -> a
  swapUri lens x = x & lens %~ f

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
          & #rootUri . #_InL %~ f
          & #rootPath . _Just . #_InL %~ modifyRootPath
