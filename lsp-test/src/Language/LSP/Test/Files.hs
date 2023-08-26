{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Language.LSP.Protocol.Message as L
import Language.LSP.Protocol.Types
import System.Directory
import System.FilePath
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import Data.Generics.Labels ()
import Language.LSP.MethodInstance ()
import Data.Generics.Product (field')

data Event
  = forall m . ClientEv UTCTime (Message Client L.Method m)
  | forall m . ServerEv UTCTime (Message Server L.Method m)

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
rootDir (ClientEv _ (Req SMethod_Initialize req) : _) =
  fromMaybe (error "Couldn't find root dir") $ do
    rootUri <- case req ^. #params . L.rootUri of
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
  fromClientMsg :: forall m . Message Client L.Method m -> Message Client L.Method m
  fromClientMsg (Req m@SMethod_Initialize r) = Req m (r & (field' @"params") .~ transformInit (r ^. #params))
  fromClientMsg (Not m@SMethod_TextDocumentDidOpen n) = Not m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Not m@SMethod_TextDocumentDidChange n) = Not m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Not m@SMethod_TextDocumentWillSave n) = Not m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Not m@SMethod_TextDocumentDidSave n) = Not m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Not m@SMethod_TextDocumentDidClose n) = Not m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Req m@SMethod_TextDocumentDocumentSymbol n) = Req m $ swapUri (#params . L.textDocument) n
  fromClientMsg (Req m@SMethod_TextDocumentRename n) = Req m $ swapUri (#params . L.textDocument) n
  fromClientMsg x = x

  fromServerMsg :: forall m . Message Server L.Method m -> Message Server L.Method m
  fromServerMsg (Req m@SMethod_WorkspaceApplyEdit r) = Req m $ #params . L.edit .~ swapWorkspaceEdit (r ^. #params . L.edit) $ r
  fromServerMsg (Not m@SMethod_TextDocumentPublishDiagnostics n) = Not m $ swapUri #params n
  fromServerMsg (Rsp m@SMethod_TextDocumentDocumentSymbol r) =
    let swapUri' :: ([SymbolInformation] |? [DocumentSymbol] |? Null) -> [SymbolInformation] |? [DocumentSymbol] |? Null
        swapUri' (InR (InL dss)) = InR $ InL dss -- no file locations here
        swapUri' (InR (InR n)) = InR $ InR n
        swapUri' (InL si) = InL (swapUri L.location <$> si)
     in Rsp m $ r & #result . _Right %~ swapUri'
  fromServerMsg (Rsp m@SMethod_TextDocumentRename r) = Rsp m $ r & #result . _Right . _L %~ swapWorkspaceEdit
  fromServerMsg x = x

  swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
  swapWorkspaceEdit e =
    let swapDocumentChangeUri :: DocumentChange -> DocumentChange
        swapDocumentChangeUri (InL textDocEdit) = InL $ swapUri L.textDocument textDocEdit
        swapDocumentChangeUri (InR (InL createFile)) = InR $ InL $ swapUri Prelude.id createFile
        -- for RenameFile, we swap `newUri`
        swapDocumentChangeUri (InR (InR (InL renameFile))) = InR $ InR $ InL $ L.newUri .~ f (renameFile ^. L.newUri) $ renameFile
        swapDocumentChangeUri (InR (InR (InR deleteFile))) = InR $ InR $ InR $ swapUri Prelude.id deleteFile
     in e
          & L.changes . _Just %~ M.mapKeys f
          & L.documentChanges . _Just . traversed %~ swapDocumentChangeUri

  swapUri :: L.HasUri b Uri => Lens' a b -> a -> a
  swapUri lens = (lens . L.uri) %~ f

  -- Transforms rootUri/rootPath.
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
