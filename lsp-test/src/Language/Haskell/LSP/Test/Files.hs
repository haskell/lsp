{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , rootDir
  )
where

import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens hiding (error)
import           Language.Haskell.LSP.Messages
import           Control.Lens
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Maybe
import           System.Directory
import           System.FilePath

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
rootDir (FromClient _ (ReqInitialize req):_) =
  fromMaybe (error "Couldn't find root dir") $ do
    rootUri <- req ^. params .rootUri
    uriToFilePath rootUri
rootDir _ = error "Couldn't find initialize request in session"

mapUris :: (Uri -> Uri) -> Event -> Event
mapUris f event =
  case event of
    FromClient t msg -> FromClient t (fromClientMsg msg)
    FromServer t msg -> FromServer t (fromServerMsg msg)

  where
    --TODO: Handle all other URIs that might need swapped
    fromClientMsg (NotDidOpenTextDocument n) = NotDidOpenTextDocument $ swapUri (params . textDocument) n
    fromClientMsg (NotDidChangeTextDocument n) = NotDidChangeTextDocument $ swapUri (params . textDocument) n
    fromClientMsg (NotWillSaveTextDocument n) = NotWillSaveTextDocument $ swapUri (params . textDocument) n
    fromClientMsg (NotDidSaveTextDocument n) = NotDidSaveTextDocument $ swapUri (params . textDocument) n
    fromClientMsg (NotDidCloseTextDocument n) = NotDidCloseTextDocument $ swapUri (params . textDocument) n
    fromClientMsg (ReqInitialize r) = ReqInitialize $ params .~ transformInit (r ^. params) $ r
    fromClientMsg (ReqDocumentSymbols r) = ReqDocumentSymbols $ swapUri (params . textDocument) r
    fromClientMsg (ReqRename r) = ReqRename $ swapUri (params . textDocument) r
    fromClientMsg x = x

    fromServerMsg :: FromServerMessage -> FromServerMessage
    fromServerMsg (ReqApplyWorkspaceEdit r) =
      ReqApplyWorkspaceEdit $ params . edit .~ swapWorkspaceEdit (r ^. params . edit) $ r

    fromServerMsg (NotPublishDiagnostics n) = NotPublishDiagnostics $ swapUri params n

    fromServerMsg (RspDocumentSymbols r) =
      let newSymbols = case r ^. result of
            Just (DSSymbolInformation si) -> Just (DSSymbolInformation (fmap (swapUri location) si))
            x -> x
      in RspDocumentSymbols $ result .~ newSymbols $ r

    fromServerMsg (RspRename r) =
      let oldResult = r ^. result :: Maybe WorkspaceEdit
          newResult = fmap swapWorkspaceEdit oldResult
      in RspRename $ result .~ newResult $ r

    fromServerMsg x = x

    swapWorkspaceEdit :: WorkspaceEdit -> WorkspaceEdit
    swapWorkspaceEdit e =
      let newDocChanges = fmap (fmap (swapUri textDocument)) $ e ^. documentChanges
          newChanges = fmap (swapKeys f) $ e ^. changes
      in WorkspaceEdit newChanges newDocChanges

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
