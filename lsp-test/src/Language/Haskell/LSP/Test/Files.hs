{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , rootDir
  )
where

import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Types hiding ( error )
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
    fromClientMsg (ReqInitialize r) = ReqInitialize $ params .~ (transformInit (r ^. params)) $ r
    fromClientMsg x = x

    fromServerMsg :: FromServerMessage -> FromServerMessage
    fromServerMsg (ReqApplyWorkspaceEdit r) =
      let newDocChanges = fmap (fmap (swapUri textDocument)) $ r ^. params . edit . documentChanges
          r1 = (params . edit . documentChanges) .~ newDocChanges $ r
          newChanges = fmap (swapKeys f) $ r1 ^. params . edit . changes
          r2 = (params . edit . changes) .~ newChanges $ r1
      in ReqApplyWorkspaceEdit r2
    fromServerMsg x = x

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
