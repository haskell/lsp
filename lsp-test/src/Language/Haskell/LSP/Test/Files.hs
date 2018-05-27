{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , FileMap
  , emptyFileMap
  , rootDir
  , cleanupFiles
  )
where

import           Language.Haskell.LSP.Types        hiding ( error )
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Text                     as T
import qualified Data.Map                      as Map
import           Data.Map ((!))
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Set                      as Set
import           Data.Maybe
import           System.Directory
import           System.IO
import           System.FilePath

type FileMap = Map.Map Uri Uri

emptyFileMap :: FileMap
emptyFileMap = Map.empty

buildFileMap :: [Uri] -> FilePath -> FilePath -> FileMap -> IO FileMap
buildFileMap uris recBaseDir curBaseDir oldMap =
  foldM (createFile recBaseDir curBaseDir) oldMap uris
  where
  createFile baseDir curDir  map uri =
    if Map.member uri map
      then return map
      else do
        let fp = fromMaybe (error "Couldn't convert file path")
                 (uriToFilePath uri)
            relativeFp = makeRelative baseDir fp
            actualFp = curDir </> relativeFp

        -- Need to store in a directory inside tmp directory
        -- otherwise ghc-mod ends up creating one for us
        tmpDir <- (</> "lsp-test" </> takeDirectory relativeFp) <$> getTemporaryDirectory
        createDirectoryIfMissing True tmpDir

        (tmpFp, tmpH) <- openTempFile tmpDir (takeFileName actualFp)

        readFile actualFp >>= hPutStr tmpH
        tmpUri <- filePathToUri <$> canonicalizePath tmpFp
        return $ Map.insert uri tmpUri map

cleanupFiles :: IO ()
cleanupFiles = removeDirectoryRecursive =<< (</> "lsp-test") <$> getTemporaryDirectory

swapFiles :: FileMap -> FilePath -> FilePath -> [B.ByteString] -> IO ([B.ByteString], FileMap)
swapFiles fileMap recBaseDir curBaseDir msgs = do

  let oldUris = Set.unions $ map extractUris msgs

  newMap <- buildFileMap (Set.elems oldUris) recBaseDir curBaseDir fileMap

  let newMsgs = map (swapUris newMap) msgs

  case decode (head newMsgs) :: Maybe InitializeRequest of
    -- If there is an initialize request we will need to swap
    -- the rootUri and rootPath
    Just req -> do
      cd <- getCurrentDirectory
      let newRoot = cd </> curBaseDir
          newRootUri = params . rootUri .~ Just (filePathToUri newRoot) $ req
          newRootPath = params . rootPath .~ Just (T.pack newRoot) $ newRootUri
          newReq = encode newRootPath
      return (newReq:(tail newMsgs), newMap)
      
    Nothing -> return (newMsgs, newMap)

rootDir :: [B.ByteString] -> FilePath
rootDir msgs = case decode (head msgs) :: Maybe InitializeRequest of
                Just req -> fromMaybe (error "Couldn't convert root dir") $ do
                  rootUri <- req ^. params . rootUri
                  uriToFilePath rootUri
                Nothing -> error "Couldn't find root dir"

extractUris :: B.ByteString -> Set.Set Uri
extractUris msgs =
  case decode msgs :: Maybe Object of
    Just obj -> HashMap.foldlWithKey' gather Set.empty obj
    Nothing -> error "Couldn't decode message"
  where gather :: Set.Set Uri -> T.Text -> Value -> Set.Set Uri
        gather uris "uri" (String s) = Set.insert (Uri s) uris
        gather uris _ (Object o) = HashMap.foldlWithKey' gather uris o
        gather uris _ _ = uris

swapUris :: FileMap -> B.ByteString -> B.ByteString
swapUris fileMap msg =
  case decode msg :: Maybe Object of
    Just obj -> encode $ HashMap.mapWithKey f obj
    Nothing -> error "Couldn't decode message"

  where f :: T.Text -> Value -> Value
        f "uri" (String uri) = String $ swap uri
        f "changes" (Object obj) = Object $
          HashMap.foldlWithKey' (\acc k v -> HashMap.insert (swap k) v acc)
                                HashMap.empty
                                obj
        f _ x = g x

        g :: Value -> Value
        g (Array arr) = Array $ fmap g arr
        g (Object obj) = Object $ HashMap.mapWithKey f obj
        g x = x

        swap origUri = let (Uri newUri) = fileMap ! Uri origUri in newUri
