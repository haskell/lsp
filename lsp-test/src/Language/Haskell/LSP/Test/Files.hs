{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , FileMap
  , emptyFileMap
  )
where

import           Language.Haskell.LSP.Types        hiding ( error )
import           Language.Haskell.LSP.Test.Parsing
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

buildFileMap :: [Uri] -> FileMap -> IO FileMap
buildFileMap uris oldMap = foldM createFile oldMap uris
 where
  createFile map uri =
    if Map.member uri map
      then return map
      else do
        let fp = fromMaybe (error "Couldn't convert file path")
                 (uriToFilePath uri)

        -- Need to store in a directory inside tmp directory
        -- otherwise ghc-mod ends up creating one for us
        tmpDir <- (</> "lsp-test") <$> getTemporaryDirectory
        createDirectoryIfMissing False tmpDir

        (tmpFp, tmpH) <- openTempFile tmpDir (takeFileName fp)

        readFile fp >>= hPutStr tmpH
        tmpUri <- filePathToUri <$> canonicalizePath tmpFp
        return $ Map.insert uri tmpUri map

swapFiles :: FileMap -> Handle -> IO ([B.ByteString], FileMap)
swapFiles fileMap h = do
  msgs <- getAllMessages h

  let oldUris = Set.unions $ map extractUris msgs

  newMap <- buildFileMap (Set.elems oldUris) fileMap

  let newMsgs = map (swapUris newMap) msgs

  return (newMsgs, newMap)

extractUris :: B.ByteString -> Set.Set Uri
extractUris msgs =
  case decode msgs :: Maybe Object of
    Just obj -> HashMap.foldlWithKey' gather Set.empty obj
    Nothing -> error "nooo"
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
