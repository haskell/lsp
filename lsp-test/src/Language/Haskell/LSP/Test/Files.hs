{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , FileMap
  , emptyFileMap
  , rootDir
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

buildFileMap :: Set.Set Uri -> FilePath -> FilePath -> FileMap -> IO FileMap
buildFileMap uris oldBaseDir newBaseDir oldMap = foldM transform oldMap uris
  where
  transform map uri = do
    let fp = fromMaybe (error "Couldn't convert file path") $ uriToFilePath uri
        rel = makeRelative oldBaseDir fp
        newFp = newBaseDir </> rel
    newUri <- filePathToUri <$> canonicalizePath newFp
    return $ Map.insert uri newUri map

swapFiles :: FileMap -> FilePath -> FilePath -> [B.ByteString] -> IO ([B.ByteString], FileMap)
swapFiles fileMap recBaseDir curBaseDir msgs = do

  let oldUris = Set.unions $ map extractUris msgs

  newMap <- buildFileMap oldUris recBaseDir curBaseDir fileMap

  let newMsgs = map (swapUris newMap) msgs

  case decode (head newMsgs) :: Maybe InitializeRequest of
    -- If there is an initialize request we will need to swap
    -- the rootUri and rootPath
    Just req -> do
      cd <- getCurrentDirectory
      let newRoot = cd </> curBaseDir
          newRootUri = params . rootUri ?~ filePathToUri newRoot $ req
          newRootPath = params . rootPath ?~ T.pack newRoot $ newRootUri
          newReq = encode newRootPath
      return (newReq:tail newMsgs, newMap)

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

