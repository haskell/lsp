{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Files
  ( swapFiles
  , rootDir
  )
where

import           Language.Haskell.LSP.Types        hiding ( error )
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe
import           System.Directory
import           System.FilePath

swapFiles :: FilePath -> FilePath -> [B.ByteString] -> IO [B.ByteString]
swapFiles recBaseDir relCurBaseDir msgs = do
  curBaseDir <- (</> relCurBaseDir) <$> getCurrentDirectory
  let transform uri =
        let fp = fromMaybe (error "Couldn't transform uri") (uriToFilePath uri)
            newFp = curBaseDir </> makeRelative recBaseDir fp
          in filePathToUri newFp
      newMsgs = map (mapUris transform) msgs :: [B.ByteString]

  return newMsgs

rootDir :: [B.ByteString] -> FilePath
rootDir msgs = fromMaybe (error "Couldn't find root dir") $ do
  req <- decode (head msgs) :: Maybe InitializeRequest
  rootUri <- req ^. params .rootUri
  uriToFilePath rootUri

mapUris :: (Uri -> Uri) -> B.ByteString -> B.ByteString
mapUris f msg =
  case decode msg :: Maybe Object of
    Just obj -> encode $ HashMap.map (mapValue f) obj
    Nothing -> error "Couldn't decode message"

  where 
    mapValue :: (Uri -> Uri) -> Value -> Value
    mapValue f x = case parse parseJSON x :: Result VersionedTextDocumentIdentifier of
      Success doc -> transform doc
      Error _ -> case parse parseJSON x :: Result TextDocumentIdentifier of
        Success doc -> transform doc
        Error _ -> case parse parseJSON x :: Result InitializeParams of
          Success params -> transformInit params
          Error _ -> case parse parseJSON x :: Result Object of
            Success obj -> Object $ HashMap.map (mapValue f) obj
            Error _ -> x

    -- parsing with just JSON
    -- mapValueWithKey :: (Uri -> Uri) -> T.Text -> Value -> Value
    -- mapValueWithKey f "uri" (String s) = fromMaybe (error "Couldn't convert uri") $ do
    --   let uri = filePathToUri $ T.unpack s
    --   String <$> (fmap T.pack (uriToFilePath $ f uri))
    -- mapValueWithKey f _ (Array xs) = Array $ fmap (mapValue f) xs
    -- mapValueWithKey f _ (Object x) = Object $ HashMap.mapWithKey (mapValueWithKey f) x

    transform x = toJSON $ x & uri .~ f (x ^. uri)

    -- transform rootUri/rootPath
    transformInit :: InitializeParams -> Value
    transformInit x =
      let newRootUri = fmap f (x ^. rootUri)
          newRootPath = do
            fp <- T.unpack <$> x ^. rootPath
            let uri = filePathToUri fp
            T.pack <$> uriToFilePath (f uri)
        in toJSON $ (rootUri .~ newRootUri) $ (rootPath .~ newRootPath) x
