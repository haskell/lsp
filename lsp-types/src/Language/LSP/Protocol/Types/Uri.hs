{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.LSP.Protocol.Types.Uri (
  Uri (..),
  uriToFilePath,
  filePathToUri,
  NormalizedUri (..),
  toNormalizedUri,
  fromNormalizedUri,
  NormalizedFilePath,
  toNormalizedFilePath,
  fromNormalizedFilePath,
  normalizedFilePathToUri,
  uriToNormalizedFilePath,
  emptyNormalizedFilePath,
  -- Private functions
  platformAwareUriToFilePath,
  platformAwareFilePathToUri,
)
where

import Control.DeepSeq
import Data.Aeson qualified as A
import Data.Binary (Binary, Get, get, put)
import Data.Hashable
import Data.List (stripPrefix)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Network.URI hiding (authority)
import Prettyprinter
import Safe (tailMay)
import System.FilePath qualified as FP
import System.FilePath.Posix qualified as FPP
import System.FilePath.Windows qualified as FPW
import System.Info qualified

-- | The @Uri@ type in the LSP specification.
newtype Uri = Uri {getUri :: Text}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (A.FromJSON, A.ToJSON, Hashable, A.ToJSONKey, A.FromJSONKey, Pretty)

instance NFData Uri

{- | A normalized 'Uri'.

If you want to use a URI as a map key, use this type. It is important to normalize
the percent encoding in the URI since URIs that only differ
when it comes to the percent-encoding should be treated as equivalent.

'NormalizedUri' has a cached hash in order to make it especially fast in a hash map.
-}
data NormalizedUri = NormalizedUri !Int !Text
  deriving stock (Read, Show, Generic, Eq)

-- Slow but compares paths alphabetically as you would expect.
instance Ord NormalizedUri where
  compare (NormalizedUri _ u1) (NormalizedUri _ u2) = compare u1 u2

instance Hashable NormalizedUri where
  hash (NormalizedUri h _) = h
  hashWithSalt salt (NormalizedUri h _) = hashWithSalt salt h

instance Pretty NormalizedUri where
  pretty (NormalizedUri _ t) = pretty t

instance NFData NormalizedUri

isUnescapedInUriPath :: SystemOS -> Char -> Bool
isUnescapedInUriPath systemOS c
  | systemOS == windowsOS = isUnreserved c || c `elem` [':', '\\', '/']
  | otherwise = isUnreserved c || c == '/'

normalizeUriEscaping :: String -> String
normalizeUriEscaping uri =
  case stripPrefix (fileScheme ++ "//") uri of
    Just p -> fileScheme ++ "//" ++ escapeURIPath (unEscapeString p)
    Nothing -> escapeURIString isUnescapedInURI $ unEscapeString uri
 where
  escapeURIPath = escapeURIString (isUnescapedInUriPath System.Info.os)

toNormalizedUri :: Uri -> NormalizedUri
toNormalizedUri uri = NormalizedUri (hash norm) norm
 where
  (Uri t) = maybe uri filePathToUri (uriToFilePath uri)
  -- To ensure all `Uri`s have the file path normalized
  norm = T.pack (normalizeUriEscaping (T.unpack t))

fromNormalizedUri :: NormalizedUri -> Uri
fromNormalizedUri (NormalizedUri _ t) = Uri t

fileScheme :: String
fileScheme = "file:"

windowsOS :: String
windowsOS = "mingw32"

type SystemOS = String

uriToFilePath :: Uri -> Maybe FilePath
uriToFilePath = platformAwareUriToFilePath System.Info.os

{-# WARNING platformAwareUriToFilePath "This function is considered private. Use normalizedFilePathToUri instead." #-}
platformAwareUriToFilePath :: String -> Uri -> Maybe FilePath
platformAwareUriToFilePath systemOS (Uri uri) = do
  URI{..} <- parseURI $ T.unpack uri
  if uriScheme == fileScheme
    then
      return $
        platformAdjustFromUriPath systemOS (uriRegName <$> uriAuthority) $
          unEscapeString uriPath
    else Nothing

{- | We pull in the authority because in relative file paths the Uri likes to put everything before the slash
   into the authority field
-}
platformAdjustFromUriPath ::
  SystemOS ->
  -- | authority
  Maybe String ->
  -- | path
  String ->
  FilePath
platformAdjustFromUriPath systemOS authority srcPath =
  maybe id (++) authority $
    if systemOS /= windowsOS
      then srcPath
      else case FPP.splitDirectories <$> tailMay srcPath of
        Just (firstSegment : rest) ->
          -- Drop leading '/' for absolute Windows paths
          let drive =
                if FPW.isDrive firstSegment
                  then FPW.addTrailingPathSeparator firstSegment
                  else firstSegment
           in FPW.joinDrive drive $ FPW.joinPath rest
        _ -> srcPath

filePathToUri :: FilePath -> Uri
filePathToUri = platformAwareFilePathToUri System.Info.os . FP.normalise

{-# WARNING platformAwareFilePathToUri "This function is considered private. Use normalizedUriToFilePath instead." #-}
platformAwareFilePathToUri :: SystemOS -> FilePath -> Uri
platformAwareFilePathToUri systemOS fp =
  Uri . T.pack . show $
    URI
      { uriScheme = fileScheme
      , uriAuthority = Just $ URIAuth "" "" ""
      , uriPath = platformAdjustToUriPath systemOS fp
      , uriQuery = ""
      , uriFragment = ""
      }

platformAdjustToUriPath :: SystemOS -> FilePath -> String
platformAdjustToUriPath systemOS srcPath
  | systemOS == windowsOS = '/' : escapedPath
  | otherwise = escapedPath
 where
  (splitDirectories, splitDrive)
    | systemOS == windowsOS =
        (FPW.splitDirectories, FPW.splitDrive)
    | otherwise =
        (FPP.splitDirectories, FPP.splitDrive)
  escapedPath =
    case splitDrive srcPath of
      (drv, rest) ->
        convertDrive drv
          `FPP.joinDrive` FPP.joinPath (map (escapeURIString (isUnescapedInUriPath systemOS)) $ splitDirectories rest)
  -- splitDirectories does not remove the path separator after the drive so
  -- we do a final replacement of \ to /
  convertDrive drv
    | systemOS == windowsOS && FPW.hasTrailingPathSeparator drv =
        FPP.addTrailingPathSeparator (init drv)
    | otherwise = drv

{- Note [Adoption Plan of OsPath]
Currently we store 'Text' in 'NormalizedFilePath'. We may change it to OsPath in the future if
the following steps are executed.

1. In the client codebase, use 'osPathToNormalizedFilePath' and 'normalizedFilePathToOsPath' instead of 'fromNormalizedFilePath'
  and 'toNormalizedFilePath'. For HLS, we could wait until GHC 9.6 becomes the oldest
  GHC we support, then change 'FilePath' to OsPath everywhere in the codebase.
2. Deprecate and remove 'fromNormalizedFilePath' and 'toNormalizedFilePath'.
3. Change 'Text' to OsPath and benchmark it to make sure performance doesn't go down. Don't forget to check Windows,
  as OsPath on Windows uses UTF-16, which may consume more memory.

See [#453](https://github.com/haskell/lsp/pull/453) and [#446](https://github.com/haskell/lsp/pull/446)
for more discussions on this topic.
-}

{- | A file path that is already normalized.

The 'NormalizedUri' is cached to avoided
repeated normalisation when we need to compute them (which is a lot).

This is one of the most performance critical parts of HLS, do not
modify it without profiling.
-}
data NormalizedFilePath = NormalizedFilePath !NormalizedUri {-# UNPACK #-} !Text
  deriving stock (Generic, Eq, Ord)

instance NFData NormalizedFilePath

instance Binary NormalizedFilePath where
  put (NormalizedFilePath _ fp) = put fp
  get = do
    v <- Data.Binary.get :: Get Text
    return (NormalizedFilePath (internalNormalizedFilePathToUri (T.unpack v)) v)

{- | Internal helper that takes a file path that is assumed to
 already be normalized to a URI. It is up to the caller
 to ensure normalization.
-}
internalNormalizedFilePathToUri :: FilePath -> NormalizedUri
internalNormalizedFilePathToUri fp = nuri
 where
  uriPath = platformAdjustToUriPath System.Info.os fp
  nuriStr = T.pack $ fileScheme <> "//" <> uriPath
  nuri = NormalizedUri (hash nuriStr) nuriStr

instance Show NormalizedFilePath where
  show (NormalizedFilePath _ fp) = "NormalizedFilePath " ++ show fp

instance Hashable NormalizedFilePath where
  hash (NormalizedFilePath uri _) = hash uri
  hashWithSalt salt (NormalizedFilePath uri _) = hashWithSalt salt uri

instance IsString NormalizedFilePath where
  fromString :: String -> NormalizedFilePath
  fromString = toNormalizedFilePath

toNormalizedFilePath :: FilePath -> NormalizedFilePath
toNormalizedFilePath fp = NormalizedFilePath nuri . T.pack $ nfp
 where
  nfp = FP.normalise fp
  nuri = internalNormalizedFilePathToUri nfp

-- | Extracts 'FilePath' from 'NormalizedFilePath'.
fromNormalizedFilePath :: NormalizedFilePath -> FilePath
fromNormalizedFilePath (NormalizedFilePath _ fp) = T.unpack fp

normalizedFilePathToUri :: NormalizedFilePath -> NormalizedUri
normalizedFilePathToUri (NormalizedFilePath uri _) = uri

uriToNormalizedFilePath :: NormalizedUri -> Maybe NormalizedFilePath
uriToNormalizedFilePath nuri = fmap (NormalizedFilePath nuri . T.pack) mbFilePath
 where
  mbFilePath = platformAwareUriToFilePath System.Info.os (fromNormalizedUri nuri)

emptyNormalizedUri :: NormalizedUri
emptyNormalizedUri =
  let s = "file://"
   in NormalizedUri (hash s) s

-- | 'NormalizedFilePath' that contains an empty file path
emptyNormalizedFilePath :: NormalizedFilePath
emptyNormalizedFilePath = NormalizedFilePath emptyNormalizedUri ""
