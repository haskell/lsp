{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.LSP.Types.Uri
  ( Uri(..)
  , uriToFilePath
  , filePathToUri
  , NormalizedUri(..)
  , toNormalizedUri
  , fromNormalizedUri
  , NormalizedFilePath
  , toNormalizedFilePath
  , fromNormalizedFilePath
  , normalizedFilePathToUri
  , uriToNormalizedFilePath
  , emptyNormalizedFilePath
  -- Private functions
  , platformAwareUriToFilePath
  , platformAwareFilePathToUri
  )
  where

import qualified Codec.Binary.UTF8.String as UTF8
import           Control.DeepSeq
import qualified Data.Aeson               as A
import           Data.Binary              (Binary, Get, get, put)
import           Data.ByteString.Short    (ShortByteString)
import qualified Data.ByteString.Short    as BS
import           Data.Hashable
import           Data.List                (stripPrefix)
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics
import           Network.URI              hiding (authority)
import           Safe                     (tailMay)
import qualified System.FilePath          as FP
import qualified System.FilePath.Posix    as FPP
import qualified System.FilePath.Windows  as FPW
import qualified System.Info


newtype Uri = Uri { getUri :: Text }
  deriving (Eq,Ord,Read,Show,Generic,A.FromJSON,A.ToJSON,Hashable,A.ToJSONKey,A.FromJSONKey)

instance NFData Uri

-- If you care about performance then you should use a hash map. The keys
-- are cached in order to make hashing very fast.
data NormalizedUri = NormalizedUri !Int !Text
  deriving (Read,Show,Generic, Eq)

-- Slow but compares paths alphabetically as you would expect.
instance Ord NormalizedUri where
  compare (NormalizedUri _ u1) (NormalizedUri _ u2) = compare u1 u2

instance Hashable NormalizedUri where
  hash (NormalizedUri h _) = h
  hashWithSalt salt (NormalizedUri h _) = hashWithSalt salt h

instance NFData NormalizedUri

isUnescapedInUriPath :: SystemOS -> Char -> Bool
isUnescapedInUriPath systemOS c
   | systemOS == windowsOS = isUnreserved c || c `elem` [':', '\\', '/']
   | otherwise = isUnreserved c || c == '/'

-- | When URIs are supposed to be used as keys, it is important to normalize
-- the percent encoding in the URI since URIs that only differ
-- when it comes to the percent-encoding should be treated as equivalent.
normalizeUriEscaping :: String -> String
normalizeUriEscaping uri =
  case stripPrefix (fileScheme ++ "//") uri of
    Just p  -> fileScheme ++ "//" ++ escapeURIPath (unEscapeString p)
    Nothing -> escapeURIString isUnescapedInURI $ unEscapeString uri
  where escapeURIPath = escapeURIString (isUnescapedInUriPath System.Info.os)

toNormalizedUri :: Uri -> NormalizedUri
toNormalizedUri uri = NormalizedUri (hash norm) norm
  where (Uri t) = maybe uri filePathToUri (uriToFilePath uri)
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
    then return $
      platformAdjustFromUriPath systemOS (uriRegName <$> uriAuthority) $ unEscapeString uriPath
    else Nothing

-- | We pull in the authority because in relative file paths the Uri likes to put everything before the slash
--   into the authority field
platformAdjustFromUriPath :: SystemOS
                          -> Maybe String -- ^ authority
                          -> String -- ^ path
                          -> FilePath
platformAdjustFromUriPath systemOS authority srcPath =
  maybe id (++) authority $
  if systemOS /= windowsOS
  then srcPath
  else case FPP.splitDirectories <$> tailMay srcPath of
      Just (firstSegment:rest) -> -- Drop leading '/' for absolute Windows paths
        let drive = if FPW.isDrive firstSegment
                    then FPW.addTrailingPathSeparator firstSegment
                    else firstSegment
         in FPW.joinDrive drive $ FPW.joinPath rest
      _ -> srcPath

filePathToUri :: FilePath -> Uri
filePathToUri = platformAwareFilePathToUri System.Info.os . FP.normalise

{-# WARNING platformAwareFilePathToUri "This function is considered private. Use normalizedUriToFilePath instead." #-}
platformAwareFilePathToUri :: SystemOS -> FilePath -> Uri
platformAwareFilePathToUri systemOS fp = Uri . T.pack . show $ URI
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
                convertDrive drv `FPP.joinDrive`
                FPP.joinPath (map (escapeURIString (isUnescapedInUriPath systemOS)) $ splitDirectories rest)
    -- splitDirectories does not remove the path separator after the drive so
    -- we do a final replacement of \ to /
    convertDrive drv
      | systemOS == windowsOS && FPW.hasTrailingPathSeparator drv =
          FPP.addTrailingPathSeparator (init drv)
      | otherwise = drv

-- | A file path that is already normalized. It is stored as an UTF-8 encoded 'ShortByteString'
--
-- The 'NormalizedUri' is cached to avoided
-- repeated normalisation when we need to compute them (which is a lot).
--
-- This is one of the most performance critical parts of ghcide, do not
-- modify it without profiling.
data NormalizedFilePath = NormalizedFilePath !NormalizedUri {-# UNPACK #-} !ShortByteString
    deriving (Generic, Eq, Ord)

instance NFData NormalizedFilePath

instance Binary NormalizedFilePath where
  put (NormalizedFilePath _ fp) = put fp
  get = do
    v <- Data.Binary.get :: Get ShortByteString
    let v' = decodeFilePath v
    return (NormalizedFilePath (internalNormalizedFilePathToUri v') v)

-- | Convert 'FilePath' to a UTF-8 encoded 'ShortByteString'
encodeFilePath :: FilePath -> ShortByteString
encodeFilePath = BS.pack . UTF8.encode

-- | Assume the given 'ShortByteString' is UTF-8 encoded, decode it into a 'FilePath'
decodeFilePath :: ShortByteString -> FilePath
decodeFilePath = UTF8.decode . BS.unpack

-- | Internal helper that takes a file path that is assumed to
-- already be normalized to a URI. It is up to the caller
-- to ensure normalization.
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
toNormalizedFilePath fp = NormalizedFilePath nuri . encodeFilePath $ nfp
  where
    nfp = FP.normalise fp
    nuri = internalNormalizedFilePathToUri nfp

-- | Extracts 'FilePath' from 'NormalizedFilePath'.
fromNormalizedFilePath :: NormalizedFilePath -> FilePath
fromNormalizedFilePath (NormalizedFilePath _ fp) = decodeFilePath fp

normalizedFilePathToUri :: NormalizedFilePath -> NormalizedUri
normalizedFilePathToUri (NormalizedFilePath uri _) = uri

uriToNormalizedFilePath :: NormalizedUri -> Maybe NormalizedFilePath
uriToNormalizedFilePath nuri = fmap (NormalizedFilePath nuri . encodeFilePath) mbFilePath
  where mbFilePath = platformAwareUriToFilePath System.Info.os (fromNormalizedUri nuri)

emptyNormalizedUri :: NormalizedUri
emptyNormalizedUri =
    let s = "file://"
    in NormalizedUri (hash s) s

-- | 'NormalizedFilePath' that contains an empty file path
emptyNormalizedFilePath :: NormalizedFilePath
emptyNormalizedFilePath = NormalizedFilePath emptyNormalizedUri ""

