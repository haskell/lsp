{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
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
  , osPathToNormalizedFilePath
  , osPathFromNormalizedFilePath
  -- Private functions
  , platformAwareUriToFilePath
  , platformAwareFilePathToUri
  )
  where

import           Control.DeepSeq
import           Control.Monad.Catch            (MonadThrow)
import qualified Data.Aeson                     as A
import           Data.Binary                    (Binary, Get, get, put)
import           Data.ByteString.Short          (ShortByteString)
import           Data.Hashable
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Generics
import           Network.URI                    hiding (authority)
import           Safe                           (tailMay)
import qualified System.FilePath                as FP
import qualified System.FilePath.Posix          as FPP
import qualified System.FilePath.Windows        as FPW
import qualified System.Info
import qualified System.OsPath                  as OsPath
import           System.OsPath                  (OsPath)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import           System.OsString.Internal.Types (OsString (..),
                                                 WindowsString (..))
#else
import           System.OsString.Internal.Types (OsString (..),
                                                 PosixString (..))
#endif

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

-- | Newtype wrapper around FilePath that always has normalized slashes.
-- The NormalizedUri and hash of the FilePath are cached to avoided
-- repeated normalisation when we need to compute them (which is a lot).
--
-- This is one of the most performance critical parts of ghcide, do not
-- modify it without profiling.
data NormalizedFilePath = NormalizedFilePath NormalizedUri !OsPath
    deriving (Generic, Eq, Ord)

instance NFData NormalizedFilePath

instance Binary NormalizedFilePath where
  put (NormalizedFilePath _ fp) = put (unwrapOsPath fp)
  get = do
    v <- Data.Binary.get :: Get ShortByteString
    let v' = makeOsPath v
        nuri = internalNormalizedFilePathToUri v'
    return (NormalizedFilePath (fromJust nuri) v')

-- Remove the CPP hack when OsPath gets 'Binary' instance.
-- See: https://gitlab.haskell.org/haskell/filepath/-/issues/122

unwrapOsPath :: OsPath -> ShortByteString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
unwrapOsPath = getWindowsString . getOsString
#else
unwrapOsPath = getPosixString . getOsString
#endif

makeOsPath :: ShortByteString -> OsPath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
makeOsPath = OsString . WindowsString
#else
makeOsPath = OsString . PosixString
#endif

-- | Internal helper that takes a file path that is assumed to
-- already be normalized to a URI. It is up to the caller
-- to ensure normalization.
internalNormalizedFilePathToUri :: MonadThrow m => OsPath -> m NormalizedUri
internalNormalizedFilePathToUri fp = nuri
  where
    uriPath = platformAdjustToUriPath System.Info.os <$> OsPath.decodeUtf fp
    nuriStr = fmap (T.pack . \p -> fileScheme <> "//" <> p) uriPath
    nuri = fmap (\nuriStr' -> NormalizedUri (hash nuriStr') nuriStr') nuriStr

instance Show NormalizedFilePath where
  show (NormalizedFilePath _ fp) = "NormalizedFilePath " ++ show fp

instance Hashable NormalizedFilePath where
  hash (NormalizedFilePath uri _) = hash uri
  hashWithSalt salt (NormalizedFilePath uri _) = hashWithSalt salt uri

toNormalizedFilePath :: MonadThrow m => FilePath -> m NormalizedFilePath
toNormalizedFilePath fp = OsPath.encodeUtf fp >>= osPathToNormalizedFilePath

fromNormalizedFilePath :: MonadThrow m => NormalizedFilePath -> m FilePath
fromNormalizedFilePath = OsPath.decodeUtf . osPathFromNormalizedFilePath

osPathToNormalizedFilePath :: MonadThrow m => OsPath -> m NormalizedFilePath
osPathToNormalizedFilePath fp = flip NormalizedFilePath nfp <$> nuri
  where
    nfp = OsPath.normalise fp
    nuri = internalNormalizedFilePathToUri nfp

osPathFromNormalizedFilePath :: NormalizedFilePath -> OsPath
osPathFromNormalizedFilePath (NormalizedFilePath _ osPath) = osPath

normalizedFilePathToUri :: NormalizedFilePath -> NormalizedUri
normalizedFilePathToUri (NormalizedFilePath uri _) = uri

uriToNormalizedFilePath :: NormalizedUri -> Maybe NormalizedFilePath
uriToNormalizedFilePath nuri = fmap (NormalizedFilePath nuri) (mbFilePath >>= OsPath.encodeUtf)
  where mbFilePath = platformAwareUriToFilePath System.Info.os (fromNormalizedUri nuri)
