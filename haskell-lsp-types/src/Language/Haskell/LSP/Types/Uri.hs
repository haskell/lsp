{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.LSP.Types.Uri
  ( Uri(..)
  , uriToFilePath
  , filePathToUri
  , NormalizedUri(..)
  , toNormalizedUri
  , fromNormalizedUri
  , NormalizedFilePath(..)
  , toNormalizedFilePath
  , fromNormalizedFilePath
  , normalizedFilePathToUri
  , uriToNormalizedFilePath
  -- Private functions
  , platformAwareUriToFilePath
  , platformAwareFilePathToUri
  )
  where

import           Control.DeepSeq
import qualified Data.Aeson                                 as A
import           Data.Binary                                (Binary, Get, put, get)
import           Data.Hashable
import           Data.List                                  (stripPrefix)
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid                                ((<>))
#endif
import           Data.String                                (IsString, fromString)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           GHC.Generics
import           Network.URI hiding (authority)
import qualified System.FilePath                            as FP
import qualified System.FilePath.Posix                      as FPP
import qualified System.FilePath.Windows                    as FPW
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
    Just p -> fileScheme ++ "//" ++ (escapeURIPath $ unEscapeString p)
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
  (maybe id (++) authority) $
  if systemOS /= windowsOS || null srcPath then srcPath
    else let
      firstSegment:rest = (FPP.splitDirectories . tail) srcPath  -- Drop leading '/' for absolute Windows paths
      drive = if FPW.isDrive firstSegment
              then FPW.addTrailingPathSeparator firstSegment
              else firstSegment
      in FPW.joinDrive drive $ FPW.joinPath rest

filePathToUri :: FilePath -> Uri
filePathToUri = (platformAwareFilePathToUri System.Info.os) . FP.normalise

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
data NormalizedFilePath = NormalizedFilePath NormalizedUri !FilePath
    deriving (Generic, Eq, Ord)

instance NFData NormalizedFilePath

instance Binary NormalizedFilePath where
  put (NormalizedFilePath _ fp) = put fp
  get = do
    v <- Data.Binary.get :: Get FilePath
    return (toNormalizedFilePath v)

instance Show NormalizedFilePath where
  show (NormalizedFilePath _ fp) = "NormalizedFilePath " ++ show fp

instance Hashable NormalizedFilePath where
  hash (NormalizedFilePath uri _) = hash uri

instance IsString NormalizedFilePath where
    fromString = toNormalizedFilePath

toNormalizedFilePath :: FilePath -> NormalizedFilePath
toNormalizedFilePath fp = NormalizedFilePath nuri nfp
  where nfp = FP.normalise fp
        uriPath = platformAdjustToUriPath System.Info.os nfp
        nuriStr = T.pack $ fileScheme <> "//" <> uriPath
        nuri = NormalizedUri (hash nuriStr) nuriStr

fromNormalizedFilePath :: NormalizedFilePath -> FilePath
fromNormalizedFilePath (NormalizedFilePath _ fp) = fp

normalizedFilePathToUri :: NormalizedFilePath -> NormalizedUri
normalizedFilePathToUri (NormalizedFilePath uri _) = uri

uriToNormalizedFilePath :: NormalizedUri -> Maybe NormalizedFilePath
uriToNormalizedFilePath nuri = fmap (NormalizedFilePath nuri) mbFilePath
  where mbFilePath = platformAwareUriToFilePath System.Info.os (fromNormalizedUri nuri)
