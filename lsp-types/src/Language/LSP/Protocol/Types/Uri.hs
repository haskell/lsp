{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  emptyNormalizedUri,
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
import Data.Text.From (fromText)
import GHC.Generics
import GHC.IO.Encoding (getFileSystemEncoding)
import Network.URI hiding (authority)
import Prettyprinter
import Safe (tailMay)
import System.FilePath.Posix qualified as FPP
import System.FilePath.Windows qualified as FPW
import System.IO (TextEncoding, utf16le)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified
import System.OsPath qualified as OSP

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

-- | Encoding of the underlying filesystem.
systemEnc :: TextEncoding
systemEnc = unsafePerformIO getFileSystemEncoding
{-# NOINLINE systemEnc #-}

{- | Encode a 'FilePath' into an 'OsPath' using the system encoding.
Imprecise-exception (error) on pathological encodings.
-}
pathToOsPath :: FilePath -> OSP.OsPath
pathToOsPath = either (error . show) id . OSP.encodeWith systemEnc utf16le

{- | Decode an 'OsPath' into a 'FilePath' using the system encoding.
Imprecise-exception (error) on pathological encodings.
-}
osPathToPath :: OSP.OsPath -> FilePath
osPathToPath = either (error . show) id . OSP.decodeWith systemEnc utf16le

uriToFilePath :: Uri -> Maybe OSP.OsPath
uriToFilePath = platformAwareUriToFilePath System.Info.os

{-# WARNING platformAwareUriToFilePath "This function is considered private. Use normalizedFilePathToUri instead." #-}
platformAwareUriToFilePath :: String -> Uri -> Maybe OSP.OsPath
platformAwareUriToFilePath systemOS uri =
  -- Total: unencodable paths yield Nothing rather than throwing, since this
  -- is reachable from peer-supplied URIs (e.g. via 'toNormalizedUri').
  uriToFilePathString systemOS uri >>= \fp ->
    either (const Nothing) Just (OSP.encodeWith systemEnc utf16le fp)

{- | Internal String-returning pipeline, shared by 'platformAwareUriToFilePath'
and 'uriToNormalizedFilePath'.
-}
uriToFilePathString :: String -> Uri -> Maybe FilePath
uriToFilePathString systemOS (Uri uri) = do
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

filePathToUri :: OSP.OsPath -> Uri
filePathToUri = platformAwareFilePathToUri System.Info.os . OSP.normalise

{-# WARNING platformAwareFilePathToUri "This function is considered private. Use normalizedUriToFilePath instead." #-}
platformAwareFilePathToUri :: SystemOS -> OSP.OsPath -> Uri
platformAwareFilePathToUri systemOS osp =
  Uri . T.pack . show $
    URI
      { uriScheme = fileScheme
      , uriAuthority = Just $ URIAuth "" "" ""
      , uriPath = platformAdjustToUriPath systemOS fp
      , uriQuery = ""
      , uriFragment = ""
      }
 where
  fp = osPathToPath osp

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

{- Note [OsPath adoption]
The public path-typed API of this package uses 'OsPath' (requiring @filepath >= 1.4.100@).
'NormalizedFilePath' still stores 'Text': it is one of the most performance-critical
types in HLS, and changing its storage is deferred pending benchmarks — see
[#453](https://github.com/haskell/lsp/pull/453), [#446](https://github.com/haskell/lsp/pull/446)
and [#608](https://github.com/haskell/lsp/issues/608).

Conversions between 'FilePath'/'OsPath' are strict, using the system encoding
('systemEnc') with @utf16le@ for the wide-character side of 'System.OsPath.encodeWith'/'decodeWith'.
'uriToFilePath' is total (unencodable paths yield 'Nothing'); the other pure
conversions may raise an imprecise exception on pathological encodings
(e.g. lone surrogates in a 'String' passed to 'fromString').

Note that on Windows hosts 'toNormalizedFilePath' now uses
'System.OsPath.normalise', so the stored 'Text' payload uses @\\@ separators
(previously @/@ on all platforms).
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
  fromString = toNormalizedFilePath . pathToOsPath

toNormalizedFilePath :: OSP.OsPath -> NormalizedFilePath
toNormalizedFilePath osp = NormalizedFilePath nuri . T.pack $ nfp
 where
  osp' = OSP.normalise osp
  nfp = osPathToPath osp'
  nuri = internalNormalizedFilePathToUri nfp

{- | Extracts 'OsPath' from 'NormalizedFilePath'.

Note that 'fromText' encodes UTF-8 (POSIX) or UTF-16 LE (Windows)
independently of the locale, unlike 'pathToOsPath'. It is therefore the
exact inverse of 'toNormalizedFilePath' only under a UTF-8 filesystem
encoding.
-}
fromNormalizedFilePath :: NormalizedFilePath -> OSP.OsPath
fromNormalizedFilePath (NormalizedFilePath _ fp) = fromText fp

normalizedFilePathToUri :: NormalizedFilePath -> NormalizedUri
normalizedFilePathToUri (NormalizedFilePath uri _) = uri

uriToNormalizedFilePath :: NormalizedUri -> Maybe NormalizedFilePath
uriToNormalizedFilePath nuri = fmap (NormalizedFilePath nuri . T.pack) mbFilePath
 where
  mbFilePath = uriToFilePathString System.Info.os (fromNormalizedUri nuri)

emptyNormalizedUri :: NormalizedUri
emptyNormalizedUri =
  let s = "file://"
   in NormalizedUri (hash s) s

-- | 'NormalizedFilePath' that contains an empty file path
emptyNormalizedFilePath :: NormalizedFilePath
emptyNormalizedFilePath = NormalizedFilePath emptyNormalizedUri ""
