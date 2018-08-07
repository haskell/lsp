{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.LSP.Types.Uri where

import qualified Data.Aeson                                 as A
import           Data.Hashable
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Network.URI
import qualified System.FilePath.Posix                      as FPP
import qualified System.FilePath.Windows                    as FPW
import qualified System.Info

newtype Uri = Uri { getUri :: Text }
  deriving (Eq,Ord,Read,Show,A.FromJSON,A.ToJSON,Hashable,A.ToJSONKey,A.FromJSONKey)

fileScheme :: String
fileScheme = "file:"

windowsOS :: String
windowsOS = "mingw32"

type SystemOS = String

uriToFilePath :: Uri -> Maybe FilePath
uriToFilePath = platformAwareUriToFilePath System.Info.os

platformAwareUriToFilePath :: String -> Uri -> Maybe FilePath
platformAwareUriToFilePath systemOS (Uri uri) = do
  parsedUri <- parseURI $ T.unpack uri
  if uriScheme parsedUri == fileScheme
    then return $ (platformAdjustFromUriPath systemOS . unEscapeString . uriPath) parsedUri
    else Nothing

platformAdjustFromUriPath :: SystemOS -> String -> FilePath
platformAdjustFromUriPath systemOS srcPath =
  if systemOS /= windowsOS || null srcPath then srcPath
    else let
      firstSegment:rest = (FPP.splitDirectories . tail) srcPath  -- Drop leading '/' for absolute Windows paths
      drive = if FPW.isDrive firstSegment then FPW.addTrailingPathSeparator firstSegment else firstSegment
      in FPW.joinDrive drive $ FPW.joinPath rest

filePathToUri :: FilePath -> Uri
filePathToUri = platformAwareFilePathToUri System.Info.os

platformAwareFilePathToUri :: SystemOS -> FilePath -> Uri
platformAwareFilePathToUri systemOS fp = Uri . T.pack . show $ URI
  { uriScheme = fileScheme
  , uriAuthority = Just $ URIAuth "" "" ""
  , uriPath = platformAdjustToUriPath systemOS fp
  , uriQuery = ""
  , uriFragment = ""
  }

platformAdjustToUriPath :: SystemOS -> FilePath -> String
platformAdjustToUriPath systemOS srcPath =
  if systemOS /= windowsOS then srcPath
    else let
      drive:rest = FPW.splitDirectories srcPath
      leaveCharUnescaped = (/= ':')
      removePathSeparator = filter (not . FPW.isPathSeparator)
      escapedDrive = removePathSeparator $ escapeURIString leaveCharUnescaped drive
      in '/' : FPP.joinPath (escapedDrive : rest)
