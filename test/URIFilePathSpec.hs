{-# LANGUAGE OverloadedStrings #-}
module URIFilePathSpec where

import Control.Monad                          (when)
import Data.List
import Data.Text                              (Text, pack)
import Language.LSP.Types

import Network.URI
import Test.Hspec
import Test.QuickCheck
import qualified System.FilePath.Windows as FPW
import System.FilePath                        (normalise)
import qualified System.Info
-- ---------------------------------------------------------------------

isWindows :: Bool
isWindows = System.Info.os == "mingw32"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Platform aware URI file path functions" platformAwareUriFilePathSpec
  describe "URI file path functions" uriFilePathSpec
  describe "URI normalization functions" uriNormalizeSpec
  describe "Normalized file path functions" normalizedFilePathSpec

windowsOS :: String
windowsOS = "mingw32"

testPosixUri :: Uri
testPosixUri = Uri $ pack "file:///home/myself/example.hs"

testPosixFilePath :: FilePath
testPosixFilePath = "/home/myself/example.hs"

relativePosixFilePath :: FilePath
relativePosixFilePath = "myself/example.hs"

testWindowsUri :: Uri
testWindowsUri = Uri $ pack "file:///c:/Users/myself/example.hs"

testWindowsFilePath :: FilePath
testWindowsFilePath = "c:\\Users\\myself\\example.hs"

platformAwareUriFilePathSpec :: Spec
platformAwareUriFilePathSpec = do
  it "converts a URI to a POSIX file path" $ do
    let theFilePath = platformAwareUriToFilePath "posix" testPosixUri
    theFilePath `shouldBe` Just testPosixFilePath

  it "converts a POSIX file path to a URI" $ do
    let theUri = platformAwareFilePathToUri "posix" testPosixFilePath
    theUri `shouldBe` testPosixUri

  it "converts a URI to a Windows file path" $ do
    let theFilePath = platformAwareUriToFilePath windowsOS testWindowsUri
    theFilePath `shouldBe` Just testWindowsFilePath

  it "converts a Windows file path to a URI" $ do
    let theUri = platformAwareFilePathToUri windowsOS testWindowsFilePath
    theUri `shouldBe` testWindowsUri

  it "converts a POSIX file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri "posix" "./Functional.hs"
    theFilePath `shouldBe` (Uri "file://./Functional.hs")

  it "converts a Windows file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri windowsOS "./Functional.hs"
    theFilePath `shouldBe` (Uri "file:///./Functional.hs")

  it "converts a Windows file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri windowsOS "c:/Functional.hs"
    theFilePath `shouldBe` (Uri "file:///c:/Functional.hs")

  it "converts a POSIX file path to a URI and back" $ do
    let theFilePath = platformAwareFilePathToUri "posix" "./Functional.hs"
    theFilePath `shouldBe` (Uri "file://./Functional.hs")
    let Just (URI scheme' auth' path' query' frag') =  parseURI "file://./Functional.hs"
    (scheme',auth',path',query',frag') `shouldBe`
      ("file:"
      ,Just (URIAuth {uriUserInfo = "", uriRegName = ".", uriPort = ""}) -- AZ: Seems odd
      ,"/Functional.hs"
      ,""
      ,"")
    Just "./Functional.hs" `shouldBe` platformAwareUriToFilePath "posix" theFilePath

  it "converts a Posix file path to a URI and back" $ property $ forAll genPosixFilePath $ \fp -> do
      let uri = platformAwareFilePathToUri "posix" fp
      platformAwareUriToFilePath "posix" uri `shouldBe` Just fp

  it "converts a Windows file path to a URI and back" $ property $ forAll genWindowsFilePath $ \fp -> do
      let uri = platformAwareFilePathToUri windowsOS fp
      -- We normalise to account for changes in the path separator.
      -- But driver letters are *not* normalized so we skip them
      when (not $ "c:" `isPrefixOf` fp) $
        platformAwareUriToFilePath windowsOS uri `shouldBe` Just (FPW.normalise fp)

  it "converts a relative POSIX file path to a URI and back" $ do
    let uri = platformAwareFilePathToUri "posix" relativePosixFilePath
    uri `shouldBe` Uri "file://myself/example.hs"
    let back = platformAwareUriToFilePath "posix" uri
    back `shouldBe` Just relativePosixFilePath


testUri :: Uri
testUri | isWindows = Uri "file:///C:/Users/myself/example.hs"
        | otherwise = Uri "file:///home/myself/example.hs"

testFilePath :: FilePath
testFilePath | isWindows = "C:\\Users\\myself\\example.hs"
             | otherwise = "/home/myself/example.hs"

withCurrentDirFilePath :: FilePath
withCurrentDirFilePath | isWindows = "C:\\Users\\.\\myself\\.\\.\\example.hs"
                       | otherwise = "/home/./myself/././example.hs"

fromRelativefilePathUri :: Uri
fromRelativefilePathUri | isWindows = Uri  "file:///myself/example.hs"
                        | otherwise = Uri "file://myself/example.hs"

relativeFilePath :: FilePath
relativeFilePath | isWindows = "myself\\example.hs"
                 | otherwise = "myself/example.hs"

withLowerCaseDriveLetterFilePath :: FilePath
withLowerCaseDriveLetterFilePath = "c:\\Users\\.\\myself\\.\\.\\example.hs"

withInitialCurrentDirUriStr :: String
withInitialCurrentDirUriStr | isWindows = "file:///Functional.hs"
                            | otherwise = "file://Functional.hs"

withInitialCurrentDirUriParts :: (String, Maybe URIAuth,  String, String, String)
withInitialCurrentDirUriParts
  | isWindows =
    ("file:"
    ,Just (URIAuth {uriUserInfo = "", uriRegName = "", uriPort = ""}) -- JNS: And asymmetrical
    ,"/Functional.hs","","")
  | otherwise =
     ("file:"
    ,Just (URIAuth {uriUserInfo = "", uriRegName = "Functional.hs", uriPort = ""}) -- AZ: Seems odd
    ,"","","")

withInitialCurrentDirFilePath :: FilePath
withInitialCurrentDirFilePath | isWindows = ".\\Functional.hs"
                              | otherwise = "./Functional.hs"

noNormalizedUriTxt :: Text
noNormalizedUriTxt | isWindows = "file:///c:/Users/./myself/././example.hs"
                   | otherwise = "file:///home/./myself/././example.hs"

noNormalizedUri :: Uri
noNormalizedUri = Uri noNormalizedUriTxt

uriFilePathSpec :: Spec
uriFilePathSpec = do
  it "converts a URI to a file path" $ do
    let theFilePath = uriToFilePath testUri
    theFilePath `shouldBe` Just testFilePath

  it "converts a file path to a URI" $ do
    let theUri = filePathToUri testFilePath
    theUri `shouldBe` testUri

  it "removes unnecesary current directory paths" $ do
    let theUri = filePathToUri withCurrentDirFilePath
    theUri `shouldBe` testUri

  when isWindows $
    it "make the drive letter upper case when converting a Windows file path to a URI" $ do
      let theUri = filePathToUri withLowerCaseDriveLetterFilePath
      theUri `shouldBe` testUri

  it "converts a file path to a URI and back" $ property $ forAll genFilePath $ \fp -> do
      let uri = filePathToUri fp
      uriToFilePath uri `shouldBe` Just (normalise fp)

  it "converts a relative file path to a URI and back" $ do
    let uri = filePathToUri relativeFilePath
    uri `shouldBe` fromRelativefilePathUri
    let back = uriToFilePath uri
    back `shouldBe` Just relativeFilePath

  it "converts a file path with initial current dir to a URI and back" $ do
    let uri = filePathToUri withInitialCurrentDirFilePath
    uri `shouldBe` (Uri (pack withInitialCurrentDirUriStr))
    let Just (URI scheme' auth' path' query' frag') =  parseURI withInitialCurrentDirUriStr
    (scheme',auth',path',query',frag') `shouldBe` withInitialCurrentDirUriParts
    Just "Functional.hs" `shouldBe` uriToFilePath uri

uriNormalizeSpec :: Spec
uriNormalizeSpec = do

  it "ignores differences in percent-encoding" $ property $ \uri ->
    toNormalizedUri (Uri $ pack $ escapeURIString isUnescapedInURI uri) `shouldBe`
        toNormalizedUri (Uri $ pack $ escapeURIString (const False) uri)

  it "ignores differences in percent-encoding (examples)" $ do
    toNormalizedUri (Uri $ pack "http://server/path%C3%B1?param=%C3%B1") `shouldBe`
        toNormalizedUri (Uri $ pack "http://server/path%c3%b1?param=%c3%b1")
    toNormalizedUri (Uri $ pack "file:///path%2A") `shouldBe`
        toNormalizedUri (Uri $ pack "file:///path%2a")

  it "normalizes uri file path when converting from uri to normalized uri" $ do
    let (NormalizedUri _ uri) = toNormalizedUri noNormalizedUri
    let (Uri nuri) = testUri
    uri `shouldBe` nuri

  it "converts a file path with reserved uri chars to a normalized URI and back" $ do
    let start = if isWindows then "C:\\" else "/"
    let fp = start ++ "path;part#fragmen?param=val"
    let nuri = toNormalizedUri (filePathToUri fp)
    uriToFilePath (fromNormalizedUri nuri) `shouldBe` Just fp

  it "converts a file path with substrings that looks like uri escaped chars and back" $ do
    let start = if isWindows then "C:\\" else "/"
    let fp = start ++ "ca%C3%B1a"
    let nuri = toNormalizedUri (filePathToUri fp)
    uriToFilePath (fromNormalizedUri nuri) `shouldBe` Just fp

  it "converts a file path to a normalized URI and back" $ property $ forAll genFilePath $ \fp -> do
    let nuri = toNormalizedUri (filePathToUri fp)
    case uriToFilePath (fromNormalizedUri nuri) of
      Just nfp -> nfp `shouldBe` (normalise fp)
      Nothing -> return () -- Some unicode paths creates invalid uris, ignoring for now

genFilePath :: Gen FilePath
genFilePath | isWindows = genWindowsFilePath
            | otherwise = genPosixFilePath

genWindowsFilePath :: Gen FilePath
genWindowsFilePath = do
    segments <- listOf1 pathSegment
    pathSep <- elements ['/', '\\']
    driveLetter <- elements ["C:", "c:"]
    pure (driveLetter <> [pathSep] <> intercalate [pathSep] segments)
  where pathSegment = listOf1 (genValidUnicodeChar `suchThat` (`notElem` ['/', '\\', ':']))

genPosixFilePath :: Gen FilePath
genPosixFilePath = do
    segments <- listOf1 pathSegment
    pure ("/" <> intercalate "/" segments)
  where pathSegment = listOf1 (genValidUnicodeChar `suchThat` (`notElem` ['/']))

genValidUnicodeChar :: Gen Char
genValidUnicodeChar = arbitraryUnicodeChar `suchThat` isCharacter
  where isCharacter x = x /= '\65534' && x /= '\65535'

normalizedFilePathSpec :: Spec
normalizedFilePathSpec = do
  it "makes file path normalized" $ property $ forAll genFilePath $ \fp -> do
    let nfp = toNormalizedFilePath fp
    fromNormalizedFilePath nfp `shouldBe` (normalise fp)

  it "converts to a normalized uri and back" $ property $ forAll genFilePath $ \fp -> do
    let nuri = normalizedFilePathToUri (toNormalizedFilePath fp)
    case uriToNormalizedFilePath nuri of
      Just nfp -> fromNormalizedFilePath nfp `shouldBe` (normalise fp)
      Nothing -> return () -- Some unicode paths creates invalid uris, ignoring for now

  it "converts a file path with reserved uri chars to a normalized URI and back" $ do
    let start = if isWindows then "C:\\" else "/"
    let fp = start ++ "path;part#fragmen?param=val"
    let nuri = normalizedFilePathToUri (toNormalizedFilePath fp)
    fmap fromNormalizedFilePath (uriToNormalizedFilePath nuri) `shouldBe` Just fp

  it "converts a file path with substrings that looks like uri escaped chars and back" $ do
    let start = if isWindows then "C:\\" else "/"
    let fp = start ++ "ca%C3%B1a"
    let nuri = normalizedFilePathToUri (toNormalizedFilePath fp)
    fmap fromNormalizedFilePath (uriToNormalizedFilePath nuri) `shouldBe` Just fp

  it "creates the same NormalizedUri than the older implementation" $ property $ forAll genFilePath $ \fp -> do
    let nuri = normalizedFilePathToUri (toNormalizedFilePath fp)
    let oldNuri = toNormalizedUri (filePathToUri fp)
    nuri `shouldBe` oldNuri
