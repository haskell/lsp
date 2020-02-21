{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module URIFilePathSpec where

import Control.Monad (when)
import Data.List
#if __GLASGOW_HASKELL__ < 808
import Data.Monoid ((<>))
#endif
import Data.Text                              (Text, pack)
import Language.Haskell.LSP.Types

import           Network.URI
import Test.Hspec
import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,10,0)
import Data.Char                              (GeneralCategory(..), generalCategory)
#endif
import System.FilePath (normalise)
import qualified System.Info
-- ---------------------------------------------------------------------

isWindows :: Bool
isWindows = System.Info.os == "mingw32"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "URI file path functions" uriFilePathSpec
  describe "URI normalization functions" uriNormalizeSpec
  describe "Normalized file path functions" normalizedFilePathSpec

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

  it "normalizes uri file path when converting from uri to normalized uri" $ do
    let (NormalizedUri _ uri) = toNormalizedUri noNormalizedUri
    let (Uri nuri) = testUri
    uri `shouldBe` nuri

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

#if !MIN_VERSION_QuickCheck(2,10,0)
arbitraryUnicodeChar :: Gen Char
arbitraryUnicodeChar =
  arbitraryBoundedEnum `suchThat` (not . isSurrogate)
  where
    isSurrogate c = generalCategory c == Surrogate
#endif

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

  it "creates the same NormalizedUri than the older implementation" $ property $ forAll genFilePath $ \fp -> do
    let nuri = normalizedFilePathToUri (toNormalizedFilePath fp)
    let oldNuri = toNormalizedUri (filePathToUri fp)
    nuri `shouldBe` oldNuri
