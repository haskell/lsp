{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module URIFilePathSpec where

import Data.List
#if __GLASGOW_HASKELL__ < 808
import Data.Monoid ((<>))
#endif
import Data.Text                              (pack)
import Language.Haskell.LSP.Types

import           Network.URI
import qualified System.FilePath.Windows as FPW
import Test.Hspec
import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,10,0)
import Data.Char                              (GeneralCategory(..), generalCategory)
#endif

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "URI file path functions" uriFilePathSpec
  describe "file path URI functions" filePathUriSpec
  describe "URI normalization functions" uriNormalizeSpec

testPosixUri :: Uri
testPosixUri = Uri $ pack "file:///home/myself/example.hs"

testPosixFilePath :: FilePath
testPosixFilePath = "/home/myself/example.hs"

relativePosixFilePath :: FilePath
relativePosixFilePath = "myself/example.hs"

withCurrentDirPosixFilePath :: FilePath
withCurrentDirPosixFilePath = "/home/./myself/././example.hs"

testWindowsUri :: Uri
testWindowsUri = Uri $ pack "file:///C:/Users/myself/example.hs"

testWindowsFilePath :: FilePath
testWindowsFilePath = "C:\\Users\\myself\\example.hs"

testWindowsFilePathDriveLowerCase :: FilePath
testWindowsFilePathDriveLowerCase = "c:\\Users\\myself\\example.hs"

withCurrentDirWindowsFilePath :: FilePath
withCurrentDirWindowsFilePath = "C:\\Users\\.\\myself\\.\\.\\example.hs"

uriFilePathSpec :: Spec
uriFilePathSpec = do
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

  it "removes unnecesary current directory paths" $ do
    let theUri = platformAwareFilePathToUri "posix" withCurrentDirPosixFilePath
    theUri `shouldBe` testPosixUri

  it "removes unnecesary current directory paths in windows" $ do
    let theUri = platformAwareFilePathToUri windowsOS withCurrentDirWindowsFilePath
    theUri `shouldBe` testWindowsUri

  it "make the drive letter upper case when converting a Windows file path to a URI" $ do
    let theUri = platformAwareFilePathToUri windowsOS testWindowsFilePathDriveLowerCase
    theUri `shouldBe` testWindowsUri

filePathUriSpec :: Spec
filePathUriSpec = do
  it "converts a POSIX file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri "posix" "./Functional.hs"
    theFilePath `shouldBe` (Uri "file://Functional.hs")

  it "converts a Windows file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri windowsOS "./Functional.hs"
    theFilePath `shouldBe` (Uri "file:///Functional.hs")

  it "converts a Windows file path to a URI" $ do
    let theFilePath = platformAwareFilePathToUri windowsOS "c:/Functional.hs"
    theFilePath `shouldBe` (Uri "file:///C:/Functional.hs")

  it "converts a POSIX file path to a URI and back" $ do
    let theFilePath = platformAwareFilePathToUri "posix" "./Functional.hs"
    theFilePath `shouldBe` (Uri "file://Functional.hs")
    let Just (URI scheme' auth' path' query' frag') =  parseURI "file://Functional.hs"
    (scheme',auth',path',query',frag') `shouldBe`
      ("file:"
      ,Just (URIAuth {uriUserInfo = "", uriRegName = "Functional.hs", uriPort = ""}) -- AZ: Seems odd
      ,""
      ,""
      ,"")
    Just "Functional.hs" `shouldBe` platformAwareUriToFilePath "posix" theFilePath

  it "converts a Posix file path to a URI and back" $ property $ forAll genPosixFilePath $ \fp -> do
      let uri = platformAwareFilePathToUri "posix" fp
      platformAwareUriToFilePath "posix" uri `shouldBe` Just fp

  it "converts a Windows file path to a URI and back" $ property $ forAll genWindowsFilePath $ \fp -> do
      let uri = platformAwareFilePathToUri windowsOS fp
      -- We normalise to account for changes in the path separator.
      platformAwareUriToFilePath windowsOS uri `shouldBe` Just (FPW.normalise fp)

  it "converts a relative POSIX file path to a URI and back" $ do
    let uri = platformAwareFilePathToUri "posix" relativePosixFilePath
    uri `shouldBe` Uri "file://myself/example.hs"
    let back = platformAwareUriToFilePath "posix" uri
    back `shouldBe` Just relativePosixFilePath

uriNormalizeSpec :: Spec
uriNormalizeSpec = do
  it "ignores differences in percent-encoding" $ property $ \uri -> do
    toNormalizedUri (Uri $ pack $ escapeURIString isUnescapedInURI uri) `shouldBe`
        toNormalizedUri (Uri $ pack $ escapeURIString (const False) uri)

genWindowsFilePath :: Gen FilePath
genWindowsFilePath = do
    segments <- listOf pathSegment
    pathSep <- elements ['/', '\\']
    driveLetter <- elements ["C:", "c:"]
    pure (driveLetter <> [pathSep] <> intercalate [pathSep] segments)
  where pathSegment = listOf1 (genValidUnicodeChar `suchThat` (`notElem` ['/', '\\', ':']))

genPosixFilePath :: Gen FilePath
genPosixFilePath = do
    segments <- listOf pathSegment
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
