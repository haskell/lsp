module URIFilePathSpec where

import Data.Text                              (pack)
import Language.Haskell.LSP.Types

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "URI file path functions" uriFilePathSpec

testPosixUri :: Uri
testPosixUri = Uri $ pack "file:///home/myself/example.hs"

testPosixFilePath :: FilePath
testPosixFilePath = "/home/myself/example.hs"

testWindowsUri :: Uri
testWindowsUri = Uri $ pack "file:///c%3A/Users/myself/example.hs"

testWindowsFilePath :: FilePath
testWindowsFilePath = "c:\\Users\\myself\\example.hs"

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
