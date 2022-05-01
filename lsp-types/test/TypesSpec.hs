{-# LANGUAGE OverloadedStrings #-}
module TypesSpec where

import qualified Language.LSP.Types as J
import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = diagnosticsSpec

-- ---------------------------------------------------------------------

diagnosticsSpec :: Spec
diagnosticsSpec = do
  describe "MarkupContent" $ do
    it "appends two plainstrings" $ do
      J.unmarkedUpContent "string1\n" <> J.unmarkedUpContent "string2\n"
        `shouldBe` J.unmarkedUpContent "string1\nstring2\n"
    it "appends a marked up and a plain string" $ do
      J.markedUpContent "haskell" "foo :: Int" <> J.unmarkedUpContent "string2\nstring3\n"
        `shouldBe` J.MarkupContent J.MarkupKind_Markdown "\n```haskell\nfoo :: Int\n```\nstring2  \nstring3  \n"
    it "appends a plain string and a marked up string" $ do
       J.unmarkedUpContent "string2\n" <> J.markedUpContent "haskell" "foo :: Int"
        `shouldBe` J.MarkupContent J.MarkupKind_Markdown "string2  \n\n```haskell\nfoo :: Int\n```\n"

-- ---------------------------------------------------------------------
