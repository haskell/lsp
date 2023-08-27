{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Language.LSP.Protocol.Types qualified as J
import Test.Hspec

spec :: Spec
spec = do
  describe "MarkupContent" $ do
    it "appends two plainstrings" $ do
      J.mkPlainText "string1\n" <> J.mkPlainText "string2\n"
        `shouldBe` J.mkPlainText "string1\nstring2\n"
    it "appends a marked up and a plain string" $ do
      J.mkMarkdownCodeBlock "haskell" "foo :: Int" <> J.mkPlainText "string2\nstring3\n"
        `shouldBe` J.MarkupContent J.MarkupKind_Markdown "\n```haskell\nfoo :: Int\n```\nstring2  \nstring3  \n"
    it "appends a plain string and a marked up string" $ do
      J.mkPlainText "string2\n" <> J.mkMarkdownCodeBlock "haskell" "foo :: Int"
        `shouldBe` J.MarkupContent J.MarkupKind_Markdown "string2  \n\n```haskell\nfoo :: Int\n```\n"
