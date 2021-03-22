{-# LANGUAGE OverloadedStrings #-}
module WorkspaceEditSpec where

import Test.Hspec
import Language.LSP.Types

spec :: Spec
spec = do
  describe "applyTextEdit" $ do
    it "inserts text" $
      let te = TextEdit (Range (Position 1 2) (Position 1 2)) "foo"
        in applyTextEdit te "lorem\nipsum\ndolor" `shouldBe` "lorem\nipfoosum\ndolor"
    it "deletes text" $
      let te = TextEdit (Range (Position 0 2) (Position 1 2)) ""
        in applyTextEdit te "lorem\nipsum\ndolor" `shouldBe` "losum\ndolor"
    it "edits a multiline text" $
      let te = TextEdit (Range (Position 1 0) (Position 2 0)) "slorem"
        in applyTextEdit te "lorem\nipsum\ndolor" `shouldBe` "lorem\nsloremdolor"
    it "inserts text past the last line" $
      let te = TextEdit (Range (Position 3 2) (Position 3 2)) "foo"
        in applyTextEdit te "lorem\nipsum\ndolor" `shouldBe` "lorem\nipsum\ndolorfoo"

  describe "editTextEdit" $
    it "edits a multiline text edit" $
      let orig = TextEdit (Range (Position 1 1) (Position 2 2)) "hello\nworld"
          inner = TextEdit (Range (Position 0 3) (Position 1 3)) "ios\ngo"
          expected = TextEdit (Range (Position 1 1) (Position 2 2)) "helios\ngold"
         in editTextEdit orig inner `shouldBe` expected
