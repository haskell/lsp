{-# LANGUAGE OverloadedStrings #-}

module LocationSpec where

import           Language.LSP.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isSubrangeOf" $ do
    it "is true if the first range is totally inside the second range" $
      isSubrangeOf (mkRange 1 2 1 5) (mkRange 1 1 1 6) `shouldBe` True
    it "is true if two ranges equal" $
      isSubrangeOf (mkRange 1 2 1 5) (mkRange 1 2 1 5) `shouldBe` True
    it "is false if the first range is outside of the second" $
      isSubrangeOf (mkRange 1 1 1 5) (mkRange 1 2 1 5) `shouldBe` False

  describe "positionInRange" $ do
    it "is false if position is after the end of a single line range" $
      positionInRange (Position 1 10) (Range (Position 1 1) (Position 1 3)) `shouldBe` False
    it "is false if position is before the begining of a single line range" $
      positionInRange (Position 1 0) (Range (Position 1 1) (Position 1 6)) `shouldBe` False
    it "is true if position is in a single line range" $
      positionInRange (Position 1 5) (Range (Position 1 1) (Position 1 6)) `shouldBe` True
    it "is false if position is right at the end of the range" $
      positionInRange (Position 1 5) (Range (Position 1 1) (Position 1 5)) `shouldBe` False
    it "is true if position is in the middle of a multiline range" $
      positionInRange (Position 3 5) (Range (Position 1 1) (Position 5 6)) `shouldBe` True
    it "is false if position is before the beginning of a multiline range" $
      positionInRange (Position 3 5) (Range (Position 3 6) (Position 4 10)) `shouldBe` False
    it "is false if position is right at the end of a multiline range" $
      positionInRange (Position 4 10) (Range (Position 3 6) (Position 4 10)) `shouldBe` False
