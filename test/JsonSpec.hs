{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Test for JSON serialization
module JsonSpec where

import           Language.Haskell.LSP.Types

import           Data.Aeson
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck               hiding (Success)
import           Test.QuickCheck.Instances     ()

-- import Debug.Trace
-- ---------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "dispatcher" jsonSpec

-- ---------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = do
  describe "General JSON instances round trip" $ do
  -- DataTypesJSON
    prop "LanguageString"    (propertyJsonRoundtrip :: LanguageString -> Bool)
    prop "MarkedString"      (propertyJsonRoundtrip :: MarkedString -> Bool)
    prop "MarkupContent"     (propertyJsonRoundtrip :: MarkupContent -> Bool)
    prop "HoverContents"     (propertyJsonRoundtrip :: HoverContents -> Bool)


-- ---------------------------------------------------------------------

propertyJsonRoundtrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
propertyJsonRoundtrip a = Success a == fromJSON (toJSON a)

-- ---------------------------------------------------------------------

instance Arbitrary LanguageString where
  arbitrary = LanguageString <$> arbitrary <*> arbitrary

instance Arbitrary MarkedString where
  arbitrary = oneof [PlainString <$> arbitrary, CodeString <$> arbitrary]

instance Arbitrary MarkupContent where
  arbitrary = MarkupContent <$> arbitrary <*> arbitrary

instance Arbitrary MarkupKind where
  arbitrary = oneof [pure MkPlainText,pure MkMarkdown]

instance Arbitrary HoverContents where
  arbitrary = oneof [HoverContentsMS <$> arbitrary, HoverContents <$> arbitrary]

-- | make lists of maximum length 3 for test performance
smallList :: Gen a -> Gen [a]
smallList = resize 3 . listOf

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = List <$> arbitrary

-- ---------------------------------------------------------------------
