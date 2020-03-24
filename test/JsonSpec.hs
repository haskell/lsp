{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Test for JSON serialization
module JsonSpec where

import           Language.Haskell.LSP.Types

import qualified Data.Aeson                    as J
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
    prop "LanguageString"    (propertyJsonRoundtrip :: LanguageString -> Property)
    prop "MarkedString"      (propertyJsonRoundtrip :: MarkedString -> Property)
    prop "MarkupContent"     (propertyJsonRoundtrip :: MarkupContent -> Property)
    prop "HoverContents"     (propertyJsonRoundtrip :: HoverContents -> Property)
    prop "ResponseError"     (propertyJsonRoundtrip :: ResponseError -> Property)
    -- todo make sure it generates Left(error)
    prop "ResponseMessage"   (propertyJsonRoundtrip :: ResponseMessage () -> Property)
    prop "ResponseMessage"   (propertyJsonRoundtrip :: ResponseMessage J.Value  -> Property)


-- ---------------------------------------------------------------------

propertyJsonRoundtrip :: (Eq a, Show a, J.ToJSON a, J.FromJSON a) => a -> Property
propertyJsonRoundtrip a = J.Success a === J.fromJSON (J.toJSON a)

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
  arbitrary = oneof [ HoverContentsMS <$> arbitrary
                    , HoverContents <$> arbitrary
                    ]

instance Arbitrary a => Arbitrary (ResponseMessage a) where
  arbitrary =
    oneof
      [ ResponseMessage
          <$> arbitrary
          <*> arbitrary
          <*> (Right <$> arbitrary)
      , ResponseMessage
          <$> arbitrary
          <*> arbitrary
          <*> (Left <$> arbitrary)
      ]

instance Arbitrary LspIdRsp where
  arbitrary = oneof [IdRspInt <$> arbitrary, IdRspString <$> arbitrary, pure IdRspNull]

instance Arbitrary ResponseError where
  arbitrary = ResponseError <$> arbitrary <*> arbitrary <*> pure Nothing

instance Arbitrary ErrorCode where
  arbitrary =
    elements
      [ ParseError
      , InvalidRequest
      , MethodNotFound
      , InvalidParams
      , InternalError
      , ServerErrorStart
      , ServerErrorEnd
      , ServerNotInitialized
      , UnknownErrorCode
      , RequestCancelled
      , ContentModified
      ]

-- | make lists of maximum length 3 for test performance
smallList :: Gen a -> Gen [a]
smallList = resize 3 . listOf

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = List <$> arbitrary

instance Arbitrary J.Value where
  arbitrary = oneof [J.String <$> arbitrary, J.Number <$> arbitrary, J.Bool <$> arbitrary, pure J.Null]
-- ---------------------------------------------------------------------
