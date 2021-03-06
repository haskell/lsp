{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- For the use of MarkedString
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | Test for JSON serialization
module JsonSpec where

import           Language.LSP.Types

import qualified Data.Aeson                    as J
import           Data.List(isPrefixOf)
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
spec = do
  describe "dispatcher" jsonSpec
  describe "ResponseMessage"  responseMessageSpec

-- ---------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = do
  describe "General JSON instances round trip" $ do
  -- DataTypesJSON
    prop "LanguageString" (propertyJsonRoundtrip :: LanguageString -> Property)
    prop "MarkedString"   (propertyJsonRoundtrip :: MarkedString -> Property)
    prop "MarkupContent"  (propertyJsonRoundtrip :: MarkupContent -> Property)
    prop "HoverContents"  (propertyJsonRoundtrip :: HoverContents -> Property)
    prop "ResponseError"  (propertyJsonRoundtrip :: ResponseError -> Property)
    prop "WatchedFiles"   (propertyJsonRoundtrip :: DidChangeWatchedFilesRegistrationOptions -> Property)
    prop "ResponseMessage Initialize"
         (propertyJsonRoundtrip :: ResponseMessage 'TextDocumentHover -> Property)
    -- prop "ResponseMessage JSON value"
        --  (propertyJsonRoundtrip :: ResponseMessage J.Value -> Property)
  describe "JSON decoding regressions" $
    it "CompletionItem" $
      (J.decode "{\"jsonrpc\":\"2.0\",\"result\":[{\"label\":\"raisebox\"}],\"id\":1}" :: Maybe (ResponseMessage 'TextDocumentCompletion))
        `shouldNotBe` Nothing


responseMessageSpec :: Spec
responseMessageSpec = do
  describe "edge cases" $ do
    it "decodes result = null" $ do
      let input = "{\"jsonrpc\": \"2.0\", \"id\": 123, \"result\": null}"
        in  J.decode input `shouldBe` Just
              ((ResponseMessage "2.0" (Just (IdInt 123)) (Right J.Null)) :: ResponseMessage 'WorkspaceExecuteCommand)
    it "handles missing params field" $ do
      J.eitherDecode "{ \"jsonrpc\": \"2.0\", \"id\": 15, \"method\": \"shutdown\"}"
        `shouldBe` Right (RequestMessage "2.0" (IdInt 15) SShutdown Empty)
  describe "invalid JSON" $ do
    it "throws if neither result nor error is present" $ do
      (J.eitherDecode "{\"jsonrpc\":\"2.0\",\"id\":1}" :: Either String (ResponseMessage 'Initialize))
        `shouldBe` Left ("Error in $: both error and result cannot be Nothing")
    it "throws if both result and error are present" $ do
      (J.eitherDecode
        "{\"jsonrpc\":\"2.0\",\"id\": 1,\"result\":{\"capabilities\": {}},\"error\":{\"code\":-32700,\"message\":\"\",\"data\":null}}"
        :: Either String (ResponseMessage 'Initialize))
        `shouldSatisfy`
          (either (\err -> "Error in $: both error and result cannot be present" `isPrefixOf` err) (\_ -> False))

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

instance Arbitrary Uri where
  arbitrary = Uri <$> arbitrary

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary

instance Arbitrary Range where
  arbitrary = Range <$> arbitrary <*> arbitrary

instance Arbitrary Hover where
  arbitrary = Hover <$> arbitrary <*> arbitrary

instance Arbitrary (ResponseResult m) => Arbitrary (ResponseMessage m) where
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

instance Arbitrary (LspId m) where
  arbitrary = oneof [IdInt <$> arbitrary, IdString <$> arbitrary]

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
  arbitrary = oneof
    [ J.String <$> arbitrary
    , J.Number <$> arbitrary
    , J.Bool <$> arbitrary
    , pure J.Null
    ]

-- ---------------------------------------------------------------------

instance Arbitrary DidChangeWatchedFilesRegistrationOptions where
  arbitrary = DidChangeWatchedFilesRegistrationOptions <$> arbitrary

instance Arbitrary FileSystemWatcher where
  arbitrary = FileSystemWatcher <$> arbitrary <*> arbitrary

instance Arbitrary WatchKind where
  arbitrary = WatchKind <$> arbitrary <*> arbitrary <*> arbitrary

-- ---------------------------------------------------------------------
