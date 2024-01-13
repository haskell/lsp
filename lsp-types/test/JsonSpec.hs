{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- we're using some deprecated stuff from the LSP spec, that's fine
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test for JSON serialization
module JsonSpec where

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types

import Data.Aeson qualified as J
import Data.List (isPrefixOf)
import Data.Row
import Data.Row qualified as R
import Data.Row.Records qualified as R
import Data.Void
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Instances ()

-- ---------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = do
  describe "MarkedString" $ do
    prop "roundtrip" (propertyJsonRoundtrip :: MarkedString -> Property)

  describe "MarkupContent" $ do
    prop "roundtrip" (propertyJsonRoundtrip :: MarkupContent -> Property)

  describe "TextDocumentContentChangeEvent" $ do
    prop "roundtrip" (propertyJsonRoundtrip :: TextDocumentContentChangeEvent -> Property)

  describe "WatchedFiles" $ do
    prop "roundtrip" (propertyJsonRoundtrip :: DidChangeWatchedFilesRegistrationOptions -> Property)

  describe "Registration" $ do
    -- Registration has a 'Maybe Value' field, so this test checks that 'Maybe Null' roundtrips properly
    prop "roundtrip" (propertyJsonRoundtrip :: Registration -> Property)

  describe "CompletionList" $ do
    it "handles optional field set to null in record" $ do
      J.eitherDecode "{ \"isIncomplete\" : true, \"itemDefaults\" : { \"data\" : null }, \"items\": [] }"
        `shouldBe` Right
          ( CompletionList
              True
              (Just (#commitCharacters .== Nothing .+ #editRange .== Nothing .+ #insertTextFormat .== Nothing .+ #insertTextMode .== Nothing .+ #data .== Just J.Null))
              mempty
          )

  describe "CompletionItem" $ do
    it "example" $
      (J.decode "{\"jsonrpc\":\"2.0\",\"result\":[{\"label\":\"raisebox\"}],\"id\":1}" :: Maybe (TResponseMessage 'Method_TextDocumentCompletion))
        `shouldNotBe` Nothing

  describe "RequestMessage" $ do
    it "handles missing params field" $ do
      J.eitherDecode "{ \"jsonrpc\": \"2.0\", \"id\": 15, \"method\": \"shutdown\"}"
        `shouldBe` Right (TRequestMessage "2.0" (IdInt 15) SMethod_Shutdown Nothing)
    -- The 'params' field on a RequestMessage is optional _and_ null is _not_ a valid value. Check that we correctly parse null as 'Nothing'
    it "handles params field set to null" $ do
      J.eitherDecode "{ \"jsonrpc\": \"2.0\", \"id\": 15, \"method\": \"shutdown\", \"params\": null }"
        `shouldBe` Right (TRequestMessage "2.0" (IdInt 15) SMethod_Shutdown Nothing)

  describe "ResponseMessage" $ do
    prop
      "Hover roundtrip"
      (propertyJsonRoundtrip :: TResponseMessage 'Method_TextDocumentHover -> Property)
    -- The 'data' field on a ResponseError is optional _and_ null is a valid value. Check that we correctly parse null as 'Just Null'
    it "decodes error data = null" $ do
      let input = "{\"jsonrpc\": \"2.0\", \"id\": 123, \"error\": { \"code\": -32700, \"message\": \"oh no\", \"data\": null }}"
       in J.decode input
            `shouldBe` Just
              ( (TResponseMessage "2.0" (Just (IdInt 123)) (Left $ ResponseError (InR ErrorCodes_ParseError) "oh no" (Just J.Null))) ::
                  TResponseMessage ('Method_CustomMethod "hello")
              )
    it "throws if neither result nor error is present" $ do
      (J.eitherDecode "{\"jsonrpc\":\"2.0\",\"id\":1}" :: Either String (TResponseMessage 'Method_Initialize))
        `shouldBe` Left ("Error in $: both error and result cannot be Nothing")
    it "throws if both result and error are present" $ do
      ( J.eitherDecode
          "{\"jsonrpc\":\"2.0\",\"id\": 1,\"result\":{\"capabilities\": {}},\"error\":{\"code\":-32700,\"message\":\"\",\"data\":{ \"retry\":false}}}" ::
          Either String (TResponseMessage 'Method_Initialize)
        )
        `shouldSatisfy` (either (\err -> "Error in $: both error and result cannot be present" `isPrefixOf` err) (\_ -> False))
    it "decodes result = null" $ do
      let input = "{\"jsonrpc\": \"2.0\", \"id\": 123, \"result\": null}"
       in J.decode input
            `shouldBe` Just
              ((TResponseMessage "2.0" (Just (IdInt 123)) (Right $ InL J.Null)) :: TResponseMessage 'Method_WorkspaceExecuteCommand)

  describe "NotificationMessage" $ do
    it "handles missing params field" $ do
      J.eitherDecode "{ \"jsonrpc\": \"2.0\", \"method\": \"exit\"}"
        `shouldBe` Right (TNotificationMessage "2.0" SMethod_Exit Nothing)

-- ---------------------------------------------------------------------

propertyJsonRoundtrip :: (Eq a, Show a, J.ToJSON a, J.FromJSON a) => a -> Property
propertyJsonRoundtrip a = J.Success a === J.fromJSON (J.toJSON a)

-- ---------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (a |? b) where
  arbitrary = oneof [InL <$> arbitrary, InR <$> arbitrary]
  shrink = genericShrink

instance Arbitrary Null where
  arbitrary = pure Null

instance (R.AllUniqueLabels r, R.Forall r Arbitrary) => Arbitrary (R.Rec r) where
  arbitrary = R.fromLabelsA @Arbitrary $ \_l -> arbitrary
  shrink record = R.traverse @Arbitrary @[] shrink record

deriving newtype instance Arbitrary MarkedString

instance Arbitrary MarkupContent where
  arbitrary = MarkupContent <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary MarkupKind where
  arbitrary = oneof [pure MarkupKind_PlainText, pure MarkupKind_Markdown]
  shrink = genericShrink

instance Arbitrary UInt where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Uri where
  arbitrary = Uri <$> arbitrary
  shrink = genericShrink

-- deriving newtype instance Arbitrary URI

instance Arbitrary WorkspaceFolder where
  arbitrary = WorkspaceFolder <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary RelativePattern where
  arbitrary = RelativePattern <$> arbitrary <*> arbitrary
  shrink = genericShrink

deriving newtype instance Arbitrary Pattern
deriving newtype instance Arbitrary GlobPattern

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Range where
  arbitrary = Range <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Hover where
  arbitrary = Hover <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance {-# OVERLAPPING #-} Arbitrary (Maybe Void) where
  arbitrary = pure Nothing

instance (ErrorData m ~ Maybe Void) => Arbitrary (TResponseError m) where
  arbitrary = TResponseError <$> arbitrary <*> arbitrary <*> pure Nothing
  shrink = genericShrink

instance Arbitrary ResponseError where
  arbitrary = ResponseError <$> arbitrary <*> arbitrary <*> pure Nothing
  shrink = genericShrink

instance (Arbitrary (MessageResult m), ErrorData m ~ Maybe Void) => Arbitrary (TResponseMessage m) where
  arbitrary = TResponseMessage <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (LspId m) where
  arbitrary = oneof [IdInt <$> arbitrary, IdString <$> arbitrary]
  shrink = genericShrink

instance Arbitrary ErrorCodes where
  arbitrary =
    elements
      [ ErrorCodes_ParseError
      , ErrorCodes_InvalidRequest
      , ErrorCodes_MethodNotFound
      , ErrorCodes_InvalidParams
      , ErrorCodes_InternalError
      , ErrorCodes_ServerNotInitialized
      , ErrorCodes_UnknownErrorCode
      ]
  shrink = genericShrink

instance Arbitrary LSPErrorCodes where
  arbitrary =
    elements
      [ LSPErrorCodes_RequestFailed
      , LSPErrorCodes_ServerCancelled
      , LSPErrorCodes_ContentModified
      , LSPErrorCodes_RequestCancelled
      ]
  shrink = genericShrink

-- ---------------------------------------------------------------------

instance Arbitrary Registration where
  arbitrary = Registration <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary DidChangeWatchedFilesRegistrationOptions where
  arbitrary = DidChangeWatchedFilesRegistrationOptions <$> arbitrary
  shrink = genericShrink

instance Arbitrary FileSystemWatcher where
  arbitrary = FileSystemWatcher <$> arbitrary <*> arbitrary
  shrink = genericShrink

-- TODO: watchKind is weird
instance Arbitrary WatchKind where
  arbitrary = oneof [pure WatchKind_Change, pure WatchKind_Create, pure WatchKind_Delete]
  shrink = genericShrink

-- ---------------------------------------------------------------------
--
instance Arbitrary TextDocumentContentChangeEvent where
  arbitrary = TextDocumentContentChangeEvent <$> arbitrary
  shrink = genericShrink
