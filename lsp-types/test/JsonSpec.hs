{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
-- we're using some deprecated stuff from the LSP spec, that's fine
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Test for JSON serialization
module JsonSpec where

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types

import Language.LSP.Protocol.QuickCheck ()

import Data.Aeson qualified as J
import Data.List (isPrefixOf)
import Data.Row
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
