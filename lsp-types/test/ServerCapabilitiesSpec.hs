{-# LANGUAGE OverloadedStrings #-}
module ServerCapabilitiesSpec where

import Control.Lens.Operators
import Data.Aeson
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Language.LSP.Types.Lens
import Test.Hspec

spec :: Spec
spec = describe "server capabilities" $ do
  describe "folding range options" $ do
    describe "decodes" $ do
      it "just id" $
        let input = "{\"id\": \"abc123\"}"
          in decode input `shouldBe` Just (FoldingRangeRegistrationOptions Nothing Nothing (Just "abc123"))
      it "id and document selector" $
        let input = "{\"id\": \"foo\", \"documentSelector\": " <> documentFiltersJson <> "}"
          in decode input `shouldBe` Just (FoldingRangeRegistrationOptions (Just documentFilters) Nothing (Just "foo"))
      it "static boolean" $
        let input = "true"
          in decode input `shouldBe` Just True
    describe "encodes" $
      it "just id" $
        encode (FoldingRangeRegistrationOptions Nothing Nothing (Just "foo")) `shouldBe` "{\"id\":\"foo\"}"
  it "decodes" $
    let input = "{\"hoverProvider\": true, \"colorProvider\": {\"id\": \"abc123\", \"documentSelector\": " <> documentFiltersJson <> "}}"
        Just caps = decode input :: Maybe ServerCapabilities
      in caps ^. colorProvider `shouldBe` Just (InR $ InR $ DocumentColorRegistrationOptions (Just documentFilters) (Just "abc123") Nothing)
  describe "client/registerCapability" $
    it "allows empty registerOptions" $
      let input = "{\"registrations\":[{\"registerOptions\":{},\"method\":\"workspace/didChangeConfiguration\",\"id\":\"4a56f5ca-7188-4f4c-a366-652d6f9d63aa\"}]}"
          Just registrationParams = decode input :: Maybe RegistrationParams
        in registrationParams ^. registrations `shouldBe`
             List [SomeRegistration $ Registration "4a56f5ca-7188-4f4c-a366-652d6f9d63aa"
                                      SWorkspaceDidChangeConfiguration (Just Empty)]
  where
    documentFilters = List [DocumentFilter (Just "haskell") Nothing Nothing]
    documentFiltersJson = "[{\"language\": \"haskell\"}]"
