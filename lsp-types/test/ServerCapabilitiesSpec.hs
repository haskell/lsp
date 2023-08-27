{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerCapabilitiesSpec where

import Control.Lens.Operators
import Data.Aeson hiding (Null)
import Data.Maybe (fromJust)
import Data.Row
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Hspec

spec :: Spec
spec = describe "server capabilities" $ do
  describe "folding range options" $ do
    describe "decodes" $ do
      it "just id" $
        let input = "{\"id\": \"abc123\", \"documentSelector\": null}"
         in decode input `shouldBe` Just (FoldingRangeRegistrationOptions (InR Null) Nothing (Just "abc123"))
      it "id and document selector" $
        let input = "{\"id\": \"foo\", \"documentSelector\": " <> documentFiltersJson <> "}"
         in decode input `shouldBe` Just (FoldingRangeRegistrationOptions (InL documentFilters) Nothing (Just "foo"))
      it "static boolean" $
        let input = "true"
         in decode input `shouldBe` Just True
    describe "encodes" $
      it "just id" $
        encode (FoldingRangeRegistrationOptions (InR Null) Nothing (Just "foo")) `shouldBe` "{\"documentSelector\":null,\"id\":\"foo\"}"
  it "decodes" $
    let input = "{\"hoverProvider\": true, \"colorProvider\": {\"id\": \"abc123\", \"documentSelector\": " <> documentFiltersJson <> "}}"
        caps :: ServerCapabilities = fromJust $ decode input
     in caps ^. colorProvider `shouldBe` Just (InR $ InR $ DocumentColorRegistrationOptions (InL documentFilters) Nothing (Just "abc123"))
  describe "client/registerCapability" $
    it "allows empty registerOptions" $
      let input = "{\"registrations\":[{\"registerOptions\":{},\"method\":\"workspace/didChangeConfiguration\",\"id\":\"4a56f5ca-7188-4f4c-a366-652d6f9d63aa\"}]}"
          registrationParams :: RegistrationParams = fromJust $ decode input
       in registrationParams ^. registrations
            `shouldBe` [ toUntypedRegistration $
                          TRegistration
                            "4a56f5ca-7188-4f4c-a366-652d6f9d63aa"
                            SMethod_WorkspaceDidChangeConfiguration
                            (Just $ DidChangeConfigurationRegistrationOptions Nothing)
                       ]
 where
  documentFilters = DocumentSelector [DocumentFilter $ InL $ TextDocumentFilter $ InL $ #language .== "haskell" .+ #scheme .== Nothing .+ #pattern .== Nothing]
  documentFiltersJson = "[{\"language\": \"haskell\"}]"
