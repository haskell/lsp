{-# LANGUAGE OverloadedStrings #-}
module ServerCapabilitiesSpec where

import Control.Lens.Operators
import Data.Aeson
import Data.Monoid ((<>))
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Test.Hspec

spec :: Spec
spec = describe "server capabilities" $ do
  describe "folding range options" $ do
    describe "decodes" $ do
      it "just id" $
        let input = "{\"id\": \"abc123\"}"
          in decode input `shouldBe` Just (FoldingRangeOptionsDynamicDocument Nothing (Just "abc123"))
      it "id and document selector" $
        let input = "{\"id\": \"foo\", \"documentSelector\": " <> documentFiltersJson <> "}"
          in decode input `shouldBe` Just (FoldingRangeOptionsDynamicDocument (Just documentFilters) (Just "foo"))
      it "static boolean" $
        let input = "true"
          in decode input `shouldBe` Just (FoldingRangeOptionsStatic True)
    describe "encodes" $
      it "just id" $
        encode (FoldingRangeOptionsDynamicDocument Nothing (Just "foo")) `shouldBe` "{\"id\":\"foo\"}"
  it "decodes" $
    let input = "{\"hoverProvider\": true, \"colorProvider\": {\"id\": \"abc123\", \"documentSelector\": " <> documentFiltersJson <> "}}"
        Just caps = decode input :: Maybe InitializeResponseCapabilitiesInner
      in caps ^. colorProvider `shouldBe` Just (ColorOptionsDynamicDocument (Just documentFilters) (Just "abc123"))
  where
    documentFilters = List [DocumentFilter (Just "haskell") Nothing Nothing]
    documentFiltersJson = "[{\"language\": \"haskell\"}]"
