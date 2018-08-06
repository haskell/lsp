module CapabilitiesSpec where

import Language.Haskell.LSP.Types.Capabilities
import Test.Hspec

spec :: Spec
spec = describe "capabilities" $ do
  it "gives 3.10 capabilities" $
    let ClientCapabilities _ (Just tdcs) _ = capsForVersion (LSPVersion 3 10)
        Just (DocumentSymbolClientCapabilities _ _ mHierarchical) = _documentSymbol tdcs
      in mHierarchical `shouldBe` Just True
  it "gives pre 3.10 capabilities" $
      let ClientCapabilities _ (Just tdcs) _ = capsForVersion (LSPVersion 3 9)
          Just (DocumentSymbolClientCapabilities _ _ mHierarchical) = _documentSymbol tdcs
        in mHierarchical `shouldBe` Nothing
