module CapabilitiesSpec where

import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Test.Hspec

spec :: Spec
spec = describe "capabilities" $ do
  it "gives 3.10 capabilities" $
    let ClientCapabilities _ (Just tdcs) _ _ = capsForVersion (LSPVersion 3 10)
        Just (DocumentSymbolClientCapabilities _ _ mHierarchical _ _ ) = _documentSymbol tdcs
      in mHierarchical `shouldBe` Just True
  it "gives pre 3.10 capabilities" $
      let ClientCapabilities _ (Just tdcs) _ _ = capsForVersion (LSPVersion 3 9)
          Just (DocumentSymbolClientCapabilities _ _ mHierarchical _ _) = _documentSymbol tdcs
        in mHierarchical `shouldBe` Nothing
