module CapabilitiesSpec where

import Language.LSP.Protocol.Capabilities
import Prettyprinter
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = describe "capabilities" $ do
  it "produces full latest client capabilities" $ defaultGolden "fullCaps" $ show $ pretty fullLatestClientCaps
  it "produces pre-3.10 client capabilities" $ defaultGolden "oldCaps" $ show $ pretty $ fullClientCapsForVersion (LSPVersion 3 9)
