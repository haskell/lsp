{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CapabilitiesSpec where

import Language.LSP.Protocol.Capabilities
import Language.LSP.Protocol.Types
import Test.Hspec
import Test.Hspec.Golden
import Prettyprinter

spec :: Spec
spec = describe "capabilities" $ do
  it "produces full latest client capabilities" $ defaultGolden "fullCaps" $ show $ pretty fullCaps
  it "produces pre-3.10 client capabilities" $ defaultGolden "oldCaps" $ show $ pretty $ capsForVersion (LSPVersion 3 9)
