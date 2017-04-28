{-# LANGUAGE OverloadedStrings #-}
module VspSpec where


import           Language.Haskell.LSP.VFS
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VSP functions" vspSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------


mkRange :: Int -> Int -> Int -> Int -> Maybe J.Range
mkRange ls cs le ce = Just $ J.Range (J.Position ls cs) (J.Position le ce)

-- ---------------------------------------------------------------------

vspSpec :: Spec
vspSpec = do
  describe "sorts changes" $ do
    it "sorts changes that all have ranges" $ do
      let
        unsorted =
          [ (J.TextDocumentContentChangeEvent (mkRange 1 0 2 0) Nothing "")
          , (J.TextDocumentContentChangeEvent (mkRange 2 0 3 0) Nothing "")
          ]
      (sortChanges unsorted) `shouldBe`
          [ (J.TextDocumentContentChangeEvent (mkRange 2 0 3 0) Nothing "")
          , (J.TextDocumentContentChangeEvent (mkRange 1 0 2 0) Nothing "")
          ]
