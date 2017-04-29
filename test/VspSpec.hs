{-# LANGUAGE OverloadedStrings #-}
module VspSpec where


import           Language.Haskell.LSP.VFS
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Yi.Rope as Yi

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

    -- ---------------------------------

  describe "deletes characters" $ do
    it "deletes characters within a line" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]
        new = deleteChars (Yi.fromString orig) (J.Position 2 1) 4
      lines (Yi.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "-oo"
          , "foo :: Int"
          ]

    -- ---------------------------------

    it "deletes one line" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]
        new = deleteChars (Yi.fromString orig) (J.Position 2 0) 8
      lines (Yi.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "foo :: Int"
          ]

    -- ---------------------------------

  describe "adds characters" $ do
    it "adds one line" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "abcdg"
          , "module Foo where"
          , "foo :: Int"
          ]
        new = addChars (Yi.fromString orig) (J.Position 1 16) "\n-- fooo"
      lines (Yi.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]

    -- ---------------------------------

  describe "changes characters" $ do
    it "removes end of a line" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          , "foo = bb"
          , ""
          , "bb = 5"
          , ""
          , "baz = do"
          , "  putStrLn \"hello world\""
          ]
        -- new = changeChars (Yi.fromString orig) (J.Position 7 0) (J.Position 7 8) "baz ="
        new = changeChars (Yi.fromString orig) (J.Position 7 0) 8 "baz ="
      lines (Yi.toString new) `shouldBe`
          [ "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          , "foo = bb"
          , ""
          , "bb = 5"
          , ""
          , "baz ="
          , "  putStrLn \"hello world\""
          ]
