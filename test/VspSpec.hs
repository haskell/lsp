{-# LANGUAGE OverloadedStrings #-}
module VspSpec where


import           Data.String
import qualified Data.Rope.UTF16 as Rope
import           Language.LSP.VFS
import qualified Language.LSP.Types as J
import qualified Data.Text as T

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

vfsFromText :: T.Text -> VirtualFile
vfsFromText text = VirtualFile 0 0 (Rope.fromText text)

-- ---------------------------------------------------------------------

vspSpec :: Spec
vspSpec = do
  describe "applys changes in order" $ do
    it "handles vscode style undos" $ do
      let orig = "abc"
          changes =
            [ J.TextDocumentContentChangeEvent (mkRange 0 2 0 3) Nothing ""
            , J.TextDocumentContentChangeEvent (mkRange 0 1 0 2) Nothing ""
            , J.TextDocumentContentChangeEvent (mkRange 0 0 0 1) Nothing ""
            ]
      applyChanges orig changes `shouldBe` ""
    it "handles vscode style redos" $ do
      let orig = ""
          changes =
            [ J.TextDocumentContentChangeEvent (mkRange 0 1 0 1) Nothing "a"
            , J.TextDocumentContentChangeEvent (mkRange 0 2 0 2) Nothing "b"
            , J.TextDocumentContentChangeEvent (mkRange 0 3 0 3) Nothing "c"
            ]
      applyChanges orig changes `shouldBe` "abc"

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
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 2 1 2 5) (Just 4) ""
      lines (Rope.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "-oo"
          , "foo :: Int"
          ]

    it "deletes characters within a line (no len)" $ do
      let
        orig = unlines
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 2 1 2 5) Nothing ""
      lines (Rope.toString new) `shouldBe`
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
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 2 0 3 0) (Just 8) ""
      lines (Rope.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "foo :: Int"
          ]

    it "deletes one line(no len)" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 2 0 3 0) Nothing ""
      lines (Rope.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "foo :: Int"
          ]
    -- ---------------------------------

    it "deletes two lines" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          , "foo = bb"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 1 0 3 0) (Just 19) ""
      lines (Rope.toString new) `shouldBe`
          [ "module Foo where"
          , "foo = bb"
          ]

    it "deletes two lines(no len)" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          , "foo = bb"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 1 0 3 0) Nothing ""
      lines (Rope.toString new) `shouldBe`
          [ "module Foo where"
          , "foo = bb"
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
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 1 16 1 16) (Just 0) "\n-- fooo"
      lines (Rope.toString new) `shouldBe`
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]

    -- ---------------------------------

    it "adds two lines" $ do
      -- based on vscode log
      let
        orig = unlines
          [ "module Foo where"
          , "foo = bb"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 1 8 1 8) Nothing "\n-- fooo\nfoo :: Int"
      lines (Rope.toString new) `shouldBe`
          [ "module Foo where"
          , "foo = bb"
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
        -- new = changeChars (fromString orig) (J.Position 7 0) (J.Position 7 8) "baz ="
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 7 0 7 8) (Just 8) "baz ="
      lines (Rope.toString new) `shouldBe`
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
    it "removes end of a line(no len)" $ do
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
        -- new = changeChars (fromString orig) (J.Position 7 0) (J.Position 7 8) "baz ="
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 7 0 7 8) Nothing "baz ="
      lines (Rope.toString new) `shouldBe`
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
    it "indexes using utf-16 code units" $ do
      let
        orig = unlines
          [ "a𐐀b"
          , "a𐐀b"
          ]
        new = applyChange (fromString orig)
                $ J.TextDocumentContentChangeEvent (mkRange 1 0 1 3) (Just 3) "𐐀𐐀"
      lines (Rope.toString new) `shouldBe`
          [ "a𐐀b"
          , "𐐀𐐀b"
          ]

    -- ---------------------------------

  describe "LSP utilities" $ do
    it "splits at a line" $ do
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
        (left,right) = Rope.splitAtLine 4 (fromString orig)

      lines (Rope.toString left) `shouldBe`
          [ "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          , "foo = bb"
          ]
      lines (Rope.toString right) `shouldBe`
          [ ""
          , "bb = 5"
          , ""
          , "baz = do"
          , "  putStrLn \"hello world\""
          ]

    -- ---------------------------------

    it "getCompletionPrefix" $ do
      let
        orig = T.unlines
          [ "{-# ings #-}"
          , "import Data.List"
          ]
      pp4 <- getCompletionPrefix (J.Position 0 4) (vfsFromText orig)
      pp4 `shouldBe` Just (PosPrefixInfo "{-# ings #-}" "" "" (J.Position 0 4))

      pp5 <- getCompletionPrefix (J.Position 0 5) (vfsFromText orig)
      pp5 `shouldBe` Just (PosPrefixInfo "{-# ings #-}" "" "i" (J.Position 0 5))

      pp6 <- getCompletionPrefix (J.Position 0 6) (vfsFromText orig)
      pp6 `shouldBe` Just (PosPrefixInfo "{-# ings #-}" "" "in" (J.Position 0 6))

      pp14 <- getCompletionPrefix (J.Position 1 14) (vfsFromText orig)
      pp14 `shouldBe` Just (PosPrefixInfo "import Data.List" "Data" "Li" (J.Position 1 14))

      pp00 <- getCompletionPrefix (J.Position 0 0) (vfsFromText orig)
      pp00 `shouldBe` Just (PosPrefixInfo "{-# ings #-}" "" "" (J.Position 0 0))
