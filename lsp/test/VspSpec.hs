{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module VspSpec where

import Data.String
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.VFS

import Data.Functor.Identity
import Test.Hspec

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

vfsFromText :: T.Text -> VirtualFile
vfsFromText text = VirtualFile 0 0 (Rope.fromText text)

-- ---------------------------------------------------------------------

mkChangeEvent :: J.Range -> T.Text -> J.TextDocumentContentChangeEvent
mkChangeEvent r t = J.TextDocumentContentChangeEvent $ J.InL $ J.TextDocumentContentChangePartial{J._range = r, J._rangeLength = Nothing, J._text = t}

vspSpec :: Spec
vspSpec = do
  describe "applys changes in order" $ do
    it "handles vscode style undos" $ do
      let orig = "abc"
          changes =
            [ mkChangeEvent (J.mkRange 0 2 0 3) ""
            , mkChangeEvent (J.mkRange 0 1 0 2) ""
            , mkChangeEvent (J.mkRange 0 0 0 1) ""
            ]
      applyChanges mempty orig changes `shouldBe` Identity ""
    it "handles vscode style redos" $ do
      let orig = ""
          changes =
            [ mkChangeEvent (J.mkRange 0 1 0 1) "a"
            , mkChangeEvent (J.mkRange 0 2 0 2) "b"
            , mkChangeEvent (J.mkRange 0 3 0 3) "c"
            ]
      applyChanges mempty orig changes `shouldBe` Identity "abc"

  -- ---------------------------------

  describe "deletes characters" $ do
    it "deletes characters within a line" $ do
      -- based on vscode log
      let
        orig =
          unlines
            [ "abcdg"
            , "module Foo where"
            , "-- fooo"
            , "foo :: Int"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 2 1 2 5) ""
      Rope.lines <$> new
        `shouldBe` Identity
          [ "abcdg"
          , "module Foo where"
          , "-oo"
          , "foo :: Int"
          ]

    -- ---------------------------------

    it "deletes one line" $ do
      -- based on vscode log
      let
        orig =
          unlines
            [ "abcdg"
            , "module Foo where"
            , "-- fooo"
            , "foo :: Int"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 2 0 3 0) ""
      Rope.lines <$> new
        `shouldBe` Identity
          [ "abcdg"
          , "module Foo where"
          , "foo :: Int"
          ]

    -- ---------------------------------

    it "deletes two lines" $ do
      -- based on vscode log
      let
        orig =
          unlines
            [ "module Foo where"
            , "-- fooo"
            , "foo :: Int"
            , "foo = bb"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 1 0 3 0) ""
      Rope.lines <$> new
        `shouldBe` Identity
          [ "module Foo where"
          , "foo = bb"
          ]

  -- ---------------------------------

  describe "adds characters" $ do
    it "adds one line" $ do
      -- based on vscode log
      let
        orig =
          unlines
            [ "abcdg"
            , "module Foo where"
            , "foo :: Int"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 1 16 1 16) "\n-- fooo"
      Rope.lines <$> new
        `shouldBe` Identity
          [ "abcdg"
          , "module Foo where"
          , "-- fooo"
          , "foo :: Int"
          ]

    -- ---------------------------------

    it "adds two lines" $ do
      -- based on vscode log
      let
        orig =
          unlines
            [ "module Foo where"
            , "foo = bb"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 1 8 1 8) "\n-- fooo\nfoo :: Int"
      Rope.lines <$> new
        `shouldBe` Identity
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
        orig =
          unlines
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
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 7 0 7 8) "baz ="
      Rope.lines <$> new
        `shouldBe` Identity
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
        orig =
          unlines
            [ "a𐐀b"
            , "a𐐀b"
            ]
        new = applyChange mempty (fromString orig) $ mkChangeEvent (J.mkRange 1 0 1 3) "𐐀𐐀"
      Rope.lines <$> new
        `shouldBe` Identity
          [ "a𐐀b"
          , "𐐀𐐀b"
          ]

  -- ---------------------------------

  describe "LSP utilities" $ do
    it "splits at a line" $ do
      let
        orig =
          unlines
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
        (left, right) = Rope.splitAtLine 4 (fromString orig)

      Rope.lines left
        `shouldBe` [ "module Foo where"
                   , "-- fooo"
                   , "foo :: Int"
                   , "foo = bb"
                   ]
      Rope.lines right
        `shouldBe` [ ""
                   , "bb = 5"
                   , ""
                   , "baz = do"
                   , "  putStrLn \"hello world\""
                   ]

    it "converts code units to code points" $ do
      let
        orig =
          unlines
            [ "a𐐀b"
            , "a𐐀b"
            ]
        vfile = VirtualFile 0 0 (fromString orig)

      positionToCodePointPosition vfile (J.Position 1 0) `shouldBe` Just (CodePointPosition 1 0)
      positionToCodePointPosition vfile (J.Position 1 1) `shouldBe` Just (CodePointPosition 1 1)
      -- Split inside code point
      positionToCodePointPosition vfile (J.Position 1 2) `shouldBe` Nothing
      positionToCodePointPosition vfile (J.Position 1 3) `shouldBe` Just (CodePointPosition 1 2)
      positionToCodePointPosition vfile (J.Position 1 4) `shouldBe` Just (CodePointPosition 1 3)
      positionToCodePointPosition vfile (J.Position 1 5) `shouldBe` Just (CodePointPosition 1 4)
      -- Greater column than max column
      positionToCodePointPosition vfile (J.Position 1 6) `shouldBe` Nothing
      positionToCodePointPosition vfile (J.Position 2 1) `shouldBe` Nothing
      -- Greater line than max line
      positionToCodePointPosition vfile (J.Position 3 0) `shouldBe` Nothing

    it "converts code points to code units" $ do
      let
        orig =
          unlines
            [ "a𐐀b"
            , "a𐐀b"
            ]
        vfile = VirtualFile 0 0 (fromString orig)

      codePointPositionToPosition vfile (CodePointPosition 1 0) `shouldBe` Just (J.Position 1 0)
      codePointPositionToPosition vfile (CodePointPosition 1 1) `shouldBe` Just (J.Position 1 1)
      codePointPositionToPosition vfile (CodePointPosition 1 2) `shouldBe` Just (J.Position 1 3)
      codePointPositionToPosition vfile (CodePointPosition 1 3) `shouldBe` Just (J.Position 1 4)
      codePointPositionToPosition vfile (CodePointPosition 1 4) `shouldBe` Just (J.Position 1 5)
      -- Greater column than max column
      codePointPositionToPosition vfile (CodePointPosition 1 5) `shouldBe` Nothing
      codePointPositionToPosition vfile (CodePointPosition 2 1) `shouldBe` Nothing
      -- Greater line than max line
      codePointPositionToPosition vfile (CodePointPosition 3 0) `shouldBe` Nothing
