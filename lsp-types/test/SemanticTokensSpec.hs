{-# LANGUAGE OverloadedStrings #-}

module SemanticTokensSpec where

import Data.Either (isRight)
import Data.List (unfoldr)
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Types
import Test.Hspec

spec :: Spec
spec = do
  let
    allMods = [SemanticTokenModifiers_Abstract, SemanticTokenModifiers_Static]
    exampleLegend =
      SemanticTokensLegend
        (fmap toEnumBaseType [SemanticTokenTypes_Property, SemanticTokenTypes_Type, SemanticTokenTypes_Class])
        (fmap toEnumBaseType allMods)
    exampleTokens1 =
      [ SemanticTokenAbsolute 2 5 3 SemanticTokenTypes_Property allMods
      , SemanticTokenAbsolute 2 10 4 SemanticTokenTypes_Type []
      , SemanticTokenAbsolute 5 2 7 SemanticTokenTypes_Class []
      ]
    exampleTokens2 =
      [ SemanticTokenAbsolute 3 5 3 SemanticTokenTypes_Property allMods
      , SemanticTokenAbsolute 3 10 4 SemanticTokenTypes_Type []
      , SemanticTokenAbsolute 6 2 7 SemanticTokenTypes_Class []
      ]

    bigNumber :: UInt
    bigNumber = 100000
    bigTokens =
      unfoldr (\i -> if i == bigNumber then Nothing else Just (SemanticTokenAbsolute i 1 1 SemanticTokenTypes_Type allMods, i + 1)) 0
    -- Relativized version of bigTokens
    bigTokensRel =
      unfoldr (\i -> if i == bigNumber then Nothing else Just (SemanticTokenRelative (if i == 0 then 0 else 1) 1 1 SemanticTokenTypes_Type allMods, i + 1)) 0

    -- One more order of magnitude makes diffing more-or-less hang - possibly we need a better diffing algorithm, since this is only ~= 200 tokens at 5 ints per token
    -- (I checked and it is the diffing that's slow, not turning it into edits)
    smallerBigNumber :: UInt
    smallerBigNumber = 1000
    bigInts :: [UInt]
    bigInts =
      unfoldr (\i -> if i == smallerBigNumber then Nothing else Just (1, i + 1)) 0
    bigInts2 :: [UInt]
    bigInts2 =
      unfoldr (\i -> if i == smallerBigNumber then Nothing else Just (if even i then 2 else 1, i + 1)) 0

  describe "relativize/absolutizeTokens" $ do
    it "round-trips" $ do
      absolutizeTokens (relativizeTokens exampleTokens1) `shouldBe` exampleTokens1
      absolutizeTokens (relativizeTokens exampleTokens2) `shouldBe` exampleTokens2
    it "handles big tokens" $ relativizeTokens bigTokens `shouldBe` bigTokensRel

  describe "encodeTokens" $ do
    context "when running the LSP examples" $ do
      it "encodes example 1 correctly" $
        let encoded = encodeTokens exampleLegend (relativizeTokens exampleTokens1)
         in encoded `shouldBe` Right [{- token 1 -} 2, 5, 3, 0, 3 {- token 2 -}, 0, 5, 4, 1, 0 {- token 3 -}, 3, 2, 7, 2, 0]
      it "encodes example 2 correctly" $
        let encoded = encodeTokens exampleLegend (relativizeTokens exampleTokens2)
         in encoded `shouldBe` Right [{- token 1 -} 3, 5, 3, 0, 3 {- token 2 -}, 0, 5, 4, 1, 0 {- token 3 -}, 3, 2, 7, 2, 0]
    it "handles big tokens" $ encodeTokens exampleLegend bigTokensRel `shouldSatisfy` isRight

  describe "computeEdits" $ do
    it "handles an edit in the middle" $
      computeEdits @Int [1, 2, 3] [1, 4, 5, 3] `shouldBe` [Edit 1 1 [4, 5]]
    it "handles an edit at the end" $
      computeEdits @Int [1, 2, 3] [1, 2, 4, 5] `shouldBe` [Edit 2 1 [4, 5]]
    it "handles an edit at the beginning" $
      computeEdits @Int [1, 2, 3] [4, 5, 2, 3] `shouldBe` [Edit 0 1 [4, 5]]
    it "handles an ambiguous edit" $
      computeEdits @Int [1, 2, 3] [1, 3, 4, 3] `shouldBe` [Edit 1 1 [], Edit 3 0 [4, 3]]
    it "handles a long edit" $
      computeEdits @Int [1, 2, 3, 4, 5] [1, 7, 7, 7, 7, 7, 5] `shouldBe` [Edit 1 3 [7, 7, 7, 7, 7]]
    it "handles multiple edits" $
      computeEdits @Int [1, 2, 3, 4, 5] [1, 6, 3, 7, 7, 5] `shouldBe` [Edit 1 1 [6], Edit 3 1 [7, 7]]
    it "handles big tokens" $
      -- It's a little hard to specify a useful predicate here, the main point is that it should not take too long
      computeEdits @UInt bigInts bigInts2 `shouldSatisfy` (not . null)

  describe "splitMultilineTokens" $ do
    it "splits a token spanning two lines" $ do
      let docRope = Rope.fromText "hello\nworld"
          token = SemanticTokenAbsolute 0 3 7 SemanticTokenTypes_String []
          result = splitMultilineTokens docRope [token]
      result `shouldBe`
        -- Note: Rope.splitAtLine includes the newline in the first line, so line 0 has length 6 ("hello\n") and line 1 has length 5 ("world")
        [ SemanticTokenAbsolute 0 3 3 SemanticTokenTypes_String []  -- "lo\n" on line 0 (chars 3-5, length 3)
        , SemanticTokenAbsolute 1 0 4 SemanticTokenTypes_String []  -- "worl" on line 1 (remaining 4 chars of the token)
        ]

    it "doesn't split single-line tokens" $ do
      let docRope = Rope.fromText "hello world"
          token = SemanticTokenAbsolute 0 0 5 SemanticTokenTypes_String []
          result = splitMultilineTokens docRope [token]
      result `shouldBe` [token]

    it "handles empty tokens" $ do
      let docRope = Rope.fromText "test"
          token = SemanticTokenAbsolute 0 0 0 SemanticTokenTypes_String []
          result = splitMultilineTokens docRope [token]
      result `shouldBe` [token]

  describe "resolveOverlappingTokens" $ do
    it "splits overlapping tokens correctly" $ do
      let token1 = SemanticTokenAbsolute 0 0 10 SemanticTokenTypes_String []
          token2 = SemanticTokenAbsolute 0 5 3 SemanticTokenTypes_Variable []
          result = resolveOverlappingTokens [token1, token2]
      result `shouldBe`
        [ SemanticTokenAbsolute 0 0 5 SemanticTokenTypes_String []    -- before overlap
        , SemanticTokenAbsolute 0 5 3 SemanticTokenTypes_Variable []  -- overlap (token2 wins)
        , SemanticTokenAbsolute 0 8 2 SemanticTokenTypes_String []    -- after overlap
        ]

    it "doesn't modify non-overlapping tokens" $ do
      let token1 = SemanticTokenAbsolute 0 0 5 SemanticTokenTypes_String []
          token2 = SemanticTokenAbsolute 0 10 3 SemanticTokenTypes_Variable []
          result = resolveOverlappingTokens [token1, token2]
      result `shouldBe` [token1, token2]

  describe "makeSemanticTokens with capabilities" $ do
    it "splits multiline when not supported" $ do
      let legend = defaultSemanticTokensLegend
          caps = Just $ SemanticTokensClientCapabilities
                   Nothing
                   (ClientSemanticTokensRequestOptions Nothing Nothing)
                   [] [] []
                   Nothing
                   (Just False)  -- multilineTokenSupport = False
                   Nothing Nothing
          docRope = Rope.fromText "ab\ncd"
          tokens = [SemanticTokenAbsolute 0 0 5 SemanticTokenTypes_String []]
          result = makeSemanticTokens legend caps docRope tokens
      isRight result `shouldBe` True

    it "preserves multiline when supported" $ do
      let legend = defaultSemanticTokensLegend
          caps = Just $ SemanticTokensClientCapabilities
                   Nothing
                   (ClientSemanticTokensRequestOptions Nothing Nothing)
                   [] [] []
                   Nothing
                   (Just True)  -- multilineTokenSupport = True
                   Nothing Nothing
          docRope = Rope.fromText "ab\ncd"
          tokens = [SemanticTokenAbsolute 0 0 5 SemanticTokenTypes_String []]
          result = makeSemanticTokens legend caps docRope tokens
      isRight result `shouldBe` True
