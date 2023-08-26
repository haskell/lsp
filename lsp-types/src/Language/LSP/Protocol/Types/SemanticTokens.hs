{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.LSP.Protocol.Types.SemanticTokens where

import Data.Text (Text)

import Control.Monad.Except

import Language.LSP.Protocol.Internal.Types.SemanticTokenModifiers
import Language.LSP.Protocol.Internal.Types.SemanticTokenTypes
import Language.LSP.Protocol.Internal.Types.SemanticTokens
import Language.LSP.Protocol.Internal.Types.SemanticTokensDelta
import Language.LSP.Protocol.Internal.Types.SemanticTokensEdit
import Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import Language.LSP.Protocol.Types.Common
import Language.LSP.Protocol.Types.LspEnum

import Data.Algorithm.Diff qualified as Diff
import Data.Bits qualified as Bits
import Data.DList qualified as DList
import Data.Foldable hiding (
  length,
 )
import Data.Map qualified as Map
import Data.Maybe (
  fromMaybe,
  maybeToList,
 )
import Data.String

defaultSemanticTokensLegend :: SemanticTokensLegend
defaultSemanticTokensLegend =
  SemanticTokensLegend
    (fmap toEnumBaseType . toList $ knownValues @SemanticTokenTypes)
    (fmap toEnumBaseType . toList $ knownValues @SemanticTokenModifiers)

----------------------------------------------------------
-- Tools for working with semantic tokens.
----------------------------------------------------------

{- | A single 'semantic token' as described in the LSP specification, using absolute positions.
 This is the kind of token that is usually easiest for editors to produce.
-}
data SemanticTokenAbsolute = SemanticTokenAbsolute
  { _line :: UInt
  , _startChar :: UInt
  , _length :: UInt
  , _tokenType :: SemanticTokenTypes
  , _tokenModifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord)

-- Note: we want the Ord instance to sort the tokens textually: this is achieved due to the
-- order of the constructors

-- | A single 'semantic token' as described in the LSP specification, using relative positions.
data SemanticTokenRelative = SemanticTokenRelative
  { _deltaLine :: UInt
  , _deltaStartChar :: UInt
  , _length :: UInt
  , _tokenType :: SemanticTokenTypes
  , _tokenModifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord)

-- Note: we want the Ord instance to sort the tokens textually: this is achieved due to the
-- order of the constructors

{- | Turn a list of absolutely-positioned tokens into a list of relatively-positioned tokens. The tokens are assumed to be in the
 order that they appear in the document!
-}
relativizeTokens :: [SemanticTokenAbsolute] -> [SemanticTokenRelative]
relativizeTokens xs = DList.toList $ go 0 0 xs mempty
 where
  -- Pass an accumulator to make this tail-recursive
  go :: UInt -> UInt -> [SemanticTokenAbsolute] -> DList.DList SemanticTokenRelative -> DList.DList SemanticTokenRelative
  go _ _ [] acc = acc
  go lastLine lastChar (SemanticTokenAbsolute l c len ty mods : ts) acc =
    let
      lastCharInLine = if l == lastLine then lastChar else 0
      dl = l - lastLine
      dc = c - lastCharInLine
     in
      go l c ts (DList.snoc acc (SemanticTokenRelative dl dc len ty mods))

{- | Turn a list of relatively-positioned tokens into a list of absolutely-positioned tokens. The tokens are assumed to be in the
 order that they appear in the document!
-}
absolutizeTokens :: [SemanticTokenRelative] -> [SemanticTokenAbsolute]
absolutizeTokens xs = DList.toList $ go 0 0 xs mempty
 where
  -- Pass an accumulator to make this tail-recursive
  go :: UInt -> UInt -> [SemanticTokenRelative] -> DList.DList SemanticTokenAbsolute -> DList.DList SemanticTokenAbsolute
  go _ _ [] acc = acc
  go lastLine lastChar (SemanticTokenRelative dl dc len ty mods : ts) acc =
    let
      lastCharInLine = if dl == 0 then lastChar else 0
      l = lastLine + dl
      c = lastCharInLine + dc
     in
      go l c ts (DList.snoc acc (SemanticTokenAbsolute l c len ty mods))

-- | Encode a series of relatively-positioned semantic tokens into an integer array following the given legend.
encodeTokens :: SemanticTokensLegend -> [SemanticTokenRelative] -> Either Text [UInt]
encodeTokens SemanticTokensLegend{_tokenTypes = tts, _tokenModifiers = tms} sts =
  DList.toList . DList.concat <$> traverse encodeToken sts
 where
  -- Note that there's no "fast" version of these (e.g. backed by an IntMap or similar)
  -- in general, due to the possibility  of unknown token types which are only identified by strings.
  tyMap :: Map.Map SemanticTokenTypes UInt
  tyMap = Map.fromList $ zip (fmap fromOpenEnumBaseType tts) [0 ..]
  modMap :: Map.Map SemanticTokenModifiers Int
  modMap = Map.fromList $ zip (fmap fromOpenEnumBaseType tms) [0 ..]

  lookupTy :: SemanticTokenTypes -> Either Text UInt
  lookupTy ty = case Map.lookup ty tyMap of
    Just tycode -> pure tycode
    Nothing -> throwError $ "Semantic token type " <> fromString (show ty) <> " did not appear in the legend"
  lookupMod :: SemanticTokenModifiers -> Either Text Int
  lookupMod modifier = case Map.lookup modifier modMap of
    Just modcode -> pure modcode
    Nothing -> throwError $ "Semantic token modifier " <> fromString (show modifier) <> " did not appear in the legend"

  -- Use a DList here for better efficiency when concatenating all these together
  encodeToken :: SemanticTokenRelative -> Either Text (DList.DList UInt)
  encodeToken (SemanticTokenRelative dl dc len ty mods) = do
    tycode <- lookupTy ty
    modcodes <- traverse lookupMod mods
    let combinedModcode :: Int = foldl' Bits.setBit Bits.zeroBits modcodes

    pure [dl, dc, len, tycode, fromIntegral combinedModcode]

-- This is basically 'SemanticTokensEdit', but slightly easier to work with.

-- | An edit to a buffer of items.
data Edit a = Edit {editStart :: UInt, editDeleteCount :: UInt, editInsertions :: [a]}
  deriving stock (Read, Show, Eq, Ord)

-- | Compute a list of edits that will turn the first list into the second list.
computeEdits :: Eq a => [a] -> [a] -> [Edit a]
computeEdits l r = DList.toList $ go 0 Nothing (Diff.getGroupedDiff l r) mempty
 where
  {-
  Strategy: traverse the list of diffs, keeping the current index and (maybe) an in-progress 'Edit'.
  Whenever we see a 'Diff' that's only one side or the other, we can bundle that in to our in-progress
  'Edit'. We only have to stop if we see a 'Diff' that's on both sides (i.e. unchanged), then we
  dump the 'Edit' into the accumulator.
  We need the index, because 'Edit's need to say where they start.
  -}
  go :: UInt -> Maybe (Edit a) -> [Diff.Diff [a]] -> DList.DList (Edit a) -> DList.DList (Edit a)
  -- No more diffs: append the current edit if there is one and return
  go _ e [] acc = acc <> DList.fromList (maybeToList e)
  -- Items only on the left (i.e. deletions): increment the current index, and record the count of deletions,
  -- starting a new edit if necessary.
  go ix e (Diff.First ds : rest) acc =
    let
      deleteCount = fromIntegral $ Prelude.length ds
      edit = fromMaybe (Edit ix 0 []) e
     in
      go (ix + deleteCount) (Just (edit{editDeleteCount = editDeleteCount edit + deleteCount})) rest acc
  -- Items only on the right (i.e. insertions): don't increment the current index, and record the insertions,
  -- starting a new edit if necessary.
  go ix e (Diff.Second as : rest) acc =
    let edit = fromMaybe (Edit ix 0 []) e
     in go ix (Just (edit{editInsertions = editInsertions edit <> as})) rest acc
  -- Items on both sides: increment the current index appropriately (since the items appear on the left),
  -- and append the current edit (if there is one) to our list of edits (since we can't continue it with a break).
  go ix e (Diff.Both bs _bs : rest) acc =
    let bothCount = fromIntegral $ Prelude.length bs
     in go (ix + bothCount) Nothing rest (acc <> DList.fromList (maybeToList e))

-- | Convenience method for making a 'SemanticTokens' from a list of 'SemanticTokenAbsolute's. An error may be returned if

-- The resulting 'SemanticTokens' lacks a result ID, which must be set separately if you are using that.
makeSemanticTokens :: SemanticTokensLegend -> [SemanticTokenAbsolute] -> Either Text SemanticTokens
makeSemanticTokens legend sts = do
  encoded <- encodeTokens legend $ relativizeTokens sts
  pure $ SemanticTokens Nothing encoded

{- | Convenience function for making a 'SemanticTokensDelta' from a previous and current 'SemanticTokens'.
 The resulting 'SemanticTokensDelta' lacks a result ID, which must be set separately if you are using that.
-}
makeSemanticTokensDelta :: SemanticTokens -> SemanticTokens -> SemanticTokensDelta
makeSemanticTokensDelta SemanticTokens{_data_ = prevTokens} SemanticTokens{_data_ = curTokens} =
  let edits = computeEdits prevTokens curTokens
      stEdits = fmap (\(Edit s ds as) -> SemanticTokensEdit s ds (Just as)) edits
   in SemanticTokensDelta Nothing stEdits
