{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}

module Data.IxMap where

import Data.Kind
import Data.Map.Strict qualified as M
import Data.Some
import Unsafe.Coerce

-- a `compare` b <=> toBase a `compare` toBase b
-- toBase (i :: f a) == toBase (j :: f b) <=> a ~ b
class Ord (Base f) => IxOrd f where
  type Base f
  toBase :: forall a. f a -> Base f

newtype IxMap (k :: a -> Type) (f :: a -> Type) = IxMap {getMap :: M.Map (Base k) (Some f)}

emptyIxMap :: IxMap k f
emptyIxMap = IxMap M.empty

insertIxMap :: IxOrd k => k m -> f m -> IxMap k f -> Maybe (IxMap k f)
insertIxMap (toBase -> i) x (IxMap m)
  | M.notMember i m = Just $ IxMap $ M.insert i (mkSome x) m
  | otherwise = Nothing

lookupIxMap :: IxOrd k => k m -> IxMap k f -> Maybe (f m)
lookupIxMap i (IxMap m) =
  case M.lookup (toBase i) m of
    Just (Some v) -> Just $ unsafeCoerce v
    Nothing -> Nothing

pickFromIxMap :: IxOrd k => k m -> IxMap k f -> (Maybe (f m), IxMap k f)
pickFromIxMap i (IxMap m) =
  case M.updateLookupWithKey (\_ _ -> Nothing) (toBase i) m of
    (Nothing, !m') -> (Nothing, IxMap m')
    (Just (Some k), !m') -> (Just (unsafeCoerce k), IxMap m')
