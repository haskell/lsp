{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.IxMap where

import qualified Data.Map as M
import Data.Some
import Data.Kind
import Unsafe.Coerce

-- a `compare` b <=> toBase a `compare` toBase b
-- toBase (i :: f a) == toBase (j :: f b) <=> a ~ b
class Ord (Base f) => IxOrd f where
  type Base f
  toBase :: forall a. f a -> Base f

newtype IxMap (k :: a -> Type) (f :: a -> Type) = IxMap { getMap :: M.Map (Base k) (Some f) }

emptyIxMap :: IxMap k f
emptyIxMap = IxMap M.empty

insertIxMap :: IxOrd k => k m -> f m -> IxMap k f -> IxMap k f
insertIxMap i x (IxMap m) = IxMap $ M.insert (toBase i) (mkSome x) m

lookupIxMap :: IxOrd k => k m -> IxMap k f -> Maybe (f m)
lookupIxMap i (IxMap m) =
  case M.lookup (toBase i) m of
    Just (Some v) -> Just $ unsafeCoerce v
    Nothing -> Nothing

pickFromIxMap :: IxOrd k => k m -> IxMap k f -> (Maybe (f m), IxMap k f)
pickFromIxMap i = pickFromIxMap' (toBase i)

pickFromIxMap' :: IxOrd k => Base k -> IxMap k f -> (Maybe (f m), IxMap k f)
pickFromIxMap' i (IxMap m) =
  case M.updateLookupWithKey (\_ _ -> Nothing) i m of
    (Nothing,m) -> (Nothing,IxMap m)
    (Just (Some k),m) -> (Just (unsafeCoerce k),IxMap m)
