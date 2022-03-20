{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Types.SMethodMap
  ( SMethodMap
  , singleton
  , insert
  , delete
  , member
  , lookup
  , map
  ) where

import Prelude hiding (lookup, map)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Exts (Int(..), dataToTag#, Any)
import Unsafe.Coerce (unsafeCoerce)

import Language.LSP.Types.Method (Method(..), SMethod(..))

-- This type exists to avoid a dependency on 'dependent-map'. It is less
-- safe (since we use 'unsafeCoerce') but much simpler and hence easier to include.
-- | A specialized alternative to a full dependent map for use with 'SMethod'.
data SMethodMap (v :: Method f t -> Type) =
  -- This works by using an 'IntMap' indexed by constructor tag for the majority
  -- of 'SMethod's, which have no parameters, and hence can only appear once as keys
  -- in the map. We do not attempt to be truly dependent here, and instead exploit
  -- 'usafeCoerce' to go to and from 'v Any'.
  -- The sole exception is 'SCustomMethod', for which we keep a separate map from
  -- its 'Text' parameter (and where we can get the type indices right).
  SMethodMap !(IntMap (v Any)) !(Map Text (v 'CustomMethod))

toIx :: SMethod a -> Int
toIx k = I# (dataToTag# k)

singleton :: SMethod a -> v a -> SMethodMap v
singleton (SCustomMethod t) v = SMethodMap mempty (Map.singleton t v)
singleton k v = SMethodMap (IntMap.singleton (toIx k) (unsafeCoerce v)) mempty

insert :: SMethod a -> v a -> SMethodMap v -> SMethodMap v
insert (SCustomMethod t) v (SMethodMap xs ys) = SMethodMap xs (Map.insert t v ys)
insert k v (SMethodMap xs ys) = SMethodMap (IntMap.insert (toIx k) (unsafeCoerce v) xs) ys

delete :: SMethod a -> SMethodMap v -> SMethodMap v
delete (SCustomMethod t) (SMethodMap xs ys) = SMethodMap xs (Map.delete t ys)
delete k (SMethodMap xs ys) = SMethodMap (IntMap.delete (toIx k) xs) ys

member :: SMethod a -> SMethodMap v -> Bool
member (SCustomMethod t) (SMethodMap _ ys) = Map.member t ys
member k (SMethodMap xs _) = IntMap.member (toIx k) xs

lookup :: SMethod a -> SMethodMap v -> Maybe (v a)
lookup (SCustomMethod t) (SMethodMap _ ys) = Map.lookup t ys
lookup k (SMethodMap xs _) = unsafeCoerce (IntMap.lookup (toIx k) xs)

map :: (forall a. u a -> v a) -> SMethodMap u -> SMethodMap v
map f (SMethodMap xs ys) = SMethodMap (IntMap.map f xs) (Map.map f ys)

instance Semigroup (SMethodMap v) where
  SMethodMap xs ys <> SMethodMap xs' ys' = SMethodMap (xs <> xs') (ys <> ys')

instance Monoid (SMethodMap v) where
  mempty = SMethodMap mempty mempty
