{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
This module defines orphan `aeson` instances for `Data.Row`.
They differ from the instances in `row-types-aeson` in one crucial respect: they
serialise `Nothing` fields by *omitting* them in the resulting object, and parse absent fields as `Nothing`.
`aeson` can be configured to have this behviour for instances for datatypes, but we want to do this
for record types generically.

This is crucial to match what LSP clients expect.
-}
module Data.Row.Aeson where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.List (intercalate)

import Data.Row
import Data.Row.Internal
import Data.Row.Records qualified as Rec

import Data.Bifunctor (second)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Proxy
import Data.String

import Language.LSP.Protocol.Types.Common ((.:!?))

-- `aeson` does not need such a typeclass because it generates code per-instance
-- that handles this, whereas we want to work generically.

{- | Serialise a value as an entry in a JSON object. This allows customizing the
 behaviour in the object context, in order to e.g. omit the field.
-}
class ToJSONEntry a where
  -- We use String so we can use fromString on it to get a key that works
  -- in both aeson-1 and aeson-2
  toJSONEntry :: String -> a -> Object

instance {-# OVERLAPPING #-} ToJSON a => ToJSONEntry (Maybe a) where
  -- Omit Nothing fields
  toJSONEntry _ Nothing = mempty
  toJSONEntry k v = fromString k .= toJSON v

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSONEntry a where
  toJSONEntry k v = fromString k .= toJSON v

class FromJSONEntry a where
  parseJSONEntry :: Object -> String -> Parser a

instance {-# OVERLAPPING #-} FromJSON a => FromJSONEntry (Maybe a) where
  -- Parse Nothing fields as optional, accepting Null to mean "missing"
  parseJSONEntry o k = o .:!? (fromString k)

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSONEntry a where
  parseJSONEntry o k = o .: (fromString k)

------

instance Forall r ToJSONEntry => ToJSON (Rec r) where
  -- Sadly, there appears to be no helper we can use that gives us access to the keys, so I just used metamorph directly
  -- adapted from 'eraseWithLabels'
  toJSON rc = Object $ getConst $ metamorph @_ @r @ToJSONEntry @(,) @Rec @(Const Object) @Identity Proxy doNil doUncons doCons rc
   where
    doNil :: Rec Empty -> Const Object Empty
    doNil _ = Const mempty
    doUncons ::
      forall l r'.
      (KnownSymbol l) =>
      Label l ->
      Rec r' ->
      (Rec (r' .- l), Identity (r' .! l))
    doUncons l = second Identity . lazyUncons l
    doCons ::
      forall l t r'.
      (KnownSymbol l, ToJSONEntry t) =>
      Label l ->
      (Const Object r', Identity t) ->
      Const Object (Extend l t r')
    doCons l (Const c, Identity x) = Const $ toJSONEntry (show' l) x <> c

instance (AllUniqueLabels r, Forall r FromJSONEntry) => FromJSON (Rec r) where
  parseJSON (Object o) = do
    r <- Rec.fromLabelsA @FromJSONEntry $ \l -> do
      x <- parseJSONEntry o (fromString $ show l)
      x `seq` pure x
    r `seq` pure r
  parseJSON v = typeMismatch msg v
   where
    msg = "REC: {" ++ intercalate "," (labels @r @FromJSONEntry) ++ "}"

--- Copied from the library, as it's private

lazyUncons :: KnownSymbol l => Label l -> Rec r -> (Rec (r .- l), r .! l)
lazyUncons l r = (Rec.lazyRemove l r, r .! l)
