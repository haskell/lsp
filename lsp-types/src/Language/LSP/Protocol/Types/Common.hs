{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | Common types that aren't in the specification
module Language.LSP.Protocol.Types.Common (
  type (|?) (..),
  toEither,
  _L,
  _R,
  Int32,
  UInt,
  Null (..),
  absorbNull,
  nullToMaybe,
  maybeToNull,
  (.=?),
  (.:!?),
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as J
import Data.Hashable
import Data.Int (Int32)
import Data.Mod.Word
import Data.Set as Set
import Data.String (fromString)
import GHC.Generics hiding (UInt)
import GHC.TypeNats hiding (Mod)
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import Text.Read (Read (readPrec))

{- | The "uinteger" type in the LSP spec.

 Unusually, this is a **31**-bit unsigned integer, not a 32-bit one.
-}
newtype UInt = UInt (Mod (2 ^ 31))
  deriving newtype (Num, Bounded, Enum, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Hashable UInt where hashWithSalt s (UInt n) = hashWithSalt s (unMod n)

instance Show UInt where
  show (UInt u) = show $ unMod u

instance Read UInt where
  readPrec = fromInteger <$> readPrec

instance Pretty UInt where
  pretty = viaShow

instance Real UInt where
  toRational (UInt u) = toRational $ unMod u

instance Integral UInt where
  quotRem (UInt x) (UInt y) = bimap fromIntegral fromIntegral $ quotRem (unMod x) (unMod y)
  toInteger (UInt u) = toInteger $ unMod u

instance ToJSON UInt where
  toJSON u = toJSON (toInteger u)

instance FromJSON UInt where
  parseJSON v = fromInteger <$> parseJSON v

{- | An alternative type (isomorphic to 'Either'), but which
 is encoded into JSON without a tag for the alternative.

 This corresponds to @a | b@ types in the LSP specification.
-}
data a |? b
  = InL a
  | InR b
  deriving stock (Read, Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (Pretty) via (ViaJSON (a |? b))

infixr 9 |?

-- | Prism for the left-hand side of an '(|?)'.
_L :: Prism' (a |? b) a
_L = prism' InL $ \case
  InL a -> Just a
  InR _ -> Nothing

-- | Prism for the right-hand side of an '(|?)'.
_R :: Prism' (a |? b) b
_R = prism' InR $ \case
  InL _ -> Nothing
  InR b -> Just b

toEither :: a |? b -> Either a b
toEither (InL a) = Left a
toEither (InR b) = Right b

instance (ToJSON a, ToJSON b) => ToJSON (a |? b) where
  toJSON (InL x) = toJSON x
  toJSON (InR x) = toJSON x

instance (FromJSON a, ToJSON a, FromJSON b, ToJSON b) => FromJSON (a |? b) where
  -- Truly atrocious and abominable hack. The issue is tha we may have siutations
  -- where some input JSON can parse correctly as both sides of the union, because
  -- we have no tag. What do we do in this situation? It's very unclear, and the
  -- spec is no help. The heuristic we adopt here is that it is better to take
  -- the version with "more fields". How do we work that out? By converting back
  -- to JSON and looking at the object fields.
  --
  -- Possibly we could do better by relying on Generic instances for a and b
  -- in order to work out which has more fields on the Haskell side.
  parseJSON v = do
    let ra :: Result a = fromJSON v
        rb :: Result b = fromJSON v
    case (ra, rb) of
      (Success a, Error _) -> pure $ InL a
      (Error _, Success b) -> pure $ InR b
      (Error e, Error _) -> fail e
      (Success a, Success b) -> case (toJSON a, toJSON b) of
        -- Both sides encode to the same thing, just pick one arbitrarily
        (l, r) | l == r -> pure $ InL a
        (Object oa, Object ob) ->
          let ka = Set.fromList $ KM.keys oa
              kb = Set.fromList $ KM.keys ob
           in if kb `Set.isSubsetOf` ka
                then pure $ InL a
                else
                  if ka `Set.isSubsetOf` kb
                    then pure $ InR b
                    else fail $ "Could not decide which type of value to produce, left encodes to an object with keys: " ++ show ka ++ "; right has keys " ++ show kb
        (l, r) -> fail $ "Could not decide which type of value to produce, left encodes to: " ++ show l ++ "; right encodes to: " ++ show r

-- We could use 'Proxy' for this, as aeson also serializes it to/from null,
-- but this is more explicit.

{- | A type for that is precisely null and nothing else.

 This is useful since the LSP specification often includes types like @a | null@
 as distinct from an optional value of type @a@.
-}
data Null = Null
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (Pretty) via (ViaJSON Null)

instance ToJSON Null where
  toJSON Null = J.Null
instance FromJSON Null where
  parseJSON J.Null = pure Null
  parseJSON _ = fail "expected 'null'"

absorbNull :: Monoid a => a |? Null -> a
absorbNull (InL a) = a
absorbNull (InR _) = mempty

nullToMaybe :: a |? Null -> Maybe a
nullToMaybe (InL a) = Just a
nullToMaybe (InR _) = Nothing

maybeToNull :: Maybe a -> a |? Null
maybeToNull (Just x) = InL x
maybeToNull Nothing = InR Null

-- This is equivalent to the instance for 'Maybe s'
instance Semigroup s => Semigroup (s |? Null) where
  InL x <> InL y = InL (x <> y)
  InL x <> InR _ = InL x
  InR _ <> InL x = InL x
  InR _ <> InR y = InR y

-- We use String so we can use fromString on it to get a key that works
-- in both aeson-1 and aeson-2

-- | Include a value in an JSON object optionally, omitting it if it is 'Nothing'.
#if MIN_VERSION_aeson(2,2,0)
(.=?) :: (J.KeyValue e kv, J.ToJSON v) => String -> Maybe v -> [kv]
#else
(.=?) :: (J.KeyValue kv, J.ToJSON v) => String -> Maybe v -> [kv]
#endif
k .=? v = case v of
  Just v' -> [fromString k J..= v']
  Nothing -> mempty

{- |
Parse a value optionally. This behaves similarly to 'J..:!' and
'J..:?', but differs in how it handles 'Null':

    * If 'Null' can be converted to the desired type...
        * 'J.:?': the result is success with 'Nothing'
        * 'J.:!': the result is success with 'Just <value>'
        * '.:!?': the result is success with 'Just <value>'
    * If 'Null' cannot be converted to the desired type...
        * 'J.:?': the result is success with 'Nothing'
        * 'J.:!': the result is failure
        * '.:!?': the result is success with 'Nothing'

That is, we allow 'Null' to mean either 'Nothing' or 'Just <value>',
with the latter taking priority.
-}
(.:!?) :: (J.FromJSON v) => Object -> Key -> J.Parser (Maybe v)
o .:!? k =
  -- If 'Null' can be converted to the desired type this succeeds
  -- with Just the converted value
  o J..:! k
    -- otherwise...
    <|>
    -- If 'Null' cannot be converted to the desired type this succeeds
    -- with Nothing
    o J..:? k
