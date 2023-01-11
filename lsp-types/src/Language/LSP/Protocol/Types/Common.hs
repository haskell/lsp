{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeOperators              #-}

-- | Common types that aren't in the specification
module Language.LSP.Protocol.Types.Common (
    type (|?) (..)
  , toEither
  , _L
  , _R
  , Int32
  , UInt
  , Null (..)
  , absorbNull
  , nullToMaybe
  , (.=?)
) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson          hiding (Null)
import qualified Data.Aeson          as J
import           Data.Hashable
import           Data.Int            (Int32)
import           Data.Mod.Word
import           GHC.Generics        hiding (UInt)
import           GHC.TypeNats        hiding (Mod)
import           Text.Read           (Read (readPrec))

-- | The "uinteger" type in the LSP spec.
--
-- Unusually, this is a **31**-bit unsigned integer, not a 32-bit one.
newtype UInt = UInt (Mod (2^31))
  deriving newtype (Num, Bounded, Enum, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Hashable UInt where hashWithSalt s (UInt n) = hashWithSalt s (unMod n)

instance Show UInt where
  show (UInt u) = show $ unMod u

instance Read UInt where
  readPrec = fromInteger <$> readPrec

instance Real UInt where
  toRational (UInt u) = toRational $ unMod u

instance Integral UInt where
  quotRem (UInt x) (UInt y) = bimap fromIntegral fromIntegral $ quotRem (unMod x) (unMod y)
  toInteger (UInt u) = toInteger $ unMod u

instance ToJSON UInt where
  toJSON u = toJSON (toInteger u)

instance FromJSON UInt where
  parseJSON v = fromInteger <$> parseJSON v

-- | An alternative type (isomorphic to 'Either'), but which
-- is encoded into JSON without a tag for the alternative.
--
-- This corresponds to @a | b@ types in the LSP specification.
data a |? b = InL a
            | InR b
  deriving stock (Read,Show,Eq,Ord,Generic)
infixr |?

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

instance (FromJSON a, FromJSON b) => FromJSON (a |? b) where
  -- Important: Try to parse the **rightmost** type first, as in the specification
  -- the more complex types tend to appear on the right of the |, e.g.
  -- @colorProvider?: boolean | DocumentColorOptions | DocumentColorRegistrationOptions;@
  parseJSON v = InR <$> parseJSON v <|> InL <$> parseJSON v

instance (NFData a, NFData b) => NFData (a |? b)

-- We could use 'Proxy' for this, as aeson also serializes it to/from null,
-- but this is more explicit.
-- | A type for that is precisely null and nothing else.
--
-- This is useful since the LSP specification often includes types like @a | null@
-- as distinct from an optional value of type @a@.
data Null = Null deriving stock (Eq,Ord,Show)

instance ToJSON Null where
  toJSON Null = J.Null
instance FromJSON Null where
  parseJSON J.Null = pure Null
  parseJSON _      = fail "expected 'null'"

absorbNull :: Monoid a => a |? Null -> a
absorbNull (InL a) = a
absorbNull (InR _) = mempty

nullToMaybe :: a |? Null -> Maybe a
nullToMaybe (InL a) = Just a
nullToMaybe (InR _) = Nothing

-- | Include a value in an JSON object optionally, omitting it if it is 'Nothing'.
(.=?) :: (J.KeyValue kv, J.ToJSON v) => J.Key -> Maybe v -> [kv]
k .=? v = case v of
  Just v' -> [k J..= v']
  Nothing -> mempty
