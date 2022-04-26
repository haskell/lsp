{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeOperators              #-}

-- | Common types that aren't in the specification
module Language.LSP.Types.Common (
    type (|?) (..)
  , toEither
  , List (..)
  , MaybeN (..)
  , unMaybeN
  , maybeN
  , Empty (..)
  , Int32
  , UInt ) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Hashable
import Data.Int (Int32)
import Data.Mod.Word
import GHC.Generics hiding (UInt)
import GHC.TypeNats hiding (Mod)
import Text.Read (Read(readPrec))

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

-- | A terser, isomorphic data type for 'Either', that does not get tagged when
-- converting to and from JSON.
data a |? b = InL a
            | InR b
  deriving (Read,Show,Eq,Ord,Generic)
infixr |?

toEither :: a |? b -> Either a b
toEither (InL a) = Left a
toEither (InR b) = Right b

instance (ToJSON a, ToJSON b) => ToJSON (a |? b) where
  toJSON (InL x) = toJSON x
  toJSON (InR x) = toJSON x

instance (FromJSON a, FromJSON b) => FromJSON (a |? b) where
  -- Important: Try to parse the **rightmost** type first, as in the specification
  -- the more complex types tend to appear on the right of the |, i.e.
  -- @colorProvider?: boolean | DocumentColorOptions | DocumentColorRegistrationOptions;@
  parseJSON v = InR <$> parseJSON v <|> InL <$> parseJSON v

instance (NFData a, NFData b) => NFData (a |? b)

-- | All LSP types representing a list **must** use this type rather than '[]'.
-- In particular this is necessary to change the 'FromJSON' instance to be compatible
-- with Elisp (where empty lists show up as 'null')
newtype List a = List [a]
    deriving stock (Traversable,Generic)
    deriving newtype (Show,Read,Eq,Ord,Semigroup,Monoid,Functor,Foldable)

instance NFData a => NFData (List a)

instance (ToJSON a) => ToJSON (List a) where
  toJSON (List ls) = toJSON ls

instance (FromJSON a) => FromJSON (List a) where
  parseJSON Null = return (List [])
  parseJSON v      = List <$> parseJSON v

data Empty = Empty deriving (Eq,Ord,Show)
instance ToJSON Empty where
  toJSON Empty = Null
instance FromJSON Empty where
  parseJSON Null = pure Empty
  parseJSON (Object o) | o == mempty = pure Empty
  parseJSON _ = fail "expected 'null' or '{}'"

-- A Maybe type which encodes to JSON "null" in the case of Nothing.
-- Used to conform to the LSP spec in cases where a field is specified to be
-- of type "a | null" instead of "a?". (In the latter case we just use ordinary
-- Maybe in combination with aeson's 'omitNothingFields' option.)
data MaybeN a = JustN a | NothingN
  deriving (Show, Read, Eq, Ord, Functor)
instance ToJSON a => ToJSON (MaybeN a) where
  toJSON NothingN = Null
  toJSON (JustN x) = toJSON x
instance FromJSON a => FromJSON (MaybeN a) where
  parseJSON Null = pure NothingN
  parseJSON x = JustN <$> parseJSON x

unMaybeN :: MaybeN a -> Maybe a
unMaybeN (JustN a) = Just a
unMaybeN NothingN = Nothing

maybeN :: Maybe a -> MaybeN a
maybeN (Just a) = JustN a
maybeN Nothing = NothingN
