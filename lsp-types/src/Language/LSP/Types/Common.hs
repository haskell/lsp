{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Common types that aren't in the specification, but which get widely used.
-}
module Language.LSP.Types.Common (
    type (|?) (..)
  , _L
  , _R
  , toEither
  , MessageDirection (..)
  , MessageKind (..)
  , SMessageDirection (..)
  , SMessageKind (..)
  , AString (..)
  , AnInteger (..)
  , Int32
  , UInt
  , Null (..)
  , absorbNull
  , nullToMaybe
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson          hiding (Null)
import qualified Data.Aeson          as J
import           Data.Hashable
import           Data.Int            (Int32)
import           Data.Mod.Word
import           Data.Proxy
import qualified Data.Text           as T
import           GHC.Generics        hiding (UInt)
import           GHC.TypeLits        hiding (Mod)
import           Text.Read           (Read (readPrec))

-- | Which direction messages are sent in.
data MessageDirection = ServerToClient | ClientToServer
-- | What kind of message is sent.
data MessageKind = Notification | Request

-- | Singleton type for 'MessageDirection'.
data SMessageDirection (f :: MessageDirection) where
  SClientToServer :: SMessageDirection ClientToServer
  SServerToClient :: SMessageDirection ServerToClient
  SBothDirections :: SMessageDirection f

-- | Singleton type for 'MessageKind'.
data SMessageKind (f :: MessageKind) where
  SNotification :: SMessageKind Notification
  SRequest :: SMessageKind Request
  SBothTypes :: SMessageKind f

-- | A type whose only inhabitant is a single, statically-known string.
data AString (s :: Symbol) where
  AString :: KnownSymbol s => AString s

instance Show (AString s) where
  show AString = symbolVal (Proxy @s)
instance Eq (AString s) where
  _ == _ = True
instance Ord (AString s) where
  compare _ _ = EQ

instance ToJSON (AString s) where
  toJSON AString = toJSON (T.pack (symbolVal (Proxy @s)))

instance KnownSymbol s => FromJSON (AString s) where
  parseJSON = withText "string literal type" $ \s -> do
    let sym = symbolVal (Proxy @s)
    if s == T.pack sym
    then pure AString
    else fail $ "wrong string, got: " <> show s <> " expected " <> sym

-- | A type whose only inhabitant is a single, statically-known integer.
data AnInteger (n :: Nat) where
  AnInteger :: KnownNat n => AnInteger n

instance Show (AnInteger n) where
  show AnInteger = show $ natVal (Proxy @n)
instance Eq (AnInteger n) where
  _ == _ = True
instance Ord (AnInteger n) where
  compare _ _ = EQ

instance ToJSON (AnInteger n) where
  toJSON AnInteger = toJSON (natVal (Proxy @n))

instance KnownNat n => FromJSON (AnInteger n) where
  parseJSON = withScientific "string literal type" $ \n -> do
    let nat = natVal (Proxy @n)
    if truncate n == nat
    then pure AnInteger
    else fail $ "wrong string, got: " <> show n <> " expected " <> show nat

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

-- | An untagged union, isomorphic to 'Either'.
data a |? b = InL a
            | InR b
  deriving (Read,Show,Eq,Ord,Generic)
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
  -- the more complex types tend to appear on the right of the |, i.e.
  -- @colorProvider?: boolean | DocumentColorOptions | DocumentColorRegistrationOptions;@
  parseJSON v = InR <$> parseJSON v <|> InL <$> parseJSON v

instance (NFData a, NFData b) => NFData (a |? b)

-- We could use 'Proxy' for this, as aeson also serializes it to/from null,
-- but this is more explicit.
-- | A type for things that should just literally be null and nothing else.
data Null = Null deriving (Eq,Ord,Show)

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
