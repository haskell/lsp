module Language.LSP.Protocol.Types.Singletons where

import           Data.Aeson
import           Data.Hashable
import           Data.Proxy
import qualified Data.Text    as T
import           Control.DeepSeq
import           GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal,
                               symbolVal)

-- | A type whose only inhabitant is a single, statically-known string.
--
-- This corresponds to types like @"hello"@ in the LSP specification that
-- are exactly types with a single inhabitant.
data AString (s :: Symbol) where
  AString :: KnownSymbol s => AString s

instance Show (AString s) where
  show AString = symbolVal (Proxy @s)
instance Eq (AString s) where
  _ == _ = True
instance Ord (AString s) where
  compare _ _ = EQ
instance NFData (AString s) where
  rnf a = seq a ()
instance Hashable (AString sym) where
  hashWithSalt s AString = hashWithSalt s (symbolVal (Proxy @sym))

instance ToJSON (AString s) where
  toJSON AString = toJSON (T.pack (symbolVal (Proxy @s)))

instance KnownSymbol s => FromJSON (AString s) where
  parseJSON = withText "string literal type" $ \s -> do
    let sym = symbolVal (Proxy @s)
    if s == T.pack sym
    then pure AString
    else fail $ "wrong string, got: " <> show s <> " expected " <> sym

-- | A type whose only inhabitant is a single, statically-known integer.
--
-- This corresponds to types like @1@ in the LSP specification that
-- are exactly types with a single inhabitant.
data AnInteger (n :: Nat) where
  AnInteger :: KnownNat n => AnInteger n

instance Show (AnInteger n) where
  show AnInteger = show $ natVal (Proxy @n)
instance Eq (AnInteger n) where
  _ == _ = True
instance Ord (AnInteger n) where
  compare _ _ = EQ
instance NFData (AnInteger s) where
  rnf a = seq a ()
instance Hashable (AnInteger i) where
  hashWithSalt s AnInteger = hashWithSalt s (natVal (Proxy @i))

instance ToJSON (AnInteger n) where
  toJSON AnInteger = toJSON (natVal (Proxy @n))

instance KnownNat n => FromJSON (AnInteger n) where
  parseJSON = withScientific "integer literal type" $ \n -> do
    let nat = natVal (Proxy @n)
    if truncate n == nat
    then pure AnInteger
    else fail $ "wrong integer, got: " <> show n <> " expected " <> show nat
