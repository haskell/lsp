{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}

-- | Common types that aren't in the specification
module Language.LSP.Types.Common (
    type (|?) (..)
  , toEither
  , List (..)
  , Empty (..)
  , Int32
  , Word32 ) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Int (Int32)
import Data.Word (Word32)
import GHC.Generics

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
                deriving (Show,Read,Eq,Ord,Semigroup,Monoid,Functor,Foldable,Traversable,Generic)

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
