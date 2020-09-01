{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}

-- | Common types that aren't in the specification
module Language.Haskell.LSP.Types.Common where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import GHC.Generics

-- | A terser, isomorphic data type for 'Either', that does not get tagged when
-- converting to and from JSON.
data a |? b = L a
            | R b
  deriving (Read,Show,Eq,Ord,Generic)
infixr |?

instance (ToJSON a, ToJSON b) => ToJSON (a |? b) where
  toJSON (L x) = toJSON x
  toJSON (R x) = toJSON x

instance (FromJSON a, FromJSON b) => FromJSON (a |? b) where
  -- Important: Try to parse the **rightmost** type first, as in the specification
  -- the more complex types tend to appear on the right of the |, i.e.
  -- @colorProvider?: boolean | DocumentColorOptions | DocumentColorRegistrationOptions;@
  parseJSON v = R <$> parseJSON v <|> L <$> parseJSON v

instance (NFData a, NFData b) => NFData (a |? b)

-- | This data type is used to host a FromJSON instance for the encoding used by
-- elisp, where an empty list shows up as "null"
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
  parseJSON _ = mempty
