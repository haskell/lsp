{-# LANGUAGE CPP                        #-}
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
  deriving (Read,Show,Eq)
infixr |?

instance (ToJSON a, ToJSON b) => ToJSON (a |? b) where
  toJSON (L x) = toJSON x
  toJSON (R x) = toJSON x

instance (FromJSON a, FromJSON b) => FromJSON (a |? b) where
  parseJSON v = L <$> parseJSON v <|> R <$> parseJSON v

-- | This data type is used to host a FromJSON instance for the encoding used by
-- elisp, where an empty list shows up as "null"
newtype List a = List [a]
                deriving (Show,Read,Eq,Ord,Monoid,Functor,Foldable,Traversable,Generic)

instance NFData a => NFData (List a)

instance (ToJSON a) => ToJSON (List a) where
  toJSON (List ls) = toJSON ls

instance (FromJSON a) => FromJSON (List a) where
  parseJSON Null = return (List [])
  parseJSON v      = List <$> parseJSON v

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup (List a) where
  (<>) = mappend
#endif
