{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Method where

import Data.Aeson.Types
import Data.Function (on)
import Data.GADT.Compare
import Data.List (isPrefixOf)
import Data.Proxy
import Data.Singletons
import Data.Type.Equality
import GHC.Exts (Int (..), dataToTag#)
import GHC.TypeLits (
  KnownSymbol,
  sameSymbol,
  symbolVal,
 )
import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import Unsafe.Coerce

instance Pretty Method where
  pretty m = pretty (methodToMethodString m)

-- | Is this an "optional" method which servers and clients are allowed to ignore?
isOptionalMethod :: Method -> Bool
-- See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests
isOptionalMethod m = "$/" `isPrefixOf` methodToMethodString m

-- This instance is written manually rather than derived to avoid a dependency
-- on 'dependent-sum-template'.
instance GEq SMethod where
  geq x y = case gcompare x y of
    GLT -> Nothing
    GEQ -> Just Refl
    GGT -> Nothing

-- This instance is written manually rather than derived to avoid a dependency
-- on 'dependent-sum-template'.
instance GCompare SMethod where
  -- This is much more compact than matching on every pair of constructors, which
  -- is what we would need to do for GHC to 'see' that this is correct. Nonetheless
  -- it is safe: since there is only one constructor of 'SMethod' for each 'Method',
  -- the argument types can only be equal if the constructor tag is equal.
  gcompare x y = case I# (dataToTag# x) `compare` I# (dataToTag# y) of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT

instance Eq (SMethod m) where
  -- This defers to 'GEq', ensuring that this version is compatible.
  (==) = defaultEq

instance Ord (SMethod m) where
  -- This defers to 'GCompare', ensuring that this version is compatible.
  compare = defaultCompare

deriving stock instance Show (SMethod m)

instance Pretty (SMethod m) where
  pretty sm = pretty (fromSing sm)

-- instance ToJSON (SMethod m) where
-- toJSON m = toJSON (SomeMethod m)

-- TODO: generate these with everything else?
-- Generates lots of instances like this in terms of the FromJSON SomeMethod instance
-- instance FromJSON (SMethod Method_X)
-- makeSingletonFromJSON 'SomeMethod ''SMethod ['SMethod_CustomMethod]
