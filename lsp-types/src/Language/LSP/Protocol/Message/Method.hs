{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Method where

import Data.Aeson.Types
import Data.Function (on)
import Data.GADT.Compare
import Data.List (isPrefixOf)
import Data.Proxy
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

---------------
-- SomeMethod
---------------

-- | Is this an "optional" method which servers and clients are allowed to ignore?
isOptionalMethod :: SomeMethod -> Bool
-- See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests
isOptionalMethod m = "$/" `isPrefixOf` someMethodToMethodString m

deriving stock instance Show SomeMethod
instance Eq SomeMethod where
  (==) = (==) `on` someMethodToMethodString
instance Ord SomeMethod where
  compare = compare `on` someMethodToMethodString

instance ToJSON SomeMethod where
  toJSON sm = toJSON $ someMethodToMethodString sm

instance FromJSON SomeMethod where
  parseJSON v = do
    s <- parseJSON v
    pure $ methodStringToSomeMethod s

deriving via ViaJSON SomeMethod instance Pretty SomeMethod

---------------
-- SMethod
---------------

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
  gcompare (SMethod_CustomMethod x) (SMethod_CustomMethod y) = case symbolVal x `compare` symbolVal y of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT
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

instance ToJSON (SMethod m) where
  toJSON m = toJSON (SomeMethod m)

instance KnownSymbol s => FromJSON (SMethod ('Method_CustomMethod s :: Method f t)) where
  parseJSON v = do
    sm <- parseJSON v
    case sm of
      SomeMethod (SMethod_CustomMethod x) -> case sameSymbol x (Proxy :: Proxy s) of
        Just Refl -> pure $ SMethod_CustomMethod x
        Nothing -> mempty
      _ -> mempty

-- TODO: generate these with everything else?
-- Generates lots of instances like this in terms of the FromJSON SomeMethod instance
-- instance FromJSON (SMethod Method_X)
makeSingletonFromJSON 'SomeMethod ''SMethod ['SMethod_CustomMethod]

deriving via ViaJSON (SMethod m) instance Pretty (SMethod m)

---------------
-- Extras
---------------

-- Some useful type synonyms
type SClientMethod (m :: Method ClientToServer t) = SMethod m
type SServerMethod (m :: Method ServerToClient t) = SMethod m

data SomeClientMethod = forall t (m :: Method ClientToServer t). SomeClientMethod (SMethod m)
deriving stock instance Show SomeClientMethod

data SomeServerMethod = forall t (m :: Method ServerToClient t). SomeServerMethod (SMethod m)
deriving stock instance Show SomeServerMethod

someClientMethod :: SMethod m -> Maybe SomeClientMethod
someClientMethod s = case messageDirection s of
  SClientToServer -> Just $ SomeClientMethod s
  SServerToClient -> Nothing
  -- See Note [Parsing methods that go both ways]
  SBothDirections -> Just $ SomeClientMethod $ unsafeCoerce s

someServerMethod :: SMethod m -> Maybe SomeServerMethod
someServerMethod s = case messageDirection s of
  SServerToClient -> Just $ SomeServerMethod s
  SClientToServer -> Nothing
  -- See Note [Parsing methods that go both ways]
  SBothDirections -> Just $ SomeServerMethod $ unsafeCoerce s

instance FromJSON SomeClientMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someClientMethod sm of
      Just scm -> pure scm
      Nothing -> mempty

instance ToJSON SomeClientMethod where
  toJSON (SomeClientMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

deriving via ViaJSON SomeClientMethod instance Pretty SomeClientMethod

instance FromJSON SomeServerMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someServerMethod sm of
      Just scm -> pure scm
      Nothing -> mempty

instance ToJSON SomeServerMethod where
  toJSON (SomeServerMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

deriving via ViaJSON SomeServerMethod instance Pretty SomeServerMethod

{- Note [Parsing methods that go both ways]

In order to parse a method as a client or server method, we use 'messageDirection'
to get a proof that the message direction is what we say it is. But this just doesn't
work for the both directions case: because we are awkwardly representing "both directions"
as "the type variable is free". So the types don't line up and we use an awful hack.

A better solution might be to move away from using an unconstrained type parameter to
mean "both directions".
-}
