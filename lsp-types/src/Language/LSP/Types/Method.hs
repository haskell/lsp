{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}
{-# LANGUAGE FlexibleContexts         #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Language.LSP.Types.Method where

import           Data.Aeson.Types
import           Data.Function                         (on)
import           Data.GADT.Compare
import           Data.Proxy
import           Data.Type.Equality
import           GHC.Exts                              (Int (..), dataToTag#)
import           GHC.TypeLits                          (KnownSymbol, sameSymbol,
                                                        symbolVal)
import           Language.LSP.Types.Common
import           Language.LSP.Types.Internal.Generated
import           Language.LSP.Types.Utils
import           Unsafe.Coerce

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

deriving instance Show (SMethod m)

instance ToJSON (SMethod m) where
  toJSON m = toJSON (SomeMethod m)

------

deriving instance Show SomeMethod
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

-------

-- Some useful type synonyms
type SClientMethod (m :: Method ClientToServer t) = SMethod m
type SServerMethod (m :: Method ServerToClient t) = SMethod m

data SomeClientMethod = forall t (m :: Method ClientToServer t). SomeClientMethod (SMethod m)
deriving instance Show SomeClientMethod

data SomeServerMethod = forall t (m :: Method ServerToClient t). SomeServerMethod (SMethod m)
deriving instance Show SomeServerMethod

someClientMethod :: SMethod m -> Maybe SomeClientMethod
someClientMethod s = case messageDirection s of
    SClientToServer -> Just $ SomeClientMethod s
    SServerToClient -> Nothing
    SBothDirections -> Nothing

someServerMethod :: SMethod m -> Maybe SomeServerMethod
someServerMethod s = case messageDirection s of
    SServerToClient-> Just $ SomeServerMethod s
    SClientToServer -> Nothing
    SBothDirections -> Nothing

instance FromJSON SomeClientMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someClientMethod sm of
      Just scm -> pure scm
      Nothing  -> mempty

instance ToJSON SomeClientMethod where
  toJSON (SomeClientMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

instance FromJSON SomeServerMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someServerMethod sm of
      Just scm -> pure scm
      Nothing  -> mempty

instance ToJSON SomeServerMethod where
  toJSON (SomeServerMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

instance KnownSymbol s => FromJSON (SMethod ('Method_CustomMethod s :: Method f t)) where
  parseJSON v = do
    sm <- parseJSON v
    case sm of
      SomeMethod (SMethod_CustomMethod x) -> case sameSymbol x (Proxy :: Proxy s) of
        Just Refl -> pure $ SMethod_CustomMethod x
        Nothing   -> mempty
      _ -> mempty

-- TODO: generate these with everything else?
-- Generates lots of instances like this in terms of the FromJSON SomeMethod instance
-- instance FromJSON (SMethod Method_X)
makeSingletonFromJSON 'SomeMethod ''SMethod ['SMethod_CustomMethod]
