{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Row.Hashable where

import Data.Hashable

import Data.Row
import Data.Row.Records qualified as Rec

instance (Forall r Hashable, Forall r Eq) => Hashable (Rec r) where
  hashWithSalt s record = hashWithSalt s (Rec.erase @Hashable (hashWithSalt s) record)
