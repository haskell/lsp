{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.LSP.Protocol.QuickCheck.Common where

import Data.Foldable
import Data.Row qualified as R
import Data.Row.Records qualified as R
import Data.Void
import GHC.TypeLits
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance (Arbitrary a, Arbitrary b) => Arbitrary (a |? b) where
  arbitrary = oneof [InL <$> arbitrary, InR <$> arbitrary]
  shrink = genericShrink

instance Arbitrary Null where
  arbitrary = pure Null

instance (R.AllUniqueLabels r, R.Forall r Arbitrary) => Arbitrary (R.Rec r) where
  arbitrary = R.fromLabelsA @Arbitrary $ \_l -> arbitrary
  shrink record = R.traverse @Arbitrary @[] shrink record

instance Arbitrary UInt where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Uri where
  arbitrary = Uri <$> arbitrary
  shrink = genericShrink

instance Arbitrary (LspId m) where
  arbitrary = oneof [IdInt <$> arbitrary, IdString <$> arbitrary]
  shrink = genericShrink

instance (LspEnum a) => Arbitrary (AsLspEnum a) where
  arbitrary = elements $ AsLspEnum <$> toList knownValues

instance (KnownSymbol s) => Arbitrary (AString s) where
  arbitrary = pure $ AString

instance (KnownNat n) => Arbitrary (AnInteger n) where
  arbitrary = pure $ AnInteger
