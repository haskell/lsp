{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- See Note [Arbitary Maybe Void]
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Language.LSP.Protocol.QuickCheck.Message where

import Data.Row qualified as R
import Data.Row.Records qualified as R
import Data.Void
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.QuickCheck.Types
import Language.LSP.Protocol.Types
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

deriving via (GenericArbitrary RequestMessage) instance Arbitrary RequestMessage
deriving via (GenericArbitrary ResponseMessage) instance Arbitrary ResponseMessage
deriving via (GenericArbitrary NotificationMessage) instance Arbitrary NotificationMessage

-- See Note [Arbitary Maybe Void]
instance {-# OVERLAPS #-} Arbitrary (Maybe Void) where
  arbitrary = pure Nothing

-- FIXME: there is something funny with the way we're representing error data.
-- Should be able to have these instances!
{-
deriving via (GenericArbitrary ResponseError) instance Arbitrary ResponseError

deriving via
  (GenericArbitrary (TResponseError m))
  instance
    (Arbitrary (ErrorData m)) => Arbitrary (TResponseError m)

deriving via
  (GenericArbitrary (TResponseMEssage m))
  instance
    (Arbitrary (MessageResult m), Arbitrary (ErrorData m)) => Arbitrary (TResponseMEssage m)
-}

instance (ErrorData m ~ Maybe Void) => Arbitrary (TResponseError m) where
  arbitrary = TResponseError <$> arbitrary <*> arbitrary <*> pure Nothing
  shrink = genericShrink

instance Arbitrary ResponseError where
  arbitrary = ResponseError <$> arbitrary <*> arbitrary <*> pure Nothing
  shrink = genericShrink

-- See Note [Arbitary Maybe Void]
deriving via
  (GenericArbitrary (TResponseMessage m))
  instance
    (Arbitrary (MessageResult m), ErrorData m ~ Maybe Void) => Arbitrary (TResponseMessage m)

-- These require a method singleton. We need something like SingI from
-- singletons to pass that into the class instance
{-
instance (Arbitrary (MessageParams m)) => Arbitrary (TRequestMessage m) where
  arbitrary = TRequestMessage <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Arbitrary (MessageParams m)) => Arbitrary (TNotificationMessage m) where
  arbitrary = TNotificationMessage <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink
-}

{- Note [Arbitary Maybe Void]
For methods that don't have error data, we say that their error data type is 'Void'.
This means that the error data field has type 'Maybe Void', i.e. can only be 'Nothing',
which is what we want.

However, we have a problem with the Arbitrary instance. There is an 'Arbitrary (Maybe a)'
instance which depends on an 'Arbitrary a' instance - but there is no 'Arbitrary Void'
instance, and apparently can't be since we can't make an empty generator.

So we cheat a bit:
- Define an overlapping 'Arbitrary (Maybe Void)' instance
- Define the instances for response messages to require 'Arbitrary (Maybe (ErrorData m))',
  which delays GHC picking the actual instance until the use site, so it can pick
  the overlapping one.
-}
