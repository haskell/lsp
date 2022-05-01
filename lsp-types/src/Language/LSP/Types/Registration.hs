{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -dppr-cols=200 -dumpdir /tmp/dumps #-}

module Language.LSP.Types.Registration where

import           Language.LSP.Types.Common
import           Language.LSP.Types.Internal.Generated
import           Language.LSP.Types.Internal.Lenses
import           Language.LSP.Types.Method
import           Language.LSP.Types.Utils

import           Control.Lens.TH
import           Data.Aeson
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           GHC.Generics

-- | Typed registration type, with correct options.
data TRegistration (m :: Method ClientToServer t) =
  TRegistration
    { -- | The id used to register the request. The id can be used to deregister
      -- the request again.
      _id              :: Text
      -- | The method / capability to register for.
    , _method          :: SClientMethod m
      -- | Options necessary for the registration.
      -- Make this strict to aid the pattern matching exhaustiveness checker
    , _registerOptions :: !(Maybe (RegistrationOptions m))
    }
  deriving Generic

deriving instance Eq (RegistrationOptions m) => Eq (TRegistration m)
deriving instance Show (RegistrationOptions m) => Show (TRegistration m)

-- TODO: can we do this generically somehow?
-- This generates the function
-- regHelper :: SMethod m
--           -> (( Show (RegistrationOptions m)
--               , ToJSON (RegistrationOptions m)
--               , FromJSON ($regOptTcon m)
--              => x)
--           -> x
makeRegHelper ''RegistrationOptions

instance ToJSON (TRegistration m) where
  toJSON x@(TRegistration _ m _) = regHelper m (genericToJSON lspOptions x)

data SomeRegistration = forall t (m :: Method ClientToServer t). SomeRegistration (TRegistration m)

instance ToJSON SomeRegistration where
  toJSON (SomeRegistration r) = toJSON r

instance FromJSON SomeRegistration where
  parseJSON = withObject "Registration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TRegistration <$> o .: "id" <*> pure m <*> regHelper m (o .: "registerOptions")
    pure (SomeRegistration r)

instance Show SomeRegistration where
  show (SomeRegistration r@(TRegistration _ m _)) = regHelper m (show r)

toUntypedRegistration :: TRegistration m -> Registration
toUntypedRegistration (TRegistration i meth opts) = Registration i (T.pack $ someMethodToMethodString $ SomeMethod meth) (Just $ regHelper meth (toJSON opts))

toSomeRegistration :: Registration -> Maybe SomeRegistration
toSomeRegistration r =
  let v = toJSON r
  in case fromJSON v of
    Success r' -> Just r'
    _          -> Nothing

-- ---------------------------------------------------------------------

-- | Typed unregistration type.
data TUnregistration (m :: Method ClientToServer t) =
  TUnregistration
    { -- | The id used to unregister the request or notification. Usually an id
      -- provided during the register request.
      _id     :: Text
      -- | The method / capability to unregister for.
    , _method :: SMethod m
    } deriving Generic

deriving instance Eq (TUnregistration m)
deriving instance Show (TUnregistration m)

instance ToJSON (TUnregistration m) where
  toJSON x@(TUnregistration _ m) = regHelper m (genericToJSON lspOptions x)

data SomeUnregistration = forall t (m :: Method ClientToServer t). SomeUnregistration (TUnregistration m)

instance ToJSON SomeUnregistration where
  toJSON (SomeUnregistration r) = toJSON r

instance FromJSON SomeUnregistration where
  parseJSON = withObject "Unregistration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TUnregistration <$> o .: "id" <*> pure m
    pure (SomeUnregistration r)

toUntypedUnregistration :: TUnregistration m -> Unregistration
toUntypedUnregistration (TUnregistration i meth) = Unregistration i (T.pack $ someMethodToMethodString $ SomeMethod meth)

toSomeUnregistration :: Unregistration -> Maybe SomeUnregistration
toSomeUnregistration r =
  let v = toJSON r
  in case fromJSON v of
    Success r' -> Just r'
    _          -> Nothing

makeFieldsNoPrefix ''TRegistration
makeFieldsNoPrefix ''TUnregistration
