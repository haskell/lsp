-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Registration where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
General parameters to to register for an notification or to register a provider.

-}
data Registration = Registration 
  { {-|
  The id used to register the request. The id can be used to deregister
  the request again.

  -}
  _id :: Data.Text.Text
  , {-|
  The method / capability to register for.

  -}
  _method :: Data.Text.Text
  , {-|
  Options necessary for the registration.

  -}
  _registerOptions :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Registration where
  toJSON (Registration arg0 arg1 arg2) = Aeson.object $ concat $  [["id" Aeson..= arg0]
    ,["method" Aeson..= arg1]
    ,"registerOptions" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON Registration where
  parseJSON = Aeson.withObject "Registration" $ \arg -> Registration <$> arg Aeson..: "id" <*> arg Aeson..: "method" <*> arg Aeson..:! "registerOptions"