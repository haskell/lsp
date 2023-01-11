-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.UnregistrationParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Unregistration
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data UnregistrationParams = UnregistrationParams 
  { {-|

  -}
  _unregisterations :: [Language.LSP.Protocol.Internal.Types.Unregistration.Unregistration]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON UnregistrationParams where
  toJSON (UnregistrationParams arg0) = Aeson.object $ concat $  [["unregisterations" Aeson..= arg0]]

instance Aeson.FromJSON UnregistrationParams where
  parseJSON = Aeson.withObject "UnregistrationParams" $ \arg -> UnregistrationParams <$> arg Aeson..: "unregisterations"