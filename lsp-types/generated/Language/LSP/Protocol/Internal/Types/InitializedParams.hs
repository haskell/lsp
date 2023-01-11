-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InitializedParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data InitializedParams = InitializedParams 
  { 
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON InitializedParams where
  toJSON (InitializedParams ) = Aeson.object $ concat $  []

instance Aeson.FromJSON InitializedParams where
  parseJSON = Aeson.withObject "InitializedParams" $ \arg -> pure InitializedParams