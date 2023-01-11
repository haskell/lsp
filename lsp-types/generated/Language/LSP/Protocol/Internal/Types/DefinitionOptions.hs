-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DefinitionOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Server Capabilities for a `DefinitionRequest`.

-}
data DefinitionOptions = DefinitionOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DefinitionOptions where
  toJSON (DefinitionOptions arg0) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DefinitionOptions where
  parseJSON = Aeson.withObject "DefinitionOptions" $ \arg -> DefinitionOptions <$> arg Aeson..:! "workDoneProgress"