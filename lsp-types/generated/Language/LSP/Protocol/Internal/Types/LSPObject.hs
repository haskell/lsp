-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LSPObject where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Map
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text

{-|
LSP object definition.
@since 3.17.0

-}
newtype LSPObject = LSPObject (Data.Map.Map Data.Text.Text Data.Aeson.Value)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)