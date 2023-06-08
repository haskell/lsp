-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Definition where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Types.Common

{-|
The definition of a symbol represented as one or many `Location`.
For most programming languages there is only one location at which a symbol is
defined.

Servers should prefer returning `DefinitionLink` over `Definition` if supported
by the client.
-}
newtype Definition = Definition (Language.LSP.Protocol.Internal.Types.Location.Location Language.LSP.Protocol.Types.Common.|? [Language.LSP.Protocol.Internal.Types.Location.Location])
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)