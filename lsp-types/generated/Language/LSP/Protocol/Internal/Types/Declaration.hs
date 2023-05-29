-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Declaration where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Types.Common

{-|
The declaration of a symbol representation as one or many `Location`.
-}
newtype Declaration = Declaration (Language.LSP.Protocol.Internal.Types.Location.Location Language.LSP.Protocol.Types.Common.|? [Language.LSP.Protocol.Internal.Types.Location.Location])
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)