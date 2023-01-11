-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ProgressToken where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
newtype ProgressToken = ProgressToken (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Data.Text.Text)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)