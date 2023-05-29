-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.GlobPattern where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Pattern
import qualified Language.LSP.Protocol.Internal.Types.RelativePattern
import qualified Language.LSP.Protocol.Types.Common

{-|
The glob pattern. Either a string pattern or a relative pattern.

@since 3.17.0

-}
newtype GlobPattern = GlobPattern (Language.LSP.Protocol.Internal.Types.Pattern.Pattern Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.RelativePattern.RelativePattern)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)