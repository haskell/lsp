-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PrepareRenameResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
newtype PrepareRenameResult = PrepareRenameResult (Language.LSP.Protocol.Internal.Types.Range.Range Language.LSP.Protocol.Types.Common.|? ((Row.Rec ("range" Row..== Language.LSP.Protocol.Internal.Types.Range.Range Row..+ ("placeholder" Row..== Data.Text.Text Row..+ Row.Empty))) Language.LSP.Protocol.Types.Common.|? (Row.Rec ("defaultBehavior" Row..== Bool Row..+ Row.Empty))))
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
