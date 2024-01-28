{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PrepareRenameResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.PrepareRenameDefaultBehavior
import qualified Language.LSP.Protocol.Internal.Types.PrepareRenamePlaceholder
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
newtype PrepareRenameResult = PrepareRenameResult (Language.LSP.Protocol.Internal.Types.Range.Range Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.PrepareRenamePlaceholder.PrepareRenamePlaceholder Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.PrepareRenameDefaultBehavior.PrepareRenameDefaultBehavior))
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON PrepareRenameResult)
