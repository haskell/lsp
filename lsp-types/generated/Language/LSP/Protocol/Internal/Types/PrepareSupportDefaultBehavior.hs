-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PrepareSupportDefaultBehavior where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|

-}
data PrepareSupportDefaultBehavior = 
    {-|
  The client's default behavior is to select the identifier
  according the to language's syntax rule.
  -}
  PrepareSupportDefaultBehavior_Identifier
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum PrepareSupportDefaultBehavior Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON PrepareSupportDefaultBehavior)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum PrepareSupportDefaultBehavior where
  knownValues = Data.Set.fromList [PrepareSupportDefaultBehavior_Identifier]
  type EnumBaseType PrepareSupportDefaultBehavior = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType PrepareSupportDefaultBehavior_Identifier = 1
  fromEnumBaseType 1 = pure PrepareSupportDefaultBehavior_Identifier
  fromEnumBaseType _ = Nothing


