-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WatchKind where

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
data WatchKind = 
    {-|
  Interested in create events.
  -}
  WatchKind_Create
  | {-|
  Interested in change events
  -}
  WatchKind_Change
  | {-|
  Interested in delete events
  -}
  WatchKind_Delete
  | WatchKind_Custom Language.LSP.Protocol.Types.Common.UInt
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum WatchKind Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON WatchKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum WatchKind where
  knownValues = Data.Set.fromList [WatchKind_Create
    ,WatchKind_Change
    ,WatchKind_Delete]
  type EnumBaseType WatchKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType WatchKind_Create = 1
  toEnumBaseType WatchKind_Change = 2
  toEnumBaseType WatchKind_Delete = 4
  toEnumBaseType (WatchKind_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum WatchKind where
  fromOpenEnumBaseType 1 = WatchKind_Create
  fromOpenEnumBaseType 2 = WatchKind_Change
  fromOpenEnumBaseType 4 = WatchKind_Delete
  fromOpenEnumBaseType arg = WatchKind_Custom arg


