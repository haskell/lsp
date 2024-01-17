{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentHighlightKind where

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
A document highlight kind.
-}
data DocumentHighlightKind = 
    {-|
  A textual occurrence.
  -}
  DocumentHighlightKind_Text
  | {-|
  Read-access of a symbol, like reading a variable.
  -}
  DocumentHighlightKind_Read
  | {-|
  Write-access of a symbol, like writing to a variable.
  -}
  DocumentHighlightKind_Write
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum DocumentHighlightKind)
  deriving Pretty via (ViaJSON DocumentHighlightKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum DocumentHighlightKind where
  knownValues = Data.Set.fromList [DocumentHighlightKind_Text
    ,DocumentHighlightKind_Read
    ,DocumentHighlightKind_Write]
  type EnumBaseType DocumentHighlightKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType DocumentHighlightKind_Text = 1
  toEnumBaseType DocumentHighlightKind_Read = 2
  toEnumBaseType DocumentHighlightKind_Write = 3
  fromEnumBaseType 1 = pure DocumentHighlightKind_Text
  fromEnumBaseType 2 = pure DocumentHighlightKind_Read
  fromEnumBaseType 3 = pure DocumentHighlightKind_Write
  fromEnumBaseType _ = Nothing


