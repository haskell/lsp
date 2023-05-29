-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookCellKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A notebook cell kind.

@since 3.17.0

-}
data NotebookCellKind = 
    {-|
  A markup-cell is formatted source that is used for display.

  -}
  NotebookCellKind_Markup
  | {-|
  A code-cell is source code.

  -}
  NotebookCellKind_Code
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum NotebookCellKind Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum NotebookCellKind where
  knownValues = Data.Set.fromList [NotebookCellKind_Markup
    ,NotebookCellKind_Code]
  type EnumBaseType NotebookCellKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType NotebookCellKind_Markup = 1
  toEnumBaseType NotebookCellKind_Code = 2
  fromEnumBaseType 1 = pure NotebookCellKind_Markup
  fromEnumBaseType 2 = pure NotebookCellKind_Code
  fromEnumBaseType _ = Nothing

