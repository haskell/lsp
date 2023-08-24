{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentSyncKind where

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
Defines how the host (editor) should sync
document changes to the language server.
-}
data TextDocumentSyncKind = 
    {-|
  Documents should not be synced at all.
  -}
  TextDocumentSyncKind_None
  | {-|
  Documents are synced by always sending the full content
  of the document.
  -}
  TextDocumentSyncKind_Full
  | {-|
  Documents are synced by sending the full content on open.
  After that only incremental updates to the document are
  send.
  -}
  TextDocumentSyncKind_Incremental
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum TextDocumentSyncKind Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON TextDocumentSyncKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum TextDocumentSyncKind where
  knownValues = Data.Set.fromList [TextDocumentSyncKind_None
    ,TextDocumentSyncKind_Full
    ,TextDocumentSyncKind_Incremental]
  type EnumBaseType TextDocumentSyncKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType TextDocumentSyncKind_None = 0
  toEnumBaseType TextDocumentSyncKind_Full = 1
  toEnumBaseType TextDocumentSyncKind_Incremental = 2
  fromEnumBaseType 0 = pure TextDocumentSyncKind_None
  fromEnumBaseType 1 = pure TextDocumentSyncKind_Full
  fromEnumBaseType 2 = pure TextDocumentSyncKind_Incremental
  fromEnumBaseType _ = Nothing


