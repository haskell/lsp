-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SymbolTag where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Symbol tags are extra annotations that tweak the rendering of a symbol.

@since 3.16

-}
data SymbolTag = 
    {-|
  Render a symbol as obsolete, usually using a strike-out.

  -}
  SymbolTag_Deprecated
  deriving stock (Show, Eq, Ord, Generic)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum SymbolTag Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum SymbolTag where
  knownValues = Data.Set.fromList [SymbolTag_Deprecated]
  type EnumBaseType SymbolTag = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType SymbolTag_Deprecated = 1
  fromEnumBaseType 1 = pure SymbolTag_Deprecated
  fromEnumBaseType _ = Nothing

