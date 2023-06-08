-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItemTag where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Completion item tags are extra annotations that tweak the rendering of a completion
item.

@since 3.15.0
-}
data CompletionItemTag = 
    {-|
  Render a completion as obsolete, usually using a strike-out.
  -}
  CompletionItemTag_Deprecated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CompletionItemTag Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CompletionItemTag where
  knownValues = Data.Set.fromList [CompletionItemTag_Deprecated]
  type EnumBaseType CompletionItemTag = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType CompletionItemTag_Deprecated = 1
  fromEnumBaseType 1 = pure CompletionItemTag_Deprecated
  fromEnumBaseType _ = Nothing


