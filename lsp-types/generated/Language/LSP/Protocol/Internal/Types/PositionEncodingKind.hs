-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PositionEncodingKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A set of predefined position encoding kinds.

@since 3.17.0
-}
data PositionEncodingKind = 
    {-|
  Character offsets count UTF-8 code units.
  -}
  PositionEncodingKind_UTF8
  | {-|
  Character offsets count UTF-16 code units.

  This is the default and must always be supported
  by servers
  -}
  PositionEncodingKind_UTF16
  | {-|
  Character offsets count UTF-32 code units.

  Implementation note: these are the same as Unicode code points,
  so this `PositionEncodingKind` may also be used for an
  encoding-agnostic representation of character offsets.
  -}
  PositionEncodingKind_UTF32
  | PositionEncodingKind_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum PositionEncodingKind Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum PositionEncodingKind where
  knownValues = Data.Set.fromList [PositionEncodingKind_UTF8
    ,PositionEncodingKind_UTF16
    ,PositionEncodingKind_UTF32]
  type EnumBaseType PositionEncodingKind = Data.Text.Text
  toEnumBaseType PositionEncodingKind_UTF8 = "utf-8"
  toEnumBaseType PositionEncodingKind_UTF16 = "utf-16"
  toEnumBaseType PositionEncodingKind_UTF32 = "utf-32"
  toEnumBaseType (PositionEncodingKind_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum PositionEncodingKind where
  fromOpenEnumBaseType "utf-8" = PositionEncodingKind_UTF8
  fromOpenEnumBaseType "utf-16" = PositionEncodingKind_UTF16
  fromOpenEnumBaseType "utf-32" = PositionEncodingKind_UTF32
  fromOpenEnumBaseType arg = PositionEncodingKind_Custom arg


