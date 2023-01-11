-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InsertTextFormat where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Defines whether the insert text in a completion item should be interpreted as
plain text or a snippet.

-}
data InsertTextFormat = 
    {-|
  The primary text to be inserted is treated as a plain string.

  -}
  InsertTextFormat_PlainText
  | {-|
  The primary text to be inserted is treated as a snippet.

  A snippet can define tab stops and placeholders with `$1`, `$2`
  and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  the end of the snippet. Placeholders with equal identifiers are linked,
  that is typing in one will update others too.

  See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax

  -}
  InsertTextFormat_Snippet
  deriving stock (Show, Eq, Ord, Generic)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum InsertTextFormat Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum InsertTextFormat where
  knownValues = Data.Set.fromList [InsertTextFormat_PlainText
    ,InsertTextFormat_Snippet]
  type EnumBaseType InsertTextFormat = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType InsertTextFormat_PlainText = 1
  toEnumBaseType InsertTextFormat_Snippet = 2
  fromEnumBaseType 1 = pure InsertTextFormat_PlainText
  fromEnumBaseType 2 = pure InsertTextFormat_Snippet
  fromEnumBaseType _ = Nothing

