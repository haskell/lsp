-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InsertTextMode where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
How whitespace and indentation is handled during completion
item insertion.

@since 3.16.0
-}
data InsertTextMode = 
    {-|
  The insertion or replace strings is taken as it is. If the
  value is multi line the lines below the cursor will be
  inserted using the indentation defined in the string value.
  The client will not apply any kind of adjustments to the
  string.
  -}
  InsertTextMode_AsIs
  | {-|
  The editor adjusts leading whitespace of new lines so that
  they match the indentation up to the cursor of the line for
  which the item is accepted.

  Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
  multi line completion item is indented using 2 tabs and all
  following lines inserted will be indented using 2 tabs as well.
  -}
  InsertTextMode_AdjustIndentation
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum InsertTextMode Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum InsertTextMode where
  knownValues = Data.Set.fromList [InsertTextMode_AsIs
    ,InsertTextMode_AdjustIndentation]
  type EnumBaseType InsertTextMode = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType InsertTextMode_AsIs = 1
  toEnumBaseType InsertTextMode_AdjustIndentation = 2
  fromEnumBaseType 1 = pure InsertTextMode_AsIs
  fromEnumBaseType 2 = pure InsertTextMode_AdjustIndentation
  fromEnumBaseType _ = Nothing


