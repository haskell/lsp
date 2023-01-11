-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValue where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.InlineValueEvaluatableExpression
import qualified Language.LSP.Protocol.Internal.Types.InlineValueText
import qualified Language.LSP.Protocol.Internal.Types.InlineValueVariableLookup
import qualified Language.LSP.Protocol.Types.Common

{-|
Inline value information can be provided by different means:
- directly as a text value (class InlineValueText).
- as a name to use for a variable lookup (class InlineValueVariableLookup)
- as an evaluatable expression (class InlineValueEvaluatableExpression)
The InlineValue types combines all inline value types into one type.

@since 3.17.0

-}
newtype InlineValue = InlineValue (Language.LSP.Protocol.Internal.Types.InlineValueText.InlineValueText Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.InlineValueVariableLookup.InlineValueVariableLookup Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.InlineValueEvaluatableExpression.InlineValueEvaluatableExpression))
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)