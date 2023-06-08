-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LSPAny where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
The LSP any type.
Please note that strictly speaking a property with the value `undefined`
can't be converted into JSON preserving the property name. However for
convenience it is allowed and assumed that all these properties are
optional as well.
@since 3.17.0
-}
newtype LSPAny = LSPAny (Data.Aeson.Object Language.LSP.Protocol.Types.Common.|? (Data.Aeson.Array Language.LSP.Protocol.Types.Common.|? (Data.Text.Text Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Types.Common.UInt Language.LSP.Protocol.Types.Common.|? (Float Language.LSP.Protocol.Types.Common.|? (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)))))))
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
