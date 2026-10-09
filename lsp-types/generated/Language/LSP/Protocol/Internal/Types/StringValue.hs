{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.StringValue where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons

{-|
A string value used as a snippet is a template which allows to insert text
and to control the editor cursor when insertion happens.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Variables are defined with `$name` and
`${name:default value}`.

@since 3.18.0
-}
data StringValue = StringValue 
  { {-|
  The kind of string value.
  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "snippet")
  , {-|
  The snippet string.
  -}
  _value :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON StringValue)

instance Aeson.ToJSON StringValue where
  toJSON (StringValue arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["value" Aeson..= arg1]]

instance Aeson.FromJSON StringValue where
  parseJSON = Aeson.withObject "StringValue" $ \arg -> StringValue <$> arg Aeson..: "kind" <*> arg Aeson..: "value"
