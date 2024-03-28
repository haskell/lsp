{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RegularExpressionsClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.RegularExpressionEngineKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities specific to regular expressions.

@since 3.16.0
-}
data RegularExpressionsClientCapabilities = RegularExpressionsClientCapabilities 
  { {-|
  The engine's name.
  -}
  engine :: Language.LSP.Protocol.Internal.Types.RegularExpressionEngineKind.RegularExpressionEngineKind
  , {-|
  The engine's version.
  -}
  version :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RegularExpressionsClientCapabilities)

instance Aeson.ToJSON RegularExpressionsClientCapabilities where
  toJSON (RegularExpressionsClientCapabilities arg0 arg1) = Aeson.object $ concat $  [["engine" Aeson..= arg0]
    ,"version" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON RegularExpressionsClientCapabilities where
  parseJSON = Aeson.withObject "RegularExpressionsClientCapabilities" $ \arg -> RegularExpressionsClientCapabilities <$> arg Aeson..: "engine" <*> arg Language.LSP.Protocol.Types.Common..:!? "version"
