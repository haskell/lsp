{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCodeActionLiteralOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientCodeActionKindOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientCodeActionLiteralOptions = ClientCodeActionLiteralOptions 
  { {-|
  The code action kind is support with the following value
  set.
  -}
  _codeActionKind :: Language.LSP.Protocol.Internal.Types.ClientCodeActionKindOptions.ClientCodeActionKindOptions
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCodeActionLiteralOptions)

instance Aeson.ToJSON ClientCodeActionLiteralOptions where
  toJSON (ClientCodeActionLiteralOptions arg0) = Aeson.object $ concat $  [["codeActionKind" Aeson..= arg0]]

instance Aeson.FromJSON ClientCodeActionLiteralOptions where
  parseJSON = Aeson.withObject "ClientCodeActionLiteralOptions" $ \arg -> ClientCodeActionLiteralOptions <$> arg Aeson..: "codeActionKind"
