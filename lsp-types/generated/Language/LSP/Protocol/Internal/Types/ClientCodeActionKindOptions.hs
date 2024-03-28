{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCodeActionKindOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientCodeActionKindOptions = ClientCodeActionKindOptions 
  { {-|
  The code action kind values the client supports. When this
  property exists the client also guarantees that it will
  handle values outside its set gracefully and falls back
  to a default value when unknown.
  -}
  valueSet :: [Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCodeActionKindOptions)

instance Aeson.ToJSON ClientCodeActionKindOptions where
  toJSON (ClientCodeActionKindOptions arg0) = Aeson.object $ concat $  [["valueSet" Aeson..= arg0]]

instance Aeson.FromJSON ClientCodeActionKindOptions where
  parseJSON = Aeson.withObject "ClientCodeActionKindOptions" $ \arg -> ClientCodeActionKindOptions <$> arg Aeson..: "valueSet"
