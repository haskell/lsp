{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionDisabled where

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

{-|
Captures why the code action is currently disabled.

@since 3.18.0
@proposed
-}
data CodeActionDisabled = CodeActionDisabled 
  { {-|
  Human readable description of why the code action is currently disabled.

  This is displayed in the code actions UI.
  -}
  _reason :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeActionDisabled)

instance Aeson.ToJSON CodeActionDisabled where
  toJSON (CodeActionDisabled arg0) = Aeson.object $ concat $  [["reason" Aeson..= arg0]]

instance Aeson.FromJSON CodeActionDisabled where
  parseJSON = Aeson.withObject "CodeActionDisabled" $ \arg -> CodeActionDisabled <$> arg Aeson..: "reason"
