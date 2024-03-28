{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ShowDocumentResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
The result of a showDocument request.

@since 3.16.0
-}
data ShowDocumentResult = ShowDocumentResult 
  { {-|
  A boolean indicating if the show was successful.
  -}
  success :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ShowDocumentResult)

instance Aeson.ToJSON ShowDocumentResult where
  toJSON (ShowDocumentResult arg0) = Aeson.object $ concat $  [["success" Aeson..= arg0]]

instance Aeson.FromJSON ShowDocumentResult where
  parseJSON = Aeson.withObject "ShowDocumentResult" $ \arg -> ShowDocumentResult <$> arg Aeson..: "success"
